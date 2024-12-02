mod command;
mod parse;
mod source;

use crate::output::{Condition, Output};
use crate::{dprintln, output};
use crate::{runtime::RunState, symbol::with_symbol_table};
use command::{Command, Label, Location, MemoryLocation};
use source::{SourceMode, SourceReader};

// TODO(refactor): Perhaps there is `clap` trait that can be implemented for
// this struct, to avoid field duplication in `Command` enum
#[derive(Debug)]
pub struct DebuggerOptions {
    pub minimal: bool,
    pub command: Option<String>,
}

#[allow(dead_code)]
pub struct Debugger {
    status: Status,
    source: SourceMode,

    initial_state: RunState,

    breakpoints: Vec<u16>,
    current_breakpoint: Option<u16>,
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub enum Status {
    #[default]
    WaitForAction,
    Step {
        count: u16,
    },
    Next {
        return_addr: u16,
    },
    Continue,
    Finish,
}

#[derive(Debug)]
pub enum Action {
    Proceed,
    StopDebugger,
    ExitProgram,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RelevantInstr {
    /// Return from a subroutine
    /// Used by `Finish`
    Ret,
    /// Halt
    /// Used by `Continue` and `Finish`
    TrapHalt,
}

impl TryFrom<u16> for RelevantInstr {
    type Error = ();

    fn try_from(instr: u16) -> Result<Self, Self::Error> {
        let opcode = instr >> 12;
        match opcode {
            // `RET` is `JMP R7`
            0xC if (instr >> 6) & 0b111 == 7 => Ok(RelevantInstr::Ret),
            // `HALT` is `TRAP 0x25`
            0xF if instr & 0xFF == 0x25 => Ok(RelevantInstr::TrapHalt),
            _ => Err(()),
        }
    }
}

impl Debugger {
    /// Should only be called *once* per process
    pub(super) fn new(
        opts: DebuggerOptions,
        initial_state: RunState,
        breakpoints: Vec<u16>,
    ) -> Self {
        output::is_minimal::set(opts.minimal);

        Self {
            status: Status::default(),
            source: SourceMode::from(opts.command),
            initial_state,
            breakpoints,
            current_breakpoint: None,
        }
    }

    pub(super) fn wait_for_action(&mut self, state: &mut RunState) -> Action {
        let pc = state.pc();

        // 0xFFFF signifies a HALT so don't warn for that
        if pc >= 0xFE00 && pc < 0xFFFF {
            dprintln!(
                Always,
                "WARNING: Program counter entered device address space"
            );
            return Action::Proceed;
        }

        let instr = RelevantInstr::try_from(state.mem(pc)).ok();
        // dprintln!("\x1b[2m-- Instruction: {:?}", instr);

        // Always break from `continue|finish|step|next` on a breakpoint or HALT
        // Breaking on `RET` (for `finish`) is handled later
        // Likewise for completing `step` or `next`
        //
        // Remember if previous cycle paused on the same breakpoint. If so, don't break now.
        if self.breakpoints.contains(&pc) && self.current_breakpoint != Some(pc) {
            dprintln!(Always, "Breakpoint reached. Pausing execution.");
            self.current_breakpoint = Some(pc);
            self.status = Status::WaitForAction;
        } else {
            self.current_breakpoint = None;
            if instr == Some(RelevantInstr::TrapHalt) {
                dprintln!(Always, "HALT reached. Pausing execution.");
                self.status = Status::WaitForAction;
            }
        }

        return self.wait_for_single_action(state, instr);
    }

    // TODO(refactor): Rename `wait_for_single_action`
    fn wait_for_single_action(
        &mut self,
        state: &mut RunState,
        instr: Option<RelevantInstr>,
    ) -> Action {
        // `HALT` and breakpoints should be already handled by caller
        loop {
            // dprintln!("\x1b[2m-- {:?}", self.status);
            match &mut self.status {
                Status::WaitForAction => {
                    // Continue loop until action is given
                    if let Some(action) = self.next_action(state) {
                        return action;
                    }
                }
                Status::Step { count } => {
                    if *count > 0 {
                        *count -= 1;
                    } else {
                        self.status = Status::WaitForAction;
                    }
                    return Action::Proceed;
                }
                Status::Next { return_addr } => {
                    if state.pc() == *return_addr {
                        self.status = Status::WaitForAction;
                    }
                    return Action::Proceed;
                }
                Status::Continue => {
                    return Action::Proceed;
                }
                Status::Finish => {
                    if instr == Some(RelevantInstr::Ret) {
                        dprintln!(Always, "RET reached. Pausing execution.");
                        // Execute `RET` before prompting command again
                        self.status = Status::Step { count: 0 };
                    }
                    return Action::Proceed;
                }
            }
        }
    }

    fn next_action(&mut self, state: &mut RunState) -> Option<Action> {
        // Convert `EOF` to `quit` command
        let command = self.next_command().unwrap_or(Command::Quit);

        match command {
            Command::Quit => return Some(Action::StopDebugger),
            Command::Exit => return Some(Action::ExitProgram),

            Command::Help => {
                dprintln!(Always, "\n{}", include_str!("./help.txt"));
            }

            Command::Continue => {
                self.status = Status::Continue;
                dprintln!(Always, "Continuing...");
            }
            Command::Finish => {
                self.status = Status::Finish;
                dprintln!(Always, "Finishing subroutine...");
            }

            Command::Step { count } => {
                self.status = Status::Step { count: count - 1 };
            }
            Command::Next => {
                self.status = Status::Next {
                    return_addr: state.pc() + 1,
                };
            }

            Command::Get { location } => match location {
                Location::Register(register) => {
                    dprintln!(Always, "Register R{}:", register as u16);
                    print_integer(
                        Output::Debugger(Condition::Always),
                        *state.reg_mut(register as u16),
                    );
                }
                Location::Memory(location) => {
                    let address = self.resolve_location_address(state, &location)?;
                    dprintln!(Always, "Memory at address 0x{:04x}:", address);
                    print_integer(Output::Debugger(Condition::Always), *state.mem_mut(address));
                }
            },

            Command::Set { location, value } => match location {
                Location::Register(register) => {
                    *state.reg_mut(register as u16) = value;
                    dprintln!(Always, "Updated register R{}", register as u16);
                }
                Location::Memory(location) => {
                    let address = self.resolve_location_address(state, &location)?;
                    dprintln!(Always, "Updated memory at address 0x{:04x}.", address);
                    *state.mem_mut(address) = value;
                }
            },

            Command::Registers => print_registers(Output::Debugger(Condition::Always), state),

            Command::Reset => {
                *state = self.initial_state.clone();
                dprintln!(Always, "RESET TO INITIAL STATE");
            }

            Command::Source { .. } => dprintln!(Always, "unimplemented: source"),
            Command::Eval { .. } => dprintln!(Always, "unimplemented: eval"),

            Command::BreakAdd { location } => {
                let address = self.resolve_location_address(state, &location)?;
                if self.breakpoints.contains(&address) {
                    dprintln!(Always, "Breakpoint already exists at 0x{:04x}", address);
                } else {
                    self.breakpoints.push(address);
                    dprintln!(Always, "Added breakpoint at 0x{:04x}", address);
                }
            }
            Command::BreakRemove { location } => {
                let address = self.resolve_location_address(state, &location)?;
                if remove_item_if_exists(&mut self.breakpoints, address) {
                    dprintln!(Always, "Removed breakpoint at 0x{:04x}", address);
                } else {
                    dprintln!(Always, "No breakpoint exists at 0x{:04x}", address);
                }
            }
            Command::BreakList => {
                if self.breakpoints.is_empty() {
                    dprintln!(Always, "No breakpoints exist");
                } else {
                    dprintln!(Always, "Breakpoints:");
                    for breakpoint in &self.breakpoints {
                        dprintln!(Always, "0x{:04x}", breakpoint);
                        // TODO(feat): This could print the instruction at the address, similar to
                        // `source` command
                    }
                }
            }
        }

        None
    }

    /// Returns `None` on EOF
    fn next_command(&mut self) -> Option<Command> {
        // Loop until valid command or EOF
        loop {
            let line = self.source.read()?.trim();
            if line.is_empty() {
                continue;
            }

            let command = match Command::try_from(line) {
                Ok(command) => command,
                Err(error) => {
                    dprintln!(Always, "{}", error);
                    dprintln!(Always, "Type `help` for a list of commands.");
                    continue;
                }
            };

            return Some(command);
        }
    }

    fn resolve_location_address(
        &self,
        state: &mut RunState,
        location: &MemoryLocation,
    ) -> Option<u16> {
        match location {
            MemoryLocation::Address(address) => Some(*address),
            MemoryLocation::PC => Some(state.pc()),
            MemoryLocation::Label(label) => self.resolve_label_address(label),
        }
    }

    fn resolve_label_address(&self, label: &Label) -> Option<u16> {
        let Some(address) = get_label_address(&label.name) else {
            dprintln!(Always, "Label not found named `{}`", label.name);
            return None;
        };

        // Check address in user program area
        let orig = self.orig() as i16;
        let address = address as i16 + label.offset + orig;
        if address < orig || (address as u16) >= 0xFE00 {
            dprintln!(Always, "Label address + offset is out of bounds of memory");
            return None;
        };

        dprintln!(
            Always,
            "Label `{}` is at address 0x{:04x}",
            label.name,
            address
        );
        Some(address as u16)
    }

    fn orig(&self) -> u16 {
        self.initial_state.pc()
    }
}

// TODO(refactor): Move these functions to `crate::output` ?
pub fn print_registers(output: Output, state: &RunState) {
    output.print_str("\x1b[2m┌────────────────────────────────────┐\x1b[0m\n");
    output.print_str("\x1b[2m│        \x1b[3mhex     int    uint    char\x1b[0m\x1b[2m │\x1b[0m\n");
    for i in 0..8 {
        output.print_str(&format!("\x1b[2m│\x1b[0m R{}  ", i));
        print_integer(output, state.reg(i));
        output.print_str(" \x1b[2m│\x1b[0m\n");
    }
    output.print_str("\x1b[2m└────────────────────────────────────┘\x1b[0m\n");
}

fn print_integer(output: Output, value: u16) {
    output.print_str(&format!("0x{:04x}  ", value));
    output.print_str(&format!("{:-6}  ", value));
    output.print_str(&format!("{:-6}  ", value as i16));
    print_char(output, value);
}

fn print_char(output: Output, value: u16) {
    output.print_str("   ");
    // Print 3 characters
    match value {
        // ASCII control characters which are arbitrarily considered significant
        0x00 => output.print_str("NUL"),
        0x08 => output.print_str("BS "),
        0x09 => output.print_str("HT "),
        0x0a => output.print_str("LF "),
        0x0b => output.print_str("VT "),
        0x0c => output.print_str("FF "),
        0x0d => output.print_str("CR "),
        0x1b => output.print_str("ESC"),
        0x7f => output.print_str("DEL"),

        // Space
        0x20 => output.print_str("[_]"),

        // Printable ASCII characters
        0x21..=0x7e => output.print_str(&format!("{:-6}", value as u8 as char)),

        // Any ASCII character not already matched (unimportant control characters)
        0x00..=0x7f => output.print_str("\x1b[2m:::\x1b[0m"),
        // Any non-ASCII character
        0x0080.. => output.print_str("\x1b[2m···\x1b[0m"),
    }
}

fn get_label_address(name: &str) -> Option<u16> {
    with_symbol_table(|sym| sym.get(name).copied())
        // Account for PC being incremented before instruction is executed
        .map(|addr| addr - 1)
}

/// Removes every instance of `item` from `vec`
///
/// Returns whether `item` was found in `vec`
fn remove_item_if_exists<T: PartialEq>(vec: &mut Vec<T>, item: T) -> bool {
    let initial_len = vec.len();
    vec.retain(|x| x != &item);
    initial_len != vec.len()
}
