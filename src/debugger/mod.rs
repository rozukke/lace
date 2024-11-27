mod command;
mod parse;
mod source;

mod print;
#[doc(hidden)]
pub use print::print as _print;

use crate::{dprint, dprintln};
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
    pub(super) fn new(opts: DebuggerOptions, initial_state: RunState) -> Self {
        print::set_is_minimal(opts.minimal);

        Self {
            status: Status::default(),
            source: SourceMode::from(opts.command),
            initial_state,
            breakpoints: Vec::new(),
            current_breakpoint: None,
        }
    }

    pub(super) fn wait_for_action(&mut self, state: &mut RunState) -> Action {
        let pc = *state.pc();

        // 0xFFFF signifies a HALT so don't warn for that
        if pc >= 0xFE00 && pc < 0xFFFF {
            dprintln!("WARNING: Program counter entered device address space");
            return Action::Proceed;
        }

        let instr = RelevantInstr::try_from(*state.mem(pc)).ok();
        dprintln!("\x1b[2m-- Instruction: {:?}", instr);

        // Always break from `continue|finish|step|next` on a breakpoint or HALT
        // Breaking on `RET` (for `finish`) is handled later
        // Likewise for completing `step` or `next`
        //
        // Remember if previous cycle paused on the same breakpoint. If so, don't break now.
        if self.breakpoints.contains(&pc) && self.current_breakpoint != Some(pc) {
            dprintln!("Breakpoint reached. Pausing execution.");
            self.current_breakpoint = Some(pc);
            self.status = Status::WaitForAction;
        } else {
            self.current_breakpoint = None;
            if instr == Some(RelevantInstr::TrapHalt) {
                dprintln!("HALT reached. Pausing execution.");
                self.status = Status::WaitForAction;
            }
        }

        return self.wait_for_single_action(state, instr);
    }

    fn wait_for_single_action(
        &mut self,
        state: &mut RunState,
        instr: Option<RelevantInstr>,
    ) -> Action {
        // `HALT` and breakpoints should be already handled by caller
        loop {
            dprintln!("\x1b[2m-- {:?}", self.status);
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
                    if state.pc() == return_addr {
                        self.status = Status::WaitForAction;
                    }
                    return Action::Proceed;
                }
                Status::Continue => {
                    return Action::Proceed;
                }
                Status::Finish => {
                    if instr == Some(RelevantInstr::Ret) {
                        dprintln!("RET reached. Pausing execution.");
                        // Execute `RET` before prompting command again
                        self.status = Status::Step { count: 0 };
                    }
                    return Action::Proceed;
                }
            }
        }
    }

    fn next_action(&mut self, state: &mut RunState) -> Option<Action> {
        let Some(command) = self.next_command() else {
            return Some(Action::StopDebugger); // EOF
        };

        match command {
            Command::Quit => return Some(Action::StopDebugger),
            Command::Exit => return Some(Action::ExitProgram),

            Command::Help => {
                dprintln!("\n{}", include_str!("./help.txt"));
            }

            Command::Continue => {
                self.status = Status::Continue;
                dprintln!("Continuing...");
            }
            Command::Finish => {
                self.status = Status::Finish;
                dprintln!("Finishing subroutine...");
            }

            Command::Step { count } => {
                self.status = Status::Step { count: count - 1 };
            }
            Command::Next => {
                self.status = Status::Next {
                    return_addr: *state.pc() + 1,
                };
            }

            Command::Get { location } => match location {
                Location::Register(register) => {
                    dprintln!("Register R{}:", register as u16);
                    Self::print_integer(*state.reg(register as u16));
                }
                Location::Memory(location) => {
                    let address = self.resolve_location_address(state, &location)?;
                    dprintln!("Memory at address 0x{:04x}:", address);
                    Self::print_integer(*state.mem(address));
                }
            },

            Command::Set { location, value } => match location {
                Location::Register(register) => {
                    *state.reg(register as u16) = value;
                    dprintln!("Updated register R{}", register as u16);
                }
                Location::Memory(location) => {
                    let address = self.resolve_location_address(state, &location)?;
                    dprintln!("Updated memory at address 0x{:04x}.", address);
                    *state.mem(address) = value;
                }
            },

            Command::Registers => Self::print_registers(state),

            Command::Reset => {
                *state = self.initial_state.clone();
                dprintln!("RESET TO INITIAL STATE");
            }

            Command::Source { .. } => dprintln!("unimplemented: source"),
            Command::Eval { .. } => dprintln!("unimplemented: eval"),

            Command::BreakAdd { location } => {
                let address = self.resolve_location_address(state, &location)?;
                if self.breakpoints.contains(&address) {
                    dprintln!("Breakpoint already exists at 0x{:04x}", address);
                } else {
                    self.breakpoints.push(address);
                    dprintln!("Added breakpoint at 0x{:04x}", address);
                }
            }
            Command::BreakRemove { location } => {
                let address = self.resolve_location_address(state, &location)?;
                if remove_item_if_exists(&mut self.breakpoints, address) {
                    dprintln!("Removed breakpoint at 0x{:04x}", address);
                } else {
                    dprintln!("No breakpoint exists at 0x{:04x}", address);
                }
            }
            Command::BreakList => {
                if self.breakpoints.is_empty() {
                    dprintln!("No breakpoints exist");
                } else {
                    dprintln!("Breakpoints:");
                    for breakpoint in &self.breakpoints {
                        dprintln!("0x{:04x}", breakpoint);
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
                    dprintln!("{}", error);
                    dprintln!("Type `help` for a list of commands.");
                    continue;
                }
            };

            return Some(command);
        }
    }

    fn print_registers(state: &mut RunState) {
        dprintln!("----------------------");
        dprintln!("| Registers:");
        for i in 0..8 {
            dprint!("| R{}  ", i);
            Self::print_integer(*state.reg(i));
        }
        dprintln!("----------------------");
    }

    fn print_integer(value: u16) {
        dprintln!("0x{:04x}\t{}", value, value);
    }

    fn resolve_location_address(
        &mut self,
        state: &mut RunState,
        location: &MemoryLocation,
    ) -> Option<u16> {
        match location {
            MemoryLocation::Address(address) => Some(*address),
            MemoryLocation::PC => Some(*state.pc()),
            MemoryLocation::Label(label) => self.resolve_label_address(label),
        }
    }

    fn resolve_label_address(&mut self, label: &Label) -> Option<u16> {
        let Some(address) = get_label_address(&label.name) else {
            dprintln!("Label not found named `{}`", label.name);
            return None;
        };

        // Check address in user program area
        let orig = self.orig() as i16;
        let address = address as i16 + label.offset + orig;
        if address < orig || (address as u16) >= 0xFE00 {
            dprintln!("Label address + offset is out of bounds of memory");
            return None;
        };

        dprintln!("Label `{}` is at address 0x{:04x}", label.name, address);
        Some(address as u16)
    }

    fn orig(&mut self) -> u16 {
        *self.initial_state.pc()
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
