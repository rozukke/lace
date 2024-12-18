mod command;
mod eval;
mod parse;
mod source;

use self::command::{Command, Label, Location, MemoryLocation};
use self::source::{SourceMode, SourceReader};
use crate::air::AsmLine;
use crate::dprintln;
use crate::output::{Condition, Output};
use crate::runtime::RunState;
use crate::symbol::with_symbol_table;

// TODO(refactor): Delete struct and replace with `Option<String>`
#[derive(Debug)]
pub struct DebuggerOptions {
    pub command: Option<String>,
}

pub struct Debugger {
    status: Status,
    source: SourceMode,

    // TODO(refactor): Make private, use method to increment
    pub(super) instruction_count: u32,
    // TODO(refactor): Rename `was_pc_changed` to something better (it doesn't necessarily indicate
    // that the pc was changed, just that it COULD have)
    was_pc_changed: bool,

    initial_state: RunState,

    breakpoints: Breakpoints,
    current_breakpoint: Option<u16>,

    ast: Vec<AsmLine>,
    src: &'static str,
}

// TODO(feat): Keep sorted!
#[derive(Debug)]
pub struct Breakpoints(Vec<Breakpoint>);

#[derive(Clone, Copy, Debug)]
pub struct Breakpoint {
    pub address: u16,
    pub predefined: bool,
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

impl Breakpoints {
    fn get(&self, address: u16) -> Option<Breakpoint> {
        for breakpoint in &self.0 {
            if breakpoint.address == address {
                return Some(*breakpoint);
            }
        }
        None
    }

    fn contains(&self, address: u16) -> bool {
        for breakpoint in &self.0 {
            if breakpoint.address == address {
                return true;
            }
        }
        false
    }

    fn insert(&mut self, breakpoint: Breakpoint) {
        self.0.push(breakpoint);
    }

    /// Removes every breakpoint with given address
    ///
    /// Returns whether any breakpoint was found with given address
    fn remove(&mut self, address: u16) -> bool {
        let initial_len = self.0.len();
        self.0.retain(|breakpoint| breakpoint.address != address);
        initial_len != self.0.len()
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn iter(&self) -> impl Iterator<Item = &Breakpoint> {
        self.0.iter()
    }
}

impl From<Vec<Breakpoint>> for Breakpoints {
    fn from(vec: Vec<Breakpoint>) -> Self {
        Self(vec)
    }
}

impl<'a> IntoIterator for &'a Breakpoints {
    type Item = &'a Breakpoint;
    type IntoIter = std::slice::Iter<'a, Breakpoint>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
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
        breakpoints: impl Into<Breakpoints>,
        ast: Vec<AsmLine>,
        src: &'static str,
    ) -> Self {
        Self {
            status: Status::default(),
            source: SourceMode::from(opts.command),
            instruction_count: 0,
            was_pc_changed: true,
            initial_state,
            breakpoints: breakpoints.into(),
            current_breakpoint: None,
            ast,
            src,
        }
    }

    pub(super) fn wait_for_action(&mut self, state: &mut RunState) -> Action {
        let pc = state.pc();

        // 0xFFFF signifies a HALT so don't warn for that
        if pc >= 0xFE00 && pc < 0xFFFF {
            dprintln!(
                Always,
                Error,
                "Reached end of user program memory. Pausing execution."
            );
            self.status = Status::WaitForAction;
        }

        let instr = RelevantInstr::try_from(state.mem(pc)).ok();

        // Always break from `continue|finish|step|next` on a breakpoint or HALT
        // Breaking on `RET` (for `finish`) is handled later
        // Likewise for completing `step` or `next`
        //
        // Remember if previous cycle paused on the same breakpoint. If so, don't break now.
        if let Some(breakpoint) = self
            .breakpoints
            .get(pc)
            .filter(|_| self.current_breakpoint != Some(pc))
        {
            if breakpoint.predefined {
                dprintln!(
                    Always,
                    Warning,
                    "Reached predefined breakpoint. Pausing execution."
                );
            } else {
                dprintln!(Always, Warning, "Reached breakpoint. Pausing execution.");
            }
            self.current_breakpoint = Some(pc);
            self.status = Status::WaitForAction;
        } else {
            self.current_breakpoint = None;
            if instr == Some(RelevantInstr::TrapHalt) {
                dprintln!(Always, Warning, "Reached HALT. Pausing execution.");
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
                        // If subroutine was excecuted (for `JSR`/`JSRR` + `RET`)
                        // As opposed to a single instruction
                        if self.instruction_count > 1 {
                            dprintln!(
                                Always,
                                Warning,
                                "Reached end of subroutine. Pausing execution."
                            );
                        }
                        self.status = Status::WaitForAction;
                        continue;
                    }
                    return Action::Proceed;
                }
                Status::Continue => {
                    return Action::Proceed;
                }
                Status::Finish => {
                    if instr == Some(RelevantInstr::Ret) {
                        dprintln!(
                            Always,
                            Warning,
                            "Reached end of subroutine. Pausing execution."
                        );
                        // Execute `RET` before prompting command again
                        self.status = Status::Step { count: 0 };
                    }
                    return Action::Proceed;
                }
            }
        }
    }

    fn next_action(&mut self, state: &mut RunState) -> Option<Action> {
        Output::Debugger(Condition::Always, Default::default()).start_new_line();

        if self.was_pc_changed {
            dprintln!(Sometimes, Info, "Program counter at: 0x{:04x}.", state.pc());
            self.was_pc_changed = false;
        }
        if self.instruction_count > 0 {
            dprintln!(
                Always,
                Info,
                "Executed {} instruction{}.",
                self.instruction_count,
                if self.instruction_count == 1 { "" } else { "s" },
            );
            self.instruction_count = 0;
        }

        // Convert `EOF` to `quit` command
        let command = self.next_command().unwrap_or(Command::Quit);

        match command {
            Command::Quit => return Some(Action::StopDebugger),
            Command::Exit => return Some(Action::ExitProgram),

            Command::Help => {
                dprintln!(Always, Special, "\n{}", include_str!("./help.txt"));
            }

            Command::Continue => {
                self.status = Status::Continue;
                self.was_pc_changed = true;
                dprintln!(Always, Info, "Continuing...");
            }
            Command::Finish => {
                self.status = Status::Finish;
                self.was_pc_changed = true;
                dprintln!(Always, Info, "Finishing subroutine...");
            }

            Command::Step { count } => {
                self.status = Status::Step { count: count - 1 };
                self.was_pc_changed = true;
            }
            Command::Next => {
                self.status = Status::Next {
                    return_addr: state.pc() + 1,
                };
                self.was_pc_changed = true;
            }

            Command::Get { location } => match location {
                // TODO(feat): Draw box around value, mirroring `registers`
                Location::Register(register) => {
                    dprintln!(Always, Info, "Register R{}:", register as u16);
                    Output::Debugger(Condition::Always, Default::default())
                        .print_integer(state.reg(register as u16));
                    dprintln!(Always);
                }
                Location::Memory(location) => {
                    let address = self.resolve_location_address(state, &location)?;
                    dprintln!(Always, Info, "Memory at address 0x{:04x}:", address);
                    Output::Debugger(Condition::Always, Default::default())
                        .print_integer(state.mem(address));
                    dprintln!(Always);
                }
            },

            Command::Set { location, value } => match location {
                Location::Register(register) => {
                    *state.reg_mut(register as u16) = value;
                    dprintln!(Always, Warning, "Updated register R{}.", register as u16);
                }
                Location::Memory(location) => {
                    let address = self.resolve_location_address(state, &location)?;
                    dprintln!(
                        Always,
                        Warning,
                        "Updated memory at address 0x{:04x}.",
                        address
                    );
                    *state.mem_mut(address) = value;
                }
            },

            Command::Registers => {
                Output::Debugger(Condition::Always, Default::default()).print_registers(state);
            }

            Command::Reset => {
                *state = self.initial_state.clone();
                self.was_pc_changed = true;
                dprintln!(Always, Warning, "Reset program to initial state.");
            }

            Command::Source { location } => {
                self.show_source(state, location);
            }

            Command::Eval { instruction } => {
                self.was_pc_changed = true;
                eval::eval(state, instruction);
            }

            Command::BreakAdd { location } => {
                let address = self.resolve_location_address(state, &location)?;
                if self.breakpoints.contains(address) {
                    dprintln!(
                        Always,
                        Error,
                        "Breakpoint already exists at 0x{:04x}.",
                        address
                    );
                } else {
                    self.breakpoints.insert(Breakpoint {
                        address,
                        predefined: false,
                    });
                    dprintln!(Always, Warning, "Added breakpoint at 0x{:04x}.", address);
                }
            }
            Command::BreakRemove { location } => {
                let address = self.resolve_location_address(state, &location)?;
                if self.breakpoints.remove(address) {
                    dprintln!(Always, Warning, "Removed breakpoint at 0x{:04x}.", address);
                } else {
                    dprintln!(Always, Error, "No breakpoint exists at 0x{:04x}.", address);
                }
            }
            Command::BreakList => {
                if self.breakpoints.is_empty() {
                    dprintln!(Always, Info, "No breakpoints exist.");
                } else {
                    dprintln!(Always, Info, "Breakpoints:");
                    for (i, breakpoint) in self.breakpoints.iter().enumerate() {
                        dprintln!(
                            Always,
                            Info,
                            "{} 0x{:04x}",
                            if i + 1 == self.breakpoints.len() {
                                "╰─"
                            } else {
                                "├─"
                            },
                            breakpoint.address
                        );
                        // TODO(feat): This could print the instruction at the
                        // address, similar to `source` command
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
                    dprintln!(Always, Error, "{}", error);
                    dprintln!(Always, Error, "Type `help` for a list of commands.");
                    continue;
                }
            };

            return Some(command);
        }
    }

    fn show_source(&self, state: &RunState, location: MemoryLocation) {
        let Some(address) = self.resolve_location_address(state, &location) else {
            return;
        };

        let orig = self.orig();
        if address < orig || (address - orig) as usize >= self.ast.len() {
            dprintln!(
                Always,
                Info,
                "Address 0x{:04x} does not correspond to an instruction",
                address
            );
            return;
        };
        let stmt = self
            .ast
            .get((address - orig) as usize)
            .expect("index was checked to be within bounds above");

        let report = miette::miette!(
            severity = miette::Severity::Advice,
            labels = vec![miette::LabeledSpan::at(
                stmt.span,
                format!("At address 0x{:04x}", address),
            )],
            "",
        )
        .with_source_code(self.src);
        eprintln!("{:?}", report);
    }

    fn resolve_location_address(&self, state: &RunState, location: &MemoryLocation) -> Option<u16> {
        match location {
            MemoryLocation::Address(address) => Some(*address),
            MemoryLocation::PC => Some(state.pc()),
            MemoryLocation::Label(label) => self.resolve_label_address(label),
        }
    }

    fn resolve_label_address(&self, label: &Label) -> Option<u16> {
        let Some(address) = get_label_address(&label.name) else {
            dprintln!(Always, Error, "Label not found named `{}`.", label.name);
            return None;
        };

        // Check address in user program area
        let orig = self.orig() as i16;
        let address = address as i16 + label.offset + orig;
        if address < orig || (address as u16) >= 0xFE00 {
            dprintln!(
                Always,
                Error,
                "Label address + offset is out of bounds of memory."
            );
            return None;
        };

        dprintln!(
            Always,
            Info,
            "Label `{}` is at address 0x{:04x}.",
            label.name,
            address
        );
        Some(address as u16)
    }

    fn orig(&self) -> u16 {
        self.initial_state.pc()
    }
}

fn get_label_address(name: &str) -> Option<u16> {
    with_symbol_table(|sym| sym.get(name).copied())
        // Account for PC being incremented before instruction is executed
        .map(|addr| addr - 1)
}
