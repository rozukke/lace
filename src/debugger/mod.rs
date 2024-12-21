mod breakpoint;
mod command;
mod error;
mod eval;
mod parse;
mod source;

pub use self::breakpoint::{Breakpoint, Breakpoints};
use self::command::{Command, Label, Location, MemoryLocation};
use self::source::{Source, SourceRead};
use crate::air::AsmLine;
use crate::dprintln;
use crate::output::{Condition, Output};
use crate::runtime::RunState;
use crate::symbol::with_symbol_table;

#[derive(Debug)]
pub struct DebuggerOptions {
    pub command: Option<String>,
}

pub struct Debugger {
    status: Status,
    source: Source,

    /// Amount of instructions executed since last command
    instruction_count: u32,
    /// Whether PC should be displayed on next command prompt
    should_echo_pc: bool,

    initial_state: RunState,

    breakpoints: Breakpoints,
    current_breakpoint: Option<u16>,

    ast: Vec<AsmLine>,
    src: &'static str,
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
        breakpoints: impl Into<Breakpoints>,
        ast: Vec<AsmLine>,
        src: &'static str,
    ) -> Self {
        Self {
            status: Status::default(),
            source: Source::from(opts.command),
            instruction_count: 0,
            should_echo_pc: true,
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
        if (0xFE00..0xFFFF).contains(&pc) {
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
            if breakpoint.is_predefined {
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

        self.wait_for_single_action(state, instr)
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

        if self.should_echo_pc {
            dprintln!(Sometimes, Info, "Program counter at: 0x{:04x}.", state.pc());
            self.should_echo_pc = false;
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
                self.should_echo_pc = true;
                dprintln!(Always, Info, "Continuing...");
            }
            Command::Finish => {
                self.status = Status::Finish;
                self.should_echo_pc = true;
                dprintln!(Always, Info, "Finishing subroutine...");
            }

            Command::Step { count } => {
                self.status = Status::Step { count: count - 1 };
                self.should_echo_pc = true;
            }
            Command::Next => {
                self.status = Status::Next {
                    return_addr: state.pc() + 1,
                };
                self.should_echo_pc = true;
            }

            Command::Get { location } => match location {
                Location::Register(register) => {
                    dprintln!(Sometimes, Info, "Register R{}:", register as u16);
                    Output::Debugger(Condition::Always, Default::default())
                        .print_integer(state.reg(register as u16));
                }
                Location::Memory(location) => {
                    let address = self.resolve_location_address(state, &location)?;
                    dprintln!(Sometimes, Info, "Memory at address 0x{:04x}:", address);
                    Output::Debugger(Condition::Always, Default::default())
                        .print_integer(state.mem(address));
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

            Command::Jump { location } => {
                let address = self.resolve_location_address(state, &location)?;
                if !(0x3000..0xFE00).contains(&address) {
                    dprintln!(
                        Always,
                        Error,
                        "Address is not in user address space. Must be in range [0x300, 0xFE00)."
                    );
                    return None;
                }
                *state.pc_mut() = address;
                self.should_echo_pc = true;
                dprintln!(Always, Warning, "Set program counter to 0x{:04x}", address);
            }

            Command::Registers => {
                dprintln!(Sometimes, Info, "Registers:");
                Output::Debugger(Condition::Always, Default::default()).print_registers(state);
            }

            Command::Reset => {
                *state = self.initial_state.clone();
                self.should_echo_pc = true;
                dprintln!(Always, Warning, "Reset program to initial state.");
            }

            Command::Source { location } => {
                self.show_source(state, location);
            }

            Command::Eval { instruction } => {
                self.should_echo_pc = true;
                eval::eval(state, instruction);
            }

            Command::BreakAdd { location } => {
                let address = self.resolve_location_address(state, &location)?;
                if self.breakpoints.insert(Breakpoint {
                    address,
                    is_predefined: false,
                }) {
                    dprintln!(
                        Always,
                        Error,
                        "Breakpoint already exists at 0x{:04x}.",
                        address
                    );
                } else {
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
            MemoryLocation::PCOffset(offset) => self.resolve_pc_offset(state.pc(), *offset),
            MemoryLocation::Label(label) => self.resolve_label_address(label),
        }
    }

    fn resolve_pc_offset(&self, pc: u16, offset: i16) -> Option<u16> {
        let Some(address) = self.add_address_offset(pc, offset) else {
            dprintln!(
                Always,
                Error,
                "Program counter + offset is out of bounds of memory."
            );
            return None;
        };
        Some(address)
    }

    fn resolve_label_address(&self, label: &Label) -> Option<u16> {
        let Some(address) = get_label_address(&label.name) else {
            dprintln!(Always, Error, "Label not found named `{}`.", label.name);
            return None;
        };

        let Some(address) = self.add_address_offset(address + self.orig(), label.offset) else {
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
        Some(address)
    }

    fn add_address_offset(&self, address: u16, offset: i16) -> Option<u16> {
        let address = address as i16 + offset;
        // Check address in user program area
        if address >= self.orig() as i16 && (address as u16) < 0xFE00 {
            Some(address as u16)
        } else {
            None
        }
    }

    fn orig(&self) -> u16 {
        self.initial_state.pc()
    }

    pub(super) fn increment_instruction_count(&mut self) {
        self.instruction_count += 1;
    }
}

fn get_label_address(name: &str) -> Option<u16> {
    with_symbol_table(|sym| sym.get(name).copied())
        // Account for PC being incremented before instruction is executed
        .map(|addr| addr - 1)
}
