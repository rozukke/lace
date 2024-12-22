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
use crate::output::{Condition, Output};
use crate::runtime::RunState;
use crate::symbol::with_symbol_table;
use crate::{dprint, dprintln};

/// Leave this as a struct, in case more options are added in the future. Plus it is more explicit.
#[derive(Debug)]
pub struct DebuggerOptions {
    pub command: Option<String>,
}

pub struct Debugger {
    /// Must not be mutated.
    initial_state: RunState,
    asm_source: AsmSource,

    status: Status,
    command_source: Source,

    breakpoints: Breakpoints,
    /// Used to allow breakpoint to be passed on second attempt.
    ///
    /// Compare this with a `HALT` instruction, which is NEVER passed with basic commands
    /// (`progress`, `next`, `continue`, `finish`).
    current_breakpoint: Option<u16>,

    /// Amount of instructions executed since last command.
    instruction_count: u32,
    /// Whether PC should be displayed on next command prompt.
    should_echo_pc: bool,
}

/// Reference to assembly source code.
///
/// Used by `assembly` and `break list` commands.
struct AsmSource {
    orig: u16,
    ast: Vec<AsmLine>,
    src: &'static str,
}

/// The current status of the debugger execution loop.
#[derive(Debug, Default)]
enum Status {
    /// Keep executing user commands, until one changes the debugger status.
    #[default]
    WaitForAction,
    /// Execute `count` instructions.
    ///
    /// Stop execution early if breakpoint or `HALT` is reached.
    ///
    /// Subroutines are not treated specially; `JSR`/`JSRR` and `RET` instructions are treated as
    /// any other instruction is.
    Step { count: u16 },
    /// Execute the next instruction, or the whole subroutine if next instruction is `JSR`/`JSRR`,
    /// until a breakpoint or `HALT`.
    ///
    /// Stop execution early if breakpoint or `HALT` is reached.
    Next { return_addr: u16 },
    /// Execute all instructions until breakpoint or `HALT` is reached.
    Continue,
    /// Execute all instructions until `RET` instruction, breakpoint, or `HALT` is reached.
    ///
    /// Used to 'finish' a subroutine.
    Finish,
}

/// An action, which the debugger passes to the runtime loop.
#[derive(Debug)]
pub enum Action {
    /// Keep executing as normal (with the debugger active).
    Proceed,
    /// Disable the debugger, keep executing.
    StopDebugger,
    /// Exit the entire program, simulating a uninterrupted `HALT`.
    ExitProgram,
}

// TODO(rename): `RelevantInstr`
// TODO(doc)!!!
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RelevantInstr {
    /// Return from a subroutine.
    /// Used by `Finish`
    Ret,
    /// Halt.
    /// Used by `continue` and `finish`.
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
    /// Must only be called *once* per process.
    pub(super) fn new(
        opts: DebuggerOptions,
        initial_state: RunState,
        breakpoints: impl Into<Breakpoints>,
        ast: Vec<AsmLine>,
        src: &'static str,
    ) -> Self {
        let orig = initial_state.pc();
        Self {
            initial_state,
            asm_source: AsmSource { orig, ast, src },

            status: Status::default(),
            command_source: Source::from(opts.command),

            breakpoints: breakpoints.into(),
            current_breakpoint: None,

            instruction_count: 0,
            should_echo_pc: true,
        }
    }

    pub(super) fn wait_for_action(&mut self, state: &mut RunState) -> Action {
        // 0xFFFF signifies a HALT so don't warn for that
        if (0xFE00..0xFFFF).contains(&state.pc()) {
            dprintln!(
                Always,
                Error,
                "Reached end of user program memory. Pausing execution."
            );
            self.status = Status::WaitForAction;
        }

        let instr = RelevantInstr::try_from(state.mem(state.pc())).ok();
        self.check_interrupts(state.pc(), instr);

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

    /// An 'interrupt' here is a breakpoint or `HALT` trap.
    fn check_interrupts(&mut self, pc: u16, instr: Option<RelevantInstr>) {
        // TODO(fix): Breakpoint at a `HALT` will alternate between their warnings when stepping

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
                if let Some(address) = self.resolve_location_address(state, &location) {
                    self.asm_source.show_line_context(address);
                }
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
                        if Output::is_minimal() {
                            dprintln!(Always, Info, "0x{:04x}", breakpoint.address);
                            continue;
                        }
                        dprint!(
                            Always,
                            Info,
                            "{} 0x{:04x}  ──  ",
                            if i + 1 == self.breakpoints.len() {
                                "╰─"
                            } else {
                                "├─"
                            },
                            breakpoint.address
                        );
                        self.asm_source.show_single_line(breakpoint.address);
                        dprintln!(Always);
                    }
                }
            }
        }

        None
    }

    /// Returns `None` on EOF.
    fn next_command(&mut self) -> Option<Command> {
        // Loop until valid command or EOF
        loop {
            let line = self.command_source.read()?.trim();
            // Necessary, since `Command::try_from` assumes non-empty line
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

impl AsmSource {
    pub fn show_line_context(&self, address: u16) {
        let Some(stmt) = self.get_source_statement(address) else {
            return;
        };
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

    pub fn show_single_line(&self, address: u16) {
        let Some(stmt) = self.get_source_statement(address) else {
            return;
        };
        let start = stmt.span.offs();
        let end = start + stmt.span.len();
        let line = &self.src[start..end];
        dprint!(Always, Normal, "{}", line);
    }

    fn get_source_statement(&self, address: u16) -> Option<&AsmLine> {
        if address < self.orig || (address - self.orig) as usize >= self.ast.len() {
            dprintln!(
                Always,
                Info,
                "Address 0x{:04x} does not correspond to an instruction",
                address
            );
            return None;
        };
        let stmt = self
            .ast
            .get((address - self.orig) as usize)
            .expect("index was checked to be within bounds above");
        Some(stmt)
    }
}
