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
use crate::runtime::{RunState, USER_MEMORY_END};
use crate::symbol::with_symbol_table;
use crate::{dprint, dprintln};

// TODO(fix): Decide which messages should be `Sometimes`

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

/// An instruction, which is relevant to the debugger, specifically the `finish` and `continue`
/// commands.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SignificantInstr {
    /// Return from a subroutine.
    /// Used by `finish`.
    Ret,
    /// Halt.
    /// Used by `continue` and `finish`.
    TrapHalt,
}

impl TryFrom<u16> for SignificantInstr {
    type Error = ();
    fn try_from(instr: u16) -> Result<Self, Self::Error> {
        let opcode = instr >> 12;
        match opcode {
            // `RET` is `JMP R7`
            0xC if (instr >> 6) & 0b111 == 7 => Ok(SignificantInstr::Ret),
            // `HALT` is `TRAP 0x25`
            0xF if instr & 0xFF == 0x25 => Ok(SignificantInstr::TrapHalt),
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

    /// Read and execute user commands, until an [`Action`] is raised.
    pub(super) fn next_action(&mut self, state: &mut RunState) -> Action {
        if state.pc() < self.orig() {
            // This shouldn't occur anyway
            dprintln!(
                Always,
                Error,
                // TODO(feat): Better message
                "Out of bounds of user program memory. Pausing execution."
            );
            self.status = Status::WaitForAction;
        }
        // 0xFFFF signifies a HALT so don't warn for that
        if state.pc() >= USER_MEMORY_END && state.pc() != 0xFFFF {
            dprintln!(
                Always,
                Error,
                "Reached end of user program memory. Pausing execution."
            );
            self.status = Status::WaitForAction;
        }

        let instr = SignificantInstr::try_from(state.mem(state.pc())).ok();
        self.check_interrupts(state.pc(), instr);

        // `HALT` and breakpoints should be already handled by caller
        loop {
            match &mut self.status {
                Status::WaitForAction => {
                    // Continue loop until action is given
                    if let Some(action) = self.run_command(state) {
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
                    if instr == Some(SignificantInstr::Ret) {
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
    fn check_interrupts(&mut self, pc: u16, instr: Option<SignificantInstr>) {
        // Always break from `continue|finish|step|next` on a breakpoint or `HALT`
        // Breaking on `RET` (for `finish`), and end of `step` and `next` is handled later

        // Remember if previous cycle paused on the same breakpoint
        // If so, don't break now
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
            return;
        }

        // Always break on `HALT` (unlike breakpoints)
        if instr == Some(SignificantInstr::TrapHalt) {
            dprintln!(Always, Warning, "Reached HALT. Pausing execution.");
            self.status = Status::WaitForAction;
            return;
        }

        // Only reset current breakpoint if not interrupted.
        self.current_breakpoint = None;
    }

    /// Read and execute the next [`Command`], returning an [`Action`] if it is raised.
    fn run_command(&mut self, state: &mut RunState) -> Option<Action> {
        debug_assert!(
            matches!(self.status, Status::WaitForAction),
            "`run_command` must only be called if `status == WaitForAction`",
        );

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

            Command::Reset => {
                *state = self.initial_state.clone();
                self.should_echo_pc = true;
                // Other fields either:
                //  - Shouldn't be mutated/reset: `initial_state`, `asm_source`, `command_source`, `breakpoints`
                //  - Or would be redundant to do so: `status`, `current_breakpoint`, `instruction_count`
                dprintln!(Always, Warning, "Reset program to initial state.");
            }

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
                    dprintln!(
                        Always,
                        Warning,
                        "Updated register R{} to 0x{:04x}.",
                        register as u16,
                        value,
                    );
                }
                Location::Memory(location) => {
                    let address = self.resolve_location_address(state, &location)?;
                    *state.mem_mut(address) = value;
                    dprintln!(
                        Always,
                        Warning,
                        "Updated memory at address 0x{:04x} to 0x{:04x}.",
                        address,
                        value,
                    );
                }
            },

            Command::Registers => {
                dprintln!(Sometimes, Info, "Registers:");
                Output::Debugger(Condition::Always, Default::default()).print_registers(state);
            }

            Command::Jump { location } => {
                let address = self.resolve_location_address(state, &location)?;
                if !(self.orig()..USER_MEMORY_END).contains(&address) {
                    dprintln!(
                        Always,
                        Error,
                        "Address is not in user address space. Must be in range [0x{:04x}, 0x{:04x}).",
                        self.orig(),
                        USER_MEMORY_END,
                    );
                    return None;
                }
                *state.pc_mut() = address;
                self.should_echo_pc = true;
                dprintln!(Always, Warning, "Set program counter to 0x{:04x}", address);
            }

            Command::Eval { instruction } => {
                self.should_echo_pc = true;
                eval::eval(state, instruction);
            }

            Command::Source { location } => {
                // TODO(feat): Only check memory in context range
                if !state.memory_equals(&self.initial_state) {
                    dprintln!(
                        Always,
                        Warning,
                        "Note: Program memory may have been modified."
                    );
                }
                if let Some(address) = self.resolve_location_address(state, &location) {
                    self.asm_source.show_line_context(address);
                }
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
                            "\x1b[2m{}\x1b[0m 0x{:04x}  \x1b[2m──\x1b[0m  ",
                            if i + 1 == self.breakpoints.len() {
                                "╰─"
                            } else {
                                "├─"
                            },
                            breakpoint.address
                        );
                        self.asm_source.show_single_line(breakpoint.address);
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

    /// Returns `None` if `location` is out of bounds or an invalid label.
    fn resolve_location_address(&self, state: &RunState, location: &MemoryLocation) -> Option<u16> {
        match location {
            MemoryLocation::Address(address) => Some(*address),
            MemoryLocation::PCOffset(offset) => self.resolve_pc_offset(state.pc(), *offset),
            MemoryLocation::Label(label) => self.resolve_label_address(label),
        }
    }

    /// Returns `None` if `pc + offset` is out of bounds.
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

    /// Returns `None` if `label` is out of bounds or an invalid label.
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

    /// Returns `None` if `pc + offset` is out of bounds.
    fn add_address_offset(&self, address: u16, offset: i16) -> Option<u16> {
        let address = address as i16 + offset;
        // Check address in user program area
        if address >= self.orig() as i16 && (address as u16) < USER_MEMORY_END {
            Some(address as u16)
        } else {
            None
        }
    }

    pub(super) fn orig(&self) -> u16 {
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
    /// Show lines surrounding instruction/directive corresponding to `address`.
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
            "", // TODO(feat): Maybe add a message here?
        )
        .with_source_code(self.src);
        eprintln!("{:?}", report);
    }

    /// Show instruction/directive corresponding to `address`, with no context.
    pub fn show_single_line(&self, address: u16) {
        let Some(stmt) = self.get_source_statement(address) else {
            return;
        };
        let start = stmt.span.offs();
        let end = start + stmt.span.len();
        let line = &self.src[start..end];
        dprintln!(Always, Normal, "{}", line);
    }

    /// Get [`AsmLine`] corresponding to `address`.
    ///
    /// Used to access source code span.
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
