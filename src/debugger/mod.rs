mod asm;
mod breakpoint;
mod command;
mod eval;

use std::cmp::Ordering;

use self::asm::AsmSource;
use self::command::{Command, CommandReader, Label, Location, MemoryLocation};
use crate::air::AsmLine;
use crate::dprintln;
use crate::output::{Condition, Output};
use crate::runtime::{RunState, HALT_ADDRESS, USER_MEMORY_END};
use crate::symbol::with_symbol_table;

pub use self::breakpoint::{Breakpoint, Breakpoints};

/// Leave this as a struct, in case more options are added in the future. Plus it is more explicit.
#[derive(Debug)]
pub struct DebuggerOptions {
    pub command: Option<String>,
}

pub struct Debugger {
    /// Must not be mutated.
    initial_state: RunState,
    /// Must not be mutated.
    asm_source: AsmSource,

    command_reader: CommandReader,
    status: Status,

    breakpoints: Breakpoints,
    /// Used to allow breakpoint to be ignored if it just broke execution.
    ///
    /// Compare this with a `HALT` instruction, which is NEVER ignored with basic commands
    /// ("progress", "next", "continue", "finish").
    current_breakpoint: Option<u16>,

    /// Amount of instructions executed since last command.
    instruction_count: u32,
    /// Whether PC should be displayed on next command prompt.
    should_echo_pc: bool,
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
    /// Subroutines are not treated specially; `JSR|JSRR|CALL` and `RET`|`RETS` instructions are
    /// treated as any other instruction is.
    Step { count: u16 },
    /// Execute the next instruction, or the whole subroutine if next instruction is
    /// `JSR|JSRR|CALL`.
    ///
    /// Stop execution early if breakpoint or `HALT` is reached.
    ///
    /// Return address is necessary to support nested subroutine calls.
    Next { return_addr: u16 },
    /// Execute all instructions until breakpoint or `HALT` is reached.
    Continue,
    /// Execute all instructions until `RET`|`RETS` instruction, breakpoint, or `HALT` is reached.
    ///
    /// Used to 'finish' a subroutine.
    Finish,
}

/// A message which the debugger passes to the runtime loop.
#[derive(Debug)]
pub enum Action {
    /// Keep executing as normal (with the debugger active).
    Proceed,
    /// Disable the debugger, keep executing.
    StopDebugger,
    /// Exit the entire program, simulating a uninterrupted `HALT`.
    ExitProgram,
}

/// An instruction, which is relevant to the debugger, specifically the "finish" and "continue"
/// commands.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SignificantInstr {
    /// Return from a subroutine. `RET` or `RETS`.
    ///
    /// Used by "finish".
    Return,
    /// Halt. `TRAP 0x25`.
    ///
    /// Used by "continue" and "finish".
    Halt,
}

impl TryFrom<u16> for SignificantInstr {
    type Error = ();
    fn try_from(instr: u16) -> Result<Self, Self::Error> {
        let opcode = instr >> 12;
        match opcode {
            // `RET` is `JMP R7`
            0xC if (instr >> 6) & 0b111 == 7 => Ok(SignificantInstr::Return),
            // `RETS` is `0xD(stack) 0b10 ...`
            0xD if (instr >> 10) & 0b11 == 0b10 => Ok(SignificantInstr::Return),
            // `HALT` is `TRAP 0x25`
            0xF if instr & 0xFF == 0x25 => Ok(SignificantInstr::Halt),
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
            asm_source: AsmSource::from(orig, ast, src),

            command_reader: CommandReader::from(opts.command),
            status: Status::default(),

            breakpoints: breakpoints.into(),
            current_breakpoint: None,

            instruction_count: 0,
            should_echo_pc: true,
        }
    }

    pub(super) fn orig(&self) -> u16 {
        debug_assert_eq!(
            self.asm_source.orig(),
            self.initial_state.pc(),
            "orig values do not match between assembly and initial state",
        );
        self.asm_source.orig()
    }

    pub(super) fn increment_instruction_count(&mut self) {
        self.instruction_count += 1;
    }

    /// Read and execute user commands, until an [`Action`] is raised.
    pub(super) fn next_action(&mut self, state: &mut RunState) -> Action {
        match state.check_pc_bounds() {
            Ordering::Less => {
                // This can probably only happen with a bad `BR*` instruction
                dprintln!(
                    Always,
                    Error,
                    "Out of bounds of user program memory (PC < 0x{:04x}). Pausing execution.",
                    self.orig(),
                );
                self.status = Status::WaitForAction;
            }
            Ordering::Greater if state.pc() != HALT_ADDRESS => {
                dprintln!(
                    Always,
                    Error,
                    "Out of bounds of user program memory (PC >= 0x{:04X}). Pausing execution.",
                    USER_MEMORY_END,
                );
                self.status = Status::WaitForAction;
            }
            _ => (),
        }

        let instr = SignificantInstr::try_from(state.mem(state.pc())).ok();
        self.check_interrupts(state.pc(), instr);

        // `HALT` and breakpoints should be already handled (above)
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
                        // If subroutine was excecuted (for `JSR|JSRR|CALL` + `RET`|`RETS`)
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
                    if instr == Some(SignificantInstr::Return) {
                        dprintln!(
                            Always,
                            Warning,
                            "Reached end of subroutine. Pausing execution."
                        );
                        // Program counter has already been incremented
                        // So immediately break execution now
                        self.status = Status::WaitForAction;
                    }
                    return Action::Proceed;
                }
            }
        }
    }

    /// An 'interrupt' here is a breakpoint or `HALT` trap.
    fn check_interrupts(&mut self, pc: u16, instr: Option<SignificantInstr>) {
        // Always break from "continue|finish|progress|next" on a breakpoint or `HALT`
        // Breaking on `RET`/`RETS` (for "finish"), and at end of "progress" and "next" is handled
        // later

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
                    "Reached preset breakpoint. Pausing execution."
                );
            } else {
                dprintln!(
                    Always,
                    Warning,
                    "Reached runtime breakpoint. Pausing execution."
                );
            }
            self.current_breakpoint = Some(pc);
            self.status = Status::WaitForAction;
            return;
        }

        // Always break on `HALT` (unlike breakpoints)
        if instr == Some(SignificantInstr::Halt) {
            dprintln!(Always, Warning, "Reached HALT. Pausing execution.");
            self.status = Status::WaitForAction;
            return;
        }

        // Only reset current breakpoint if not interrupted.
        self.current_breakpoint = None;
    }

    /// Read and execute the next [`Command`], returning an [`Action`] if it is raised.
    fn run_command(&mut self, state: &mut RunState) -> Option<Action> {
        assert!(
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
                Sometimes,
                Info,
                "Executed {} instruction{}.",
                self.instruction_count,
                if self.instruction_count == 1 { "" } else { "s" },
            );
            self.instruction_count = 0;
        }

        // Read and parse next command
        let command = Command::read_from(&mut self.command_reader, |error| {
            dprintln!(Always, Error, "{}", error);
            dprintln!(Sometimes, Error, "Type `help` for a list of commands.");
        })
        .unwrap_or(Command::Quit); // "quit" on EOF

        match command {
            Command::Quit => return Some(Action::StopDebugger),
            Command::Exit => return Some(Action::ExitProgram),

            Command::Reset => {
                *state = self.initial_state.clone();
                self.should_echo_pc = true;
                // Other fields either:
                // - Shouldn't be mutated/reset
                // - Or would be redundant to do so
                dprintln!(Sometimes, Warning, "Reset program to initial state.");
            }

            Command::Help => {
                dprintln!(Always, Special, "\n{}", include_str!("./help.txt"));
            }

            Command::Continue => {
                self.status = Status::Continue;
                self.should_echo_pc = true;
                dprintln!(Sometimes, Info, "Continuing...");
            }

            Command::Finish => {
                self.status = Status::Finish;
                self.should_echo_pc = true;
                dprintln!(Sometimes, Info, "Finishing subroutine...");
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
                    // Allow non-userspace addresses (it can't hurt)
                    dprintln!(Sometimes, Info, "Memory at address 0x{:04x}:", address);
                    Output::Debugger(Condition::Always, Default::default())
                        .print_integer(state.mem(address));
                }
            },

            Command::Set { location, value } => match location {
                Location::Register(register) => {
                    *state.reg_mut(register as u16) = value;
                    dprintln!(
                        Sometimes,
                        Warning,
                        "Updated register R{} to 0x{:04x}.",
                        register as u16,
                        value,
                    );
                }
                Location::Memory(location) => {
                    let address = self.resolve_location_address(state, &location)?;
                    self.expect_userspace_address(address)?;
                    *state.mem_mut(address) = value;
                    dprintln!(
                        Sometimes,
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
                self.expect_userspace_address(address)?;
                *state.pc_mut() = address;
                self.should_echo_pc = true;
                dprintln!(
                    Sometimes,
                    Warning,
                    "Set program counter to 0x{:04x}",
                    address
                );
            }

            Command::Eval { instruction } => {
                eval::eval(state, instruction);
                self.should_echo_pc = true;
            }

            Command::Source { location } => {
                if let Some(address) = self.resolve_location_address(state, &location) {
                    self.show_assembly_source(state, address);
                }
            }

            Command::BreakAdd { location } => {
                let address = self.resolve_location_address(state, &location)?;
                self.expect_userspace_address(address)?;
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
                    dprintln!(Sometimes, Warning, "Added breakpoint at 0x{:04x}.", address);
                }
            }

            Command::BreakRemove { location } => {
                let address = self.resolve_location_address(state, &location)?;
                self.expect_userspace_address(address)?;
                if self.breakpoints.remove(address) {
                    dprintln!(
                        Sometimes,
                        Warning,
                        "Removed breakpoint at 0x{:04x}.",
                        address
                    );
                } else {
                    dprintln!(Always, Error, "No breakpoint exists at 0x{:04x}.", address);
                }
            }

            Command::BreakList => {
                if self.breakpoints.is_empty() {
                    dprintln!(Sometimes, Info, "No breakpoints exist.");
                } else {
                    dprintln!(Sometimes, Info, "Breakpoints:");

                    if Output::is_minimal() {
                        for breakpoint in &self.breakpoints {
                            dprintln!(Always, Info, "0x{:04x}", breakpoint.address);
                        }
                    } else {
                        Output::Debugger(Condition::Always, Default::default())
                            .print_key_value_table(|i| {
                                let address = self.breakpoints.nth(i)?.address;
                                let label =
                                    resolve_symbol_name(address - self.orig()).unwrap_or("");
                                let line = self.asm_source.get_single_line(address).unwrap_or("");
                                Some((address, label, line))
                            });
                    }
                }
            }
        }

        None
    }

    /// Wrapper for [`AsmSource::get_source_statement`].
    ///
    /// Show warning if the memory corresponding to the lines displayed may have been changed.
    fn show_assembly_source(&self, state: &RunState, address: u16) {
        if Output::is_minimal() {
            self.asm_source.show_single_line(address);
            dprintln!(Always);
            return;
        }

        let Some(stmt) = self.asm_source.show_line_context(address) else {
            dprintln!(
                Always,
                Error,
                "Address 0x{:04x} does not correspond to an instruction",
                address,
            );
            return;
        };

        let (start, end) = self.asm_source.get_context_range(stmt);
        if !state.memory_equals(&self.initial_state, start, end) {
            dprintln!(
                Sometimes,
                Warning,
                "Note: Program memory may have been modified."
            );
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
        let address = resolve_symbol_address(label.name)?;

        let Some(address) = self.add_address_offset(address + self.orig(), label.offset) else {
            dprintln!(
                Always,
                Error,
                "Label address + offset is out of bounds of memory."
            );
            return None;
        };

        dprintln!(
            Sometimes,
            Info,
            "Label `{}` is at address 0x{:04x}.",
            label.name,
            address
        );
        Some(address)
    }

    /// Returns `None` if `pc + offset` is out of bounds.
    fn add_address_offset(&self, address: u16, offset: i16) -> Option<u16> {
        let address = (address as i16).checked_add(offset)?;
        // Check address in user program area
        if address >= self.orig() as i16 && (address as u16) < USER_MEMORY_END {
            Some(address as u16)
        } else {
            None
        }
    }

    fn expect_userspace_address(&self, address: u16) -> Option<()> {
        if (self.orig()..USER_MEMORY_END).contains(&address) {
            return Some(());
        }
        dprintln!(
            Always,
            Error,
            "Address is not in user address space. Must be in range [0x{:04x}, 0x{:04x}).",
            self.orig(),
            USER_MEMORY_END,
        );
        None
    }
}

/// Get address of symbol with given name.
///
/// Returns `None` if `label` is an invalid label.
///
/// Label names are case-sensitive.
/// Prints a warning if the given name only has a case-insensitive match.
fn resolve_symbol_address(label: &str) -> Option<u16> {
    with_symbol_table(|sym| {
        if let Some(addr) = sym.get(label) {
            // -1 to account for PC being incremented before instruction is executed
            return Some(addr - 1);
        }

        dprintln!(Always, Error, "Label not found named `{}`.", label);
        // Check for case-*insensitive* match
        for key in sym.keys() {
            if key.eq_ignore_ascii_case(label) {
                dprintln!(Sometimes, Warning, "Hint: Similar label named `{}`", key);
                break;
            }
        }
        None
    })
}

/// Get name of symbol with given address.
///
/// Returns `None` if no symbol exists at `address`.
fn resolve_symbol_name(address: u16) -> Option<&'static str> {
    with_symbol_table(|sym| {
        for (label, symbol_address) in sym {
            // +1 to account for PC being incremented before instruction is executed
            if *symbol_address == address + 1 {
                // SAFETY: Symbol table is statically allocated, and all keys will last until the
                // end of the program lifetime
                let label_static = unsafe { &*(label.as_str() as *const str) };
                return Some(label_static);
            }
        }
        None
    })
}
