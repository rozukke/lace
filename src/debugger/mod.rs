mod breakpoint;
mod command;
mod eval;

use std::cmp::Ordering;
use std::ops::Range;

pub use self::breakpoint::{Breakpoint, Breakpoints};
use self::command::read::{CommandSource, SourceRead};
use self::command::{Command, Label, Location, MemoryLocation};
use crate::air::AsmLine;
use crate::output::{Condition, Output};
use crate::runtime::{RunState, HALT_ADDRESS, USER_MEMORY_END};
use crate::symbol::with_symbol_table;
use crate::{dprint, dprintln, DIAGNOSTIC_CONTEXT_LINES};

// TODO(fix): `finish` (fibonacci.asm)

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
    command_source: CommandSource,

    breakpoints: Breakpoints,
    /// Used to allow breakpoint to be ignored if it just broke execution.
    ///
    /// Compare this with a `HALT` instruction, which is NEVER ignored with basic commands
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
    /// Subroutines are not treated specially; `JSR|JSRR|CALL` and `RET` instructions are treated
    /// as any other instruction is.
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
    /// Execute all instructions until `RET` instruction, breakpoint, or `HALT` is reached.
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
            command_source: CommandSource::from(opts.command),

            breakpoints: breakpoints.into(),
            current_breakpoint: None,

            instruction_count: 0,
            should_echo_pc: true,
        }
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
                        // If subroutine was excecuted (for `JSR|JSRR|CALL` + `RET`)
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
        // Always break from `continue|finish|progress|next` on a breakpoint or `HALT`
        // Breaking on `RET` (for `finish`), and at end of `progress` and `next` is handled later

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
        // Inlined to avoid lifetime complications borrowing `self` entirely
        let command = loop {
            let line = match self.command_source.read() {
                Some(line) => line.trim(),
                None => break Command::Quit, // EOF
            };

            // Necessary, since `Command::try_from` assumes non-empty line
            if line.is_empty() {
                continue;
            }

            // Remove silly lifetime restriction
            // SAFETY: Any reference which is returned from this function WILL be valid
            // SAFETY: The buffer which owns this string is not freed until all debugger business
            // has ended
            // SAFETY: The buffer also will not be overwritten until command has entirely
            // completed its execution. The fact that the buffer holds a line of multiple commands
            // does not change this fact
            let line = unsafe { &*(line as *const str) };

            let command = match Command::try_from(line) {
                Ok(command) => command,
                Err(error) => {
                    dprintln!(Always, Error, "{}", error);
                    dprintln!(Sometimes, Error, "Type `help` for a list of commands.");
                    continue;
                }
            };

            break command;
        };

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

    /// Wrapper for [`AsmSource::get_source_statement`].
    ///
    /// Show warning if the memory corresponding to the lines displayed may have been changed.
    fn show_assembly_source(&self, state: &RunState, address: u16) {
        if Output::is_minimal() {
            self.asm_source.show_single_line(address);
            return;
        }

        let Some(stmt) = self.asm_source.show_line_context(address) else {
            return;
        };

        let (start, end) = self.asm_source.get_context_range(self.orig(), stmt);
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
        let address = Self::resolve_label_name_address(label.name)?;

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

    /// Returns `None` if `label` is an invalid label.
    ///
    /// Label names are case-sensitive.
    /// Print a warning if the given name only has a case-insensitive match.
    fn resolve_label_name_address(label: &str) -> Option<u16> {
        with_symbol_table(|sym| {
            if let Some(addr) = sym.get(label) {
                // Account for PC being incremented before instruction is executed
                return Some(addr + 1);
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

    pub(super) fn orig(&self) -> u16 {
        debug_assert_eq!(
            self.asm_source.orig,
            self.initial_state.pc(),
            "orig values do not match",
        );
        self.asm_source.orig
    }

    pub(super) fn increment_instruction_count(&mut self) {
        self.instruction_count += 1;
    }
}

impl AsmSource {
    /// Show lines surrounding instruction/directive corresponding to `address`.
    pub fn show_line_context(&self, address: u16) -> Option<&AsmLine> {
        let stmt = self.get_source_statement(address)?;
        let report = miette::miette!(
            severity = miette::Severity::Advice,
            labels = vec![miette::LabeledSpan::at(
                stmt.span,
                format!("Next instruction, at address 0x{:04x}", address),
            )],
            "",
        )
        .with_source_code(self.src);
        eprintln!("{:?}", report);
        Some(stmt)
    }

    /// Show instruction/directive corresponding to `address`, with no context.
    pub fn show_single_line(&self, address: u16) {
        let Some(stmt) = self.get_source_statement(address) else {
            return;
        };
        let range: Range<usize> = stmt.span.into();
        let line = &self.src[range];
        dprintln!(Always, Normal, "{}", line);
    }

    /// Get [`AsmLine`] corresponding to `address`.
    ///
    /// Used to access source code span.
    fn get_source_statement(&self, address: u16) -> Option<&AsmLine> {
        if address < self.orig || (address - self.orig) as usize >= self.ast.len() {
            dprintln!(
                Always,
                Error,
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

    /// Get memory addresses of first and last line shown in source context.
    fn get_context_range(&self, orig: u16, stmt: &AsmLine) -> (u16, u16) {
        let stmt_start = stmt.span.offs();
        let stmt_end = stmt.span.end();

        // Please note that this code demands trust, not comprehension

        // Split source into characters before and after span
        // Neither string contains characters in the span, but may contain characters in the same
        // line as the instruction
        let source_above = &self.src[..stmt_start];
        let source_below = &self.src[stmt_end..];

        /// Count characters in an iterator, within a maximum amount of lines.
        fn count_chars_in_lines<I>(iter: I) -> usize
        where
            I: Iterator<Item = char>,
        {
            let mut count = 0;
            // Counts 0..=DIAGNOSTIC_CONTEXT_LINES
            // Note inclusive range, to account for characters in current line, outside of
            // instruction span
            let mut line = 0;
            // Previous attempts to restructure/invert this loop have been unproductive
            for ch in iter {
                if ch == '\n' {
                    line += 1;
                    if line > DIAGNOSTIC_CONTEXT_LINES {
                        break;
                    }
                }
                count += 1;
            }
            count
        }

        let start = stmt_start - count_chars_in_lines(source_above.chars().rev());
        let end = stmt_end + count_chars_in_lines(source_below.chars());

        // Ugly -- but what else can be done...
        // Please do not try to abstract this pair of expressions; it won't lead to anything good

        // Get address of earliest statement, whose span is (at least partially) within `start..`
        let start_addr = {
            let mut line = stmt.line;
            for stmt in self.ast.iter().rev() {
                if stmt.span.end() < start {
                    break;
                }
                line = stmt.line;
            }
            // -1 applied to addresses, to account for the `line` field counting from 1, not 0
            line + orig - 1
        };
        // Get address of latest statement, whose span is (at least partially) within `..end`
        let end_addr = {
            let mut line = stmt.line;
            for stmt in self.ast.iter() {
                if stmt.span.offs() >= end {
                    break;
                }
                line = stmt.line;
            }
            line + orig - 1
        };

        (start_addr, end_addr)
    }
}
