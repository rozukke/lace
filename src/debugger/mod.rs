macro_rules! dprint {
    ( $fmt:literal $($tt:tt)* ) => {{
        eprint!(concat!("\x1b[{}m", $fmt, "\x1b[0m"), DEBUGGER_COLOR $($tt)*);
    }};
}
macro_rules! dprintln {
    () => {{
        eprintln!();
    }};
    ( $fmt:literal $($tt:tt)* ) => {{
        eprintln!(concat!("\x1b[{}m", $fmt, "\x1b[0m"), DEBUGGER_COLOR $($tt)*);
    }};
}

mod command;
mod parse;
mod source;

use crate::runtime::RunState;
use command::{Command, Location, MemoryLocation};
use source::{SourceMode, SourceReader};

const DEBUGGER_COLOR: u8 = 34;

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
    minimal: bool,
    source: SourceMode,

    // TODO(refactor): Make this good
    initial_state: Box<RunState>,
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
    pub(super) fn new(opts: DebuggerOptions, initial_state: RunState) -> Self {
        Self {
            status: Status::default(),
            minimal: opts.minimal,
            source: SourceMode::from(opts.command),
            initial_state: Box::new(initial_state),
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
        println!("Instruction: {:?}", instr);

        // Always break from `continue`/`next`/`step` if HALT is reached
        if instr == Some(RelevantInstr::TrapHalt) {
            dprintln!("HALT reached. Pausing execution.");
            self.status = Status::WaitForAction;
        }

        return self.wait_for_single_action(state, instr);
    }

    fn wait_for_single_action(
        &mut self,
        state: &mut RunState,
        instr: Option<RelevantInstr>,
    ) -> Action {
        loop {
            println!("{:?}", self.status);
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
                    // TODO(feat): Breakpoints
                    // Halt already handled
                    return Action::Proceed;
                }

                Status::Finish => {
                    // TODO(feat): Breakpoints
                    // Halt already handled
                    match instr {
                        Some(RelevantInstr::Ret) => {
                            dprintln!("RET reached. Pausing execution.");
                            // Execute `RET` before prompting command again
                            self.status = Status::Step { count: 0 };
                        }
                        _ => return Action::Proceed,
                    }
                }
            };
        }
    }

    fn next_action(&mut self, state: &mut RunState) -> Option<Action> {
        dprintln!();
        let Some(command) = self.next_command() else {
            return Some(Action::StopDebugger); // EOF
        };

        match command {
            Command::Quit => return Some(Action::StopDebugger),
            Command::Exit => return Some(Action::ExitProgram),

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
                Location::Memory(memory_location) => {
                    let address = match memory_location {
                        MemoryLocation::Address(address) => address,
                        MemoryLocation::PC => *state.pc(),
                        MemoryLocation::Label(_) => {
                            dprintln!("unimplemented: labels");
                            return None;
                        }
                    };

                    dprintln!("Memory at address 0x{:04x}:", address);
                    Self::print_integer(*state.mem(address));
                }
            },

            Command::Set { location, value } => match location {
                Location::Register(register) => {
                    *state.reg(register as u16) = value;
                    dprintln!("Updated register R{}", register as u16);
                }
                Location::Memory(memory_location) => {
                    let address = match memory_location {
                        MemoryLocation::Address(address) => address,
                        MemoryLocation::PC => *state.pc(),
                        MemoryLocation::Label(_) => {
                            dprintln!("unimplemented: labels");
                            return None;
                        }
                    };
                    dprintln!("Updated memory at address 0x{:04x}.", address);
                    *state.mem(address) = value;
                }
            },

            Command::Registers => Self::print_registers(state),

            Command::Reset => {
                *state = *self.initial_state.clone();
                println!("RESET TO INITIAL STATE");
            }

            Command::Source { .. } => dprintln!("unimplemented: source"),
            Command::Eval { .. } => dprintln!("unimplemented: eval"),

            Command::BreakAdd { .. } => dprintln!("unimplemented: break add"),
            Command::BreakRemove { .. } => dprintln!("unimplemented: break remove"),
            Command::BreakList { .. } => dprintln!("unimplemented: break list"),
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
}
