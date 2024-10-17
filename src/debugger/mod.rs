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
    ContinueUntilZero {
        count: u16,
    },
    ContinueUntilBreakpoint,
    ContinueUntilEndOfSubroutine,
}

#[derive(Debug)]
pub enum Action {
    Proceed,
    StopDebugger,
    ExitProgram,
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
        loop {
            println!("{:?}", self.status);
            match &mut self.status {
                Status::WaitForAction => {
                    let Some(action) = self.next_action(state) else {
                        continue;
                    };
                    return action;
                }

                Status::ContinueUntilZero { count: 0 | 1 } => self.status = Status::WaitForAction,
                Status::ContinueUntilZero { count } => *count -= 1,

                Status::ContinueUntilBreakpoint => {
                    // TODO
                }
                Status::ContinueUntilEndOfSubroutine => {
                    // TODO
                }
            }
            return Action::Proceed;
        }
    }

    fn next_action(&mut self, state: &mut RunState) -> Option<Action> {
        dprintln!();
        let Some(command) = self.next_command() else {
            return Some(Action::StopDebugger); // EOF
        };
        eprintln!("{:?}", command); // Never use color

        match command {
            Command::Continue => {
                self.status = Status::ContinueUntilBreakpoint;
                dprintln!("Continuing...");
            }

            Command::Step { count } => {
                self.status = Status::ContinueUntilZero { count };
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

            Command::Quit => return Some(Action::StopDebugger),
            Command::Exit => return Some(Action::ExitProgram),

            _ => {
                dprintln!("unimplemented");
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
                    dprintln!("{:?}", error);
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
