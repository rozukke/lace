// macro_rules! dprint {
//     ( $($tt:tt)* ) => {{
//         eprint!(concat!("\x1b[{}m", $fmt, "\x1b[0m"), DEBUGGER_COLOR $($tt)*);
//     }};
// }
macro_rules! dprintln {
    () => {{
        eprintln!();
    }};
    ( $fmt:literal $($tt:tt)* ) => {{
        eprintln!(concat!("\x1b[{}m", $fmt, "\x1b[0m"), DEBUGGER_COLOR $($tt)*);
    }};
}

mod command;
mod source;

use crate::runtime::RunState;
use command::{Command, Location, MemoryLocation};
use source::{SourceMode, SourceReader};

// TODO(feat): Use stderr for all debugger output (except in terminal mode?)

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
    state: *mut RunState,
    initial_state: Box<RunState>,
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub enum Status {
    #[default]
    WaitForAction,
    ContinueUntilBreakpoint,
    ContinueUntilEndOfSubroutine,
}

#[derive(Debug)]
pub enum Action {
    None,
    Proceed,
    StopDebugger,
    QuitProgram,
}

impl Debugger {
    pub(super) fn new(opts: DebuggerOptions, state: &mut RunState) -> Self {
        Self {
            status: Status::default(),
            minimal: opts.minimal,
            source: SourceMode::from(opts.command),
            state: state as *mut RunState,
            initial_state: Box::new(state.clone()),
        }
    }

    pub fn wait_for_action(&mut self) -> Action {
        dprintln!();
        let Some(command) = self.next_command() else {
            return Action::StopDebugger;
        };
        eprintln!("{:?}", command); // Never use color

        match command {
            Command::Continue => {
                self.status = Status::ContinueUntilBreakpoint;
                dprintln!("Continuing...");
            }

            Command::Get { location } => match location {
                Location::Register(register) => {
                    dprintln!("Register R{}:", register as u16);
                    let value = *self.state().reg(register as u16);
                    Self::print_integer(value);
                }
                Location::Memory(memory_location) => {
                    let address = match memory_location {
                        MemoryLocation::Address(address) => address,
                        MemoryLocation::PC => self.state().pc,
                        MemoryLocation::Label(_) => {
                            dprintln!("unimplemented: labels");
                            return Action::None;
                        }
                    };

                    dprintln!("Memory at address 0x{:04x}:", address);
                    let b = self.state().mem(address);
                    Self::print_integer(*b);
                }
            },

            Command::Set { location, value } => match location {
                Location::Register(register) => {
                    *self.state().reg(register as u16) = value;
                }
                Location::Memory(_memory_location) => {
                    todo!();
                }
            },

            _ => {
                dprintln!("unimplemented");
            }
        }

        Action::None
    }

    // Returns `None` on EOF
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

    fn state(&mut self) -> &mut RunState {
        // TODO: safety
        unsafe { &mut *self.state }
    }

    fn print_integer(value: u16) {
        dprintln!("0x{:04x}\t{}", value, value);
    }
}
