mod command;
mod source;

use crate::runtime::RunState;
use command::{Command, Location, MemoryLocation};
use source::{SourceMode, SourceReader};

// TODO(feat): Use stderr for all debugger output (except in terminal mode?)

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
        println!("");
        let Some(command) = self.next_command() else {
            return Action::StopDebugger;
        };
        println!("{:?}", command);

        match command {
            Command::Continue => {
                self.status = Status::ContinueUntilBreakpoint;
                println!("Continuing...");
            }

            Command::Get { location } => match location {
                Location::Register(register) => {
                    print!("\x1b[34m");
                    println!("Register R{}:", register as u16);
                    print!("\x1b[0m");
                    let value = *self.state().reg(register as u16);
                    Self::print_integer(value);
                }
                Location::Memory(memory_location) => {
                    let address = match memory_location {
                        MemoryLocation::Address(address) => address,
                        MemoryLocation::PC => self.state().pc,
                        MemoryLocation::Label(_) => {
                            eprintln!("unimplemented: labels");
                            return Action::None;
                        }
                    };

                    print!("\x1b[34m");
                    println!("Memory at address 0x{:04x}:", address);
                    print!("\x1b[0m");
                    let b = self.state().mem(address);
                    println!("b");
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
                eprintln!("unimplemented");
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
                    eprintln!("{:?}", error);
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
        print!("\x1b[34m");
        println!("0x{:04x}\t{}", value, value);
        print!("\x1b[0m");
    }
}
