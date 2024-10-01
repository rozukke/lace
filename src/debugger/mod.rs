mod command;
mod source;

use crate::{runtime::MEMORY_MAX, RunState};
use command::Command;
use source::{SourceMode, SourceReader};

type Memory = Box<[u16; MEMORY_MAX]>;

// TODO(feat): Use stderr for all debugger output (except in terminal mode?)

// TODO(refactor): Perhaps there is `clap` trait that can be implemented for
// this struct, to avoid field duplication in `Command` enum
#[derive(Debug)]
pub struct DebuggerOptions {
    pub minimal: bool,
    pub input: Option<String>,
}

pub struct Debugger {
    status: Status,
    minimal: bool,
    source: SourceMode,

    // TODO(refactor): Make this good
    state: *mut RunState,
    initial_state: Box<RunState>,
}

#[derive(Debug, Default)]
pub enum Status {
    #[default]
    WaitForAction,
    ContinueUntilBreakpoint,
    ContinueUntilEndOfSubroutine,
}

#[derive(Debug)]
pub enum Action {
    Proceed,
    StopDebugger,
    QuitProgram,
}

impl Debugger {
    pub fn new(opts: DebuggerOptions, state: &mut RunState) -> Self {
        Self {
            status: Status::default(),
            minimal: opts.minimal,
            source: SourceMode::from(opts.input),
            state: state as *mut RunState,
            initial_state: Box::new(state.clone()),
        }
    }

    pub fn wait_for_action(&mut self) -> Action {
        let Some(command) = self.next_command() else {
            return Action::StopDebugger;
        };
        println!("{:?}", command);

        match command {
            Command::Continue => {
                self.status = Status::ContinueUntilBreakpoint;
                println!("Continuing...");
            }

            _ => {
                eprintln!("(command not yet implemented)");
            }
        }

        Action::Proceed
    }

    // Returns `None` on EOF
    fn next_command(&mut self) -> Option<Command> {
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
}
