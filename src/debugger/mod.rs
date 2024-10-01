mod command;
mod source;

use crate::runtime::MEMORY_MAX;
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

#[derive(Debug)]
pub struct Debugger {
    state: State,
    minimal: bool,
    source: SourceMode,

    orig: u16,
    memory: Memory,
}

#[derive(Debug, Default)]
pub enum State {
    #[default]
    WaitForAction,
    // ContinueUntilBreakpoint,
    // ContinueUntilEndOfSubroutine,
}

#[derive(Debug)]
pub enum Action {
    Continue,
    StopDebugger,
    QuitProgram,
}

impl Debugger {
    pub fn new(opts: DebuggerOptions, orig: u16, memory: Memory) -> Self {
        Self {
            state: State::default(),
            minimal: opts.minimal,
            source: SourceMode::from(opts.input),
            orig,
            memory,
        }
    }

    pub fn wait_for_action(&mut self) -> Action {
        loop {
            // TODO
            let Some(line) = self.source.read() else {
                println!("EOF");
                break Action::StopDebugger;
            };
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            let command = match Command::try_from(line) {
                Ok(command) => command,
                Err(err) => {
                    eprintln!("{:?}", err);
                    continue;
                }
            };

            println!("{:?}", command);
        }
    }
}
