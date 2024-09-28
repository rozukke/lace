mod source;

use crate::runtime::MEMORY_MAX;
use source::{Source, SourceReader};

type Memory = Box<[u16; MEMORY_MAX]>;

// TODO(refactor): Perhaps there is `clap` trait that can be implemented for
// this struct, to avoid field duplication in `Command` enum
pub struct DebuggerOptions {
    pub minimal: bool,
    pub input: Option<String>,
}

pub struct Debugger {
    state: State,
    minimal: bool,
    source: Source,

    orig: u16,
    memory: Memory,
}

pub enum State {
    WaitForCommand,
    // ContinueUntilBreakpoint,
    // ContinueUntilEndOfSubroutine,
}

impl Debugger {
    pub fn new(opts: DebuggerOptions, orig: u16, memory: Memory) -> Self {
        Self {
            state: State::WaitForCommand,
            minimal: opts.minimal,
            source: Source::from(opts.input),
            orig,
            memory,
        }
    }

    pub fn wait_for_command(&mut self) {
        loop {
            let Some(line) = self.source.read() else {
                println!("EOF");
                break;
            };
            println!("<{}>", line);
        }
    }
}
