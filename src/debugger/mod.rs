mod source;

use crate::{Air, StaticSource};

use source::{Source, SourceReader};

// TODO(refactor): Perhaps there is `clap` trait that can be implemented for
// this struct, to avoid field duplication in `Command` enum
pub struct DebuggerOptions {
    pub minimal: bool,
    pub input: Option<String>,
}

pub struct Debugger {
    status: Status,
    minimal: bool,

    source: Source,
    // ...
}

pub enum Status {
    WaitForCommand,
    // ContinueUntilBreakpoint,
    // ContinueUntilEndOfSubroutine,
}

impl Debugger {
    pub fn new(contents: StaticSource, air: Air, opts: DebuggerOptions) -> Self {
        Self {
            status: Status::WaitForCommand,
            minimal: opts.minimal,
            source: Source::from(opts.input),
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
