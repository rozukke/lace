use std::io::{stdin, stdout, Write};

use crate::{Air, StaticSource};

// TODO(refactor): Perhaps there is `clap` trait that can be implemented for
// this struct, to avoid field duplication in `Command` enum
pub struct DebuggerOptions {
    pub minimal: bool,
    pub input: Option<String>,
}

pub struct Debugger {
    status: DebuggerStatus,
    minimal: bool,
    input: Option<String>,
    // ...
}

pub enum DebuggerStatus {
    WaitForCommand,
    // ContinueUntilBreakpoint,
    // ContinueUntilEndOfSubroutine,
}

impl Debugger {
    pub fn new(contents: StaticSource, air: Air, opts: DebuggerOptions) -> Self {
        Self {
            status: DebuggerStatus::WaitForCommand,
            minimal: opts.minimal,
            input: opts.input,
        }
    }

    pub fn wait_for_command(&mut self) {
        print!("Command: ");
        stdout().flush().unwrap();
        let mut buffer = String::new();
        stdin().read_line(&mut buffer).unwrap();
        println!("<{}>", buffer.trim());
    }
}
