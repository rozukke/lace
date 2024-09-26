use std::io::{stdin, stdout, Write};

use crate::{Air, StaticSource};

pub struct Debugger {
    status: DebuggerStatus,
    // ...
}

pub enum DebuggerStatus {
    WaitForCommand,
    // ContinueUntilBreakpoint,
    // ContinueUntilEndOfSubroutine,
}

impl Debugger {
    pub fn new(contents: StaticSource, air: Air) -> Self {
        Self {
            status: DebuggerStatus::WaitForCommand,
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
