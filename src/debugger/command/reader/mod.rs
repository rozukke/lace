mod argument;
mod stdin;
mod terminal;

use std::io::{self, IsTerminal as _};

use self::argument::Argument;
use self::stdin::Stdin;
use self::terminal::Terminal;
use crate::{dprint, dprintln};

/// Must be ASCII to ensure `.len() == .chars().count()`
const PROMPT: &str = "DEBUGGER> ";

/// Initial capacity of a command buffer.
const INITIAL_BUFFER_CAPACITY: usize = 64;

/// Read from argument first, if `Some`. Then read from stream.
#[derive(Debug)]
pub struct CommandReader {
    argument: Option<Argument>,
    stream: Stream,
}

/// Stdin or interactive terminal.
#[derive(Debug)]
enum Stream {
    Stdin(Stdin),
    Terminal(Terminal),
}

impl CommandReader {
    pub fn from(argument: Option<String>) -> Self {
        Self {
            argument: argument.map(Argument::from),
            stream: Stream::new(),
        }
    }
}

impl Stream {
    pub fn new() -> Self {
        let stdin = io::stdin();
        if stdin.is_terminal() {
            Self::Terminal(Terminal::new())
        } else {
            Self::Stdin(Stdin::from(stdin))
        }
    }
}

/// A trait for objects which can yield a command, by iterating a string or reading a file.
pub trait Read {
    /// `None` indicates EOF.
    /// Returned string slice MAY include leading or trailing whitespace.
    fn read(&mut self) -> Option<&str>;
}

impl Read for CommandReader {
    fn read(&mut self) -> Option<&str> {
        // Always try to read from argument first
        // If argument is `None`, or if read from argument returns `None`, then read from stream
        // Note that `self.argument` cannot then be set to `None`, due to lifetime of returned value
        if let Some(argument) = &mut self.argument {
            if let Some(command) = argument.read() {
                echo_command(Some(command));
                return Some(command);
            }
        }
        self.stream.read()
    }
}

impl Read for Stream {
    fn read(&mut self) -> Option<&str> {
        match self {
            Self::Stdin(stdin) => {
                let command = stdin.read();
                echo_command(command);
                command
            }
            Self::Terminal(terminal) => terminal.read(),
        }
    }
}

/// Print prompt and command.
fn echo_command(command: Option<&str>) {
    if command.is_some_and(|command| command.trim().is_empty()) {
        return;
    }
    // Equivalent code found in `Terminal`
    dprint!(Sometimes, Normal, "\x1b[1m{}", PROMPT);
    dprintln!(
        Sometimes,
        Normal,
        "{}",
        command.unwrap_or("\x1b[3m(end of input)").trim()
    );
}
