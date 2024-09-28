use std::io::{self, IsTerminal, Read, Write};

use console::Key;

use crate::{Air, StaticSource};

// TODO(refactor): Perhaps there is `clap` trait that can be implemented for
// this struct, to avoid field duplication in `Command` enum
pub struct DebuggerOptions {
    pub minimal: bool,
    pub input: Option<String>,
}

pub struct Debugger {
    status: Status,
    minimal: bool,

    command_source: CommandSource,
    // ...
}

pub enum Status {
    WaitForCommand,
    // ContinueUntilBreakpoint,
    // ContinueUntilEndOfSubroutine,
}

// TODO(refactor?): These types could be put in a module
enum CommandSource {
    Stdin,
    Terminal(TerminalSource),
    Argument(ArgumentSource),
}

struct ArgumentSource {
    argument: String,
    cursor: usize,
}

struct TerminalSource {
    next: String,
    history: Vec<String>,
    /// Line cursor
    cursor: usize,
    /// Focused item in history, or new entry if index==length
    index: usize,
}

impl Debugger {
    pub fn new(contents: StaticSource, air: Air, opts: DebuggerOptions) -> Self {
        Self {
            status: Status::WaitForCommand,
            minimal: opts.minimal,
            command_source: CommandSource::from(opts.input),
        }
    }

    pub fn wait_for_command(&mut self) {
        loop {
            let Some(line) = self.command_source.read() else {
                println!("EOF");
                break;
            };
            println!("<{}>", line);
        }
    }
}

impl CommandSource {
    pub fn from(argument: Option<String>) -> Self {
        if let Some(argument) = argument {
            return CommandSource::Argument(ArgumentSource::from(argument));
        }
        if io::stdin().is_terminal() {
            return CommandSource::Terminal(TerminalSource::new());
        }
        CommandSource::Stdin
    }

    pub fn read(&mut self) -> Option<&str> {
        match self {
            Self::Stdin => self.read_stdin(),
            Self::Terminal(terminal) => terminal.read(),
            Self::Argument(argument) => argument.read(),
        }
    }

    // TODO(feat): Handle EOF (return None)
    fn read_stdin(&self) -> Option<&str> {
        let ch = Self::read_stdin_char()?;
        todo!();
    }

    fn read_stdin_char() -> Option<char> {
        let mut buffer = [0; 1];
        if io::stdin().read(&mut buffer).unwrap() == 0 {
            return None;
        }
        Some(buffer[0] as char)
    }
}

impl ArgumentSource {
    pub fn from(source: String) -> Self {
        Self {
            argument: source,
            cursor: 0,
        }
    }

    pub fn read(&mut self) -> Option<&str> {
        // TODO(opt): This recalculates char index each time
        // Skip leading whitespace
        let mut chars = self.argument.chars().skip(self.cursor).peekable();
        while let Some(ch) = chars.peek() {
            if !ch.is_ascii_whitespace() {
                break;
            }
            chars.next();
            self.cursor += 1;
        }
        let start = self.cursor;

        // EOF
        if chars.peek().is_none() {
            return None;
        }

        // Take characters until delimiter
        let mut end = self.cursor;
        while let Some(ch) = chars.next() {
            if ch == '\n' || ch == ';' {
                break;
            }
            self.cursor += 1;
            if !ch.is_ascii_whitespace() {
                end = self.cursor;
            }
        }
        self.cursor += 1;

        Some(self.argument.get(start..end).expect("checked above"))
    }
}

impl TerminalSource {
    pub fn new() -> Self {
        Self {
            next: String::new(),
            history: Vec::new(),
            cursor: 0,
            index: 0,
        }
    }

    pub fn read(&mut self) -> Option<&str> {
        // TODO(opt): This creates a new handle each time
        let mut cons = console::Term::stdout();

        self.next.clear();
        self.cursor = 0;

        // Read keys until newline
        loop {
            // Clear line, print prompt, set cursor position
            cons.clear_line().unwrap();
            // Must use `write!` to be flushed
            write!(cons, "Command: ").unwrap();
            write!(cons, "{}", self.get_current()).unwrap();
            cons.move_cursor_left(
                self.get_current()
                    .len()
                    .checked_sub(self.cursor)
                    .unwrap_or(0), // If invariance is violated
            )
            .unwrap();
            cons.flush().unwrap();

            let key = cons.read_key().unwrap();
            match key {
                Key::Enter | Key::Char('\n') => {
                    if self.is_next() && self.next.is_empty() {
                        println!();
                    } else {
                        self.update_next();
                        break;
                    }
                }

                Key::Char(ch) => match ch {
                    // Ignore ASCII control characters
                    '\x00'..='\x1f' | '\x7f' => (),

                    ';' => {
                        // This would need a buffer field on `Self`
                        unimplemented!("multiple commands in one line");
                    }

                    // Skip leading whitespace
                    ' ' if self.next.is_empty() => (),

                    // Depending on terminal, this should also support pasting from clipboard
                    _ => {
                        self.update_next();
                        self.next.insert(self.cursor, ch);
                        self.cursor += 1;
                    }
                },

                Key::Backspace => {
                    self.update_next();
                    if !self.next.is_empty() {
                        self.next.pop();
                        self.cursor -= 1;
                    }
                }

                Key::ArrowLeft => {
                    if self.cursor > 0 {
                        self.cursor -= 1;
                    }
                }
                Key::ArrowRight => {
                    if self.cursor < self.get_current().len() {
                        self.cursor += 1;
                    }
                }
                Key::ArrowUp => {
                    if self.index > 0 {
                        self.index -= 1;
                        self.cursor = self.get_current().len();
                    }
                }
                Key::ArrowDown => {
                    if self.index < self.history.len() {
                        self.index += 1;
                        self.cursor = self.get_current().len();
                    }
                }

                _ => (),
            }
        }
        println!();

        debug_assert!(!self.next.is_empty(), "should have looped until non-empty");

        // Push to history if different to last command
        if !self
            .history
            .last()
            .is_some_and(|previous| previous == &self.next)
        {
            self.history.push(self.next.clone());
        }
        // Always reset index to next command
        self.index = self.history.len();

        Some(&self.next)
    }

    fn is_next(&self) -> bool {
        debug_assert!(self.index <= self.history.len(), "index went past history");
        self.index >= self.history.len()
    }

    /// Run before modifying `next`
    /// If focused on a history item, clone it to `next` and update index
    fn update_next(&mut self) {
        debug_assert!(self.index <= self.history.len(), "index went past history");
        if self.index < self.history.len() {
            self.next = self.history.get(self.index).expect("checked above").clone();
            self.index = self.history.len();
        }
    }

    /// Get next or historic command, from index
    fn get_current(&self) -> &str {
        assert!(self.index <= self.history.len(), "index went past history");
        if self.index >= self.history.len() {
            &self.next
        } else {
            self.history.get(self.index).expect("checked above")
        }
    }
}
