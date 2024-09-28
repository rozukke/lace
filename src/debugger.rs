use std::io::{IsTerminal, Read, Write};

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

    input: CommandSource,
    // ...
}

pub enum Status {
    WaitForCommand,
    // ContinueUntilBreakpoint,
    // ContinueUntilEndOfSubroutine,
}

// TODO(refactor): Rename all these types!
enum CommandSource {
    Input(CommandInput),
    History(CommandHistory),
}

struct CommandInput {
    source: String,
    cursor: usize,
}

struct CommandHistory {
    history: Vec<String>,
    next: String,
    index: usize, // Focused item in history, or new entry if index==length
    cursor: usize,
}

impl Debugger {
    pub fn new(contents: StaticSource, air: Air, opts: DebuggerOptions) -> Self {
        let input = match opts.input {
            Some(source) => CommandSource::Input(CommandInput::from(source)),
            None => CommandSource::History(CommandHistory::new()),
        };
        Self {
            status: Status::WaitForCommand,
            minimal: opts.minimal,
            input,
        }
    }

    pub fn wait_for_command(&mut self) {
        loop {
            // for item in &self.history.history {
            //     println!(" - {}", item);
            // }

            let Some(line) = self.input.read_command() else {
                println!("EOF?");
                break;
            };
            println!("<{}>", line);
        }
    }
}

impl CommandSource {
    fn read_command(&mut self) -> Option<&str> {
        match self {
            Self::Input(input) => input.read_command(),
            Self::History(history) => history.read_command(),
        }
    }
}

impl CommandInput {
    pub fn from(source: String) -> Self {
        Self { source, cursor: 0 }
    }

    fn read_command(&mut self) -> Option<&str> {
        // TODO(opt): This recalculates char index each time
        let mut chars = self.source.chars().skip(self.cursor).peekable();
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

        Some(self.source.get(start..end).expect("checked above"))
    }
}

impl CommandHistory {
    pub fn new() -> Self {
        Self {
            history: Vec::new(),
            next: String::new(),
            index: 0,
            cursor: 0,
        }
    }

    // TODO(feat): Support non-terminal stdin
    // TODO(feat): Handle EOF (return None)
    pub fn read_command(&mut self) -> Option<&str> {
        let mut cons = console::Term::stdout();

        self.next.clear();
        self.cursor = 0;

        loop {
            // Clear line, print prompt, set cursor position
            cons.clear_line().unwrap();
            // Must use `write!` to be flushed
            write!(cons, "Command: ").unwrap();
            // write!(cons, " {}/{}> ", self.index, self.history.len()).unwrap();
            write!(cons, "{}", self.get_current()).unwrap();
            cons.move_cursor_left(
                self.get_current()
                    .len()
                    .checked_sub(self.cursor)
                    .unwrap_or(0),
            )
            .unwrap();
            cons.flush().unwrap();

            let key = cons.read_key().unwrap();
            match key {
                Key::Enter | Key::Char('\n') => {
                    if !self.next.is_empty() || self.index < self.history.len() {
                        self.update_next();
                        break;
                    } else {
                        println!();
                    }
                }

                Key::Char(ch) => match ch {
                    ';' => {
                        unimplemented!("multiple commands in one line");
                    }

                    ' ' if self.next.is_empty() => (),

                    // ASCII printable characters
                    '\x20'..='\x7e' => {
                        self.update_next();
                        self.next.insert(self.cursor, ch);
                        self.cursor += 1;
                    }
                    _ => (),
                },

                Key::Backspace => {
                    self.update_next();
                    if !self.next.is_empty() {
                        self.next.pop();
                        self.cursor -= 1;
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

    /// Run before modifying `next`
    /// If focused on a history item, clone it to `next` and update index
    fn update_next(&mut self) {
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
