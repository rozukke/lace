use std::io::{self, IsTerminal, Read, Write};

use console::Key;

#[allow(private_interfaces)]
pub enum Source {
    Argument(Argument),
    Stdin(Stdin),
    Terminal(Terminal),
}

struct Stdin {
    line: String,
}

struct Argument {
    argument: String,
    cursor: usize,
}

struct Terminal {
    next: String,
    history: Vec<String>,
    /// Line cursor
    cursor: usize,
    /// Focused item in history, or new entry if index==length
    index: usize,
}

pub trait SourceReader {
    /// Returned string slice will not include leading or trailing whitespace
    fn read(&mut self) -> Option<&str>;
}

impl Source {
    pub fn from(argument: Option<String>) -> Self {
        if let Some(argument) = argument {
            return Source::Argument(Argument::from(argument));
        }
        if io::stdin().is_terminal() {
            return Source::Terminal(Terminal::new());
        }
        Source::Stdin(Stdin::new())
    }
}

impl SourceReader for Source {
    fn read(&mut self) -> Option<&str> {
        let command = match self {
            Self::Argument(argument) => argument.read(),
            Self::Stdin(stdin) => stdin.read(),
            Self::Terminal(terminal) => return terminal.read(),
        };
        // Echo prompt and command for non-terminal source
        println!("Command: {}", command.unwrap_or(""));
        command
    }
}

impl Argument {
    pub fn from(source: String) -> Self {
        Self {
            argument: source,
            cursor: 0,
        }
    }
}

impl SourceReader for Argument {
    fn read(&mut self) -> Option<&str> {
        // TODO(opt): This recalculates char index each time
        let mut chars = self.argument.chars().skip(self.cursor);

        // Take characters until delimiter
        let start = self.cursor;
        loop {
            let Some(ch) = chars.next() else {
                if start == self.cursor {
                    // First character is EOF
                    return None;
                }
                break;
            };
            if ch == '\n' || ch == ';' {
                break;
            }
            self.cursor += 1;
        }

        let end = self.cursor;
        self.cursor += 1;

        let command = self
            .argument
            .get(start..end)
            .expect("calculated incorrect character indexes")
            .trim();
        Some(command)
    }
}

impl Stdin {
    pub fn new() -> Self {
        Self {
            line: String::new(),
        }
    }

    fn read_stdin_char() -> Option<char> {
        let mut buffer = [0; 1];
        if io::stdin().read(&mut buffer).unwrap() == 0 {
            return None;
        }
        Some(buffer[0] as char)
    }
}

impl SourceReader for Stdin {
    fn read(&mut self) -> Option<&str> {
        self.line.clear();

        // Take characters until delimiter
        loop {
            let Some(ch) = Self::read_stdin_char() else {
                if self.line.is_empty() {
                    // First character is EOF
                    return None;
                }
                break;
            };
            if ch == '\n' || ch == ';' {
                break;
            }
            self.line.push(ch);
        }

        Some(self.line.trim())
    }
}

impl Terminal {
    pub fn new() -> Self {
        Self {
            next: String::new(),
            history: Vec::new(),
            cursor: 0,
            index: 0,
        }
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

impl SourceReader for Terminal {
    fn read(&mut self) -> Option<&str> {
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

        Some(&self.next.trim())
    }
}
