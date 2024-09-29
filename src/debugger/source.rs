use std::io::{self, IsTerminal, Read, Write};

use console::Key;

#[allow(private_interfaces)] // Perhaps a bad practice
#[derive(Debug)]
pub enum SourceMode {
    Argument(Argument),
    Stdin(Stdin),
    Terminal(Terminal),
}

// Stdin which is not attached to a terminal, i.e. piped.
#[derive(Debug)]
struct Stdin {
    stdin: io::Stdin,
    /// Command must be stored somewhere to be referenced
    buffer: String,
}

// Command-line argument
#[derive(Debug)]
struct Argument {
    argument: String,
    cursor: usize,
}

// Interactive unbuffered terminal
#[derive(Debug)]
struct Terminal {
    term: console::Term,
    next: String,
    history: Vec<String>,
    head: Option<usize>,
    /// Line cursor
    cursor: usize,
    /// Focused item in history, or new entry if index==length
    index: usize,
}

pub trait SourceReader {
    /// `None` indicates EOF
    /// Returned string slice MAY include leading or trailing whitespace
    fn read(&mut self) -> Option<&str>;
}

impl SourceMode {
    pub fn from(argument: Option<String>) -> Self {
        if let Some(argument) = argument {
            return SourceMode::Argument(Argument::from(argument));
        }
        if io::stdin().is_terminal() {
            return SourceMode::Terminal(Terminal::new());
        }
        SourceMode::Stdin(Stdin::new())
    }
}

impl SourceReader for SourceMode {
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
            .expect("calculated incorrect character indexes");
        Some(command)
    }
}

impl Stdin {
    pub fn new() -> Self {
        Self {
            stdin: io::stdin(),
            buffer: String::new(),
        }
    }

    /// `None` indicates EOF
    fn read_char(&mut self) -> Option<char> {
        let mut buffer = [0; 1];
        if self.stdin.read(&mut buffer).unwrap() == 0 {
            return None;
        }
        Some(buffer[0] as char)
    }
}

impl SourceReader for Stdin {
    fn read(&mut self) -> Option<&str> {
        self.buffer.clear();

        // Take characters until delimiter
        loop {
            let Some(ch) = self.read_char() else {
                if self.buffer.is_empty() {
                    // First character is EOF
                    return None;
                }
                break;
            };
            if ch == '\n' || ch == ';' {
                break;
            }
            self.buffer.push(ch);
        }

        Some(&self.buffer)
    }
}

impl Terminal {
    pub fn new() -> Self {
        Self {
            term: console::Term::stdout(),
            next: String::new(),
            history: Vec::new(),
            head: None,
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

    fn print_prompt(&mut self) {
        // Clear line, print prompt, set cursor position
        self.term.clear_line().unwrap();

        // Must use `write!` to be flushed
        write!(self.term, "Command: ").unwrap();

        // TODO: What in the world is this...
        let current = unsafe { &*(self.get_current() as *const str) };
        write!(self.term, "{}", current).unwrap();

        self.term
            .move_cursor_left(
                self.get_current()
                    .len()
                    .checked_sub(self.cursor)
                    .unwrap_or(0), // If invariance is violated
            )
            .unwrap();

        self.term.flush().unwrap();
    }

    // Return of `true` indicates to break loop
    fn read_key(&mut self) -> bool {
        let key = self.term.read_key().unwrap();
        match key {
            Key::Enter | Key::Char('\n') => {
                if self.is_next() && self.next.is_empty() {
                    println!();
                } else {
                    self.update_next();
                    return true;
                }
            }

            Key::Char(ch) => match ch {
                // Ignore ASCII control characters
                '\x00'..='\x1f' | '\x7f' => (),

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
        false
    }
}

impl SourceReader for Terminal {
    fn read(&mut self) -> Option<&str> {
        // Read next command in line with command delimeters
        if let Some(head) = self.head {
            // TODO(refactor): This is duplicated at the bottom of the function
            let rest = &self.next[head..];
            let command = match rest.find(';') {
                Some(index) => {
                    self.head = Some(head + index + 1);
                    &rest[..index]
                }
                None => {
                    self.head = None;
                    &rest
                }
            };
            return Some(command);
        }

        self.next.clear();
        self.cursor = 0;

        // Read keys until newline
        loop {
            self.print_prompt();
            if self.read_key() {
                break;
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

        // Return first command in line
        let command = match self.next.find(';') {
            Some(index) => {
                self.head = Some(index + 1);
                &self.next[..index]
            }
            None => {
                self.head = None;
                &self.next
            }
        };
        Some(command)
    }
}
