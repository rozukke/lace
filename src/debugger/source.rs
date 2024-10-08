use std::io::{self, IsTerminal, Read, Write};

use console::Key;

use super::DEBUGGER_COLOR;

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
    buffer: String,
    /// Byte index
    cursor: usize,
}

// Interactive unbuffered terminal
#[derive(Debug)]
struct Terminal {
    term: console::Term,

    buffer: String,
    /// Byte index
    cursor: usize,

    history: Vec<String>,
    /// Focused item in history, or new entry if index==length
    history_index: usize,
    /// Visible line cursor in terminal
    visible_cursor: usize,
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
        let stdin = io::stdin();
        if stdin.is_terminal() {
            return SourceMode::Terminal(Terminal::new());
        }
        SourceMode::Stdin(Stdin::from(stdin))
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
        // Equivalent code found in terminal source
        dprintln!("\x1b[1mCommand");
        dprintln!("{}", command.unwrap_or("").trim());
        command
    }
}

impl Argument {
    pub fn from(source: String) -> Self {
        Self {
            buffer: source,
            cursor: 0,
        }
    }
}

impl SourceReader for Argument {
    fn read(&mut self) -> Option<&str> {
        // EOF
        if self.cursor >= self.buffer.len() {
            return None;
        }

        // Take characters until delimiter
        let start = self.cursor;
        let mut chars = self.buffer[self.cursor..].chars();
        while let Some(ch) = chars.next().filter(|ch| *ch != '\n' && *ch != ';') {
            self.cursor += ch.len_utf8();
        }

        let end = self.cursor;
        self.cursor += 1; // sizeof('\n' or ';')

        let command = self
            .buffer
            .get(start..end)
            .expect("calculated incorrect character indexes");
        Some(command)
    }
}

impl Stdin {
    pub fn from(stdin: io::Stdin) -> Self {
        Self {
            stdin,
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
                    return None; // First character is EOF
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
            buffer: String::new(),
            cursor: 0,
            history: Vec::new(),
            history_index: 0,
            visible_cursor: 0,
        }
    }

    fn is_next(&self) -> bool {
        debug_assert!(
            self.history_index <= self.history.len(),
            "index went past history"
        );
        self.history_index >= self.history.len()
    }

    /// Run before modifying `next`
    /// If focused on a historic item, clone it to `next` and update index
    fn update_next(&mut self) {
        if self.is_next() {
            return;
        }
        self.buffer = self
            .history
            .get(self.history_index)
            .expect("checked above")
            .clone();
        self.history_index = self.history.len();
    }

    /// Get next or historic command, from index
    fn get_current(&self) -> &str {
        if self.is_next() {
            &self.buffer
        } else {
            self.history.get(self.history_index).expect("checked above")
        }
    }

    fn print_prompt(&mut self) {
        // Clear line, print prompt, set cursor position
        self.term.clear_line().unwrap();

        // Print prompt and current input
        // Equivalent code found in non-terminal source
        write!(self.term, "\x1b[1;{}m", DEBUGGER_COLOR).unwrap();
        write!(self.term, "Command: ").unwrap();
        write!(self.term, "\x1b[0m").unwrap();

        // Inline `self.get_current()` due to borrowing issues
        let current = if self.is_next() {
            &self.buffer
        } else {
            self.history.get(self.history_index).expect("checked above")
        };
        write!(self.term, "{}", current).unwrap();

        self.term
            .move_cursor_left(
                self.get_current()
                    .len()
                    .checked_sub(self.visible_cursor)
                    .unwrap_or(0), // If invariance is somehow violated
            )
            .unwrap();

        self.term.flush().unwrap();
    }

    // Return of `true` indicates to break loop
    fn read_key(&mut self) -> bool {
        let key = self.term.read_key().unwrap();
        match key {
            Key::Enter | Key::Char('\n') => {
                if self.is_next() && self.buffer.trim().is_empty() {
                    self.buffer.clear();
                    println!();
                } else {
                    self.update_next();
                    return true;
                }
            }

            Key::Char(ch) => match ch {
                // Ignore ASCII control characters
                '\x00'..='\x1f' | '\x7f' => (),

                // Pasting should be automatically supported, since terminals simulate typing each
                // character
                _ => {
                    self.update_next();
                    self.buffer.insert(self.visible_cursor, ch);
                    self.visible_cursor += 1;
                }
            },

            Key::Backspace => {
                self.update_next();
                if self.visible_cursor > 0 && self.visible_cursor <= self.get_current().len() {
                    self.buffer.remove(self.visible_cursor - 1);
                    self.visible_cursor -= 1;
                }
            }
            Key::Del => {
                self.update_next();
                if self.visible_cursor + 1 <= self.get_current().len() {
                    self.buffer.remove(self.visible_cursor);
                }
            }

            // Left/right in current input
            Key::ArrowLeft => {
                if self.visible_cursor > 0 {
                    self.visible_cursor -= 1;
                }
            }
            Key::ArrowRight => {
                if self.visible_cursor < self.get_current().len() {
                    self.visible_cursor += 1;
                }
            }

            // Back/forth through history
            Key::ArrowUp => {
                if self.history_index > 0 {
                    self.history_index -= 1;
                    self.visible_cursor = self.get_current().len();
                }
            }
            Key::ArrowDown => {
                if self.history_index < self.history.len() {
                    self.history_index += 1;
                    self.visible_cursor = self.get_current().len();
                }
            }

            _ => (),
        }
        false
    }

    /// Read entire (multi-command) line from terminal
    fn read_line(&mut self) {
        self.buffer.clear();
        self.visible_cursor = 0;

        // Read keys until newline
        loop {
            self.print_prompt();
            if self.read_key() {
                break;
            }
        }
        println!();

        debug_assert!(
            !self.buffer.trim().is_empty(),
            "should have looped until non-empty"
        );

        // Push to history if different to last command
        if !self
            .history
            .last()
            .is_some_and(|previous| previous == &self.buffer)
        {
            self.history.push(self.buffer.clone());
        }
        // Always reset index to next command
        self.history_index = self.history.len();
    }

    /// Returns next command from line buffer
    fn get_next_command(&mut self) -> &str {
        let rest = &self.buffer[self.cursor..];
        match rest.find(';') {
            // Multiple commands in buffer
            // Take first command and update head index
            Some(index) => {
                self.cursor += index + 1;
                &rest[..index]
            }
            // Rest of buffer is 1 command
            // Take rest of buffer and reset head index
            None => {
                self.cursor = 0;
                &rest
            }
        }
    }
}

impl SourceReader for Terminal {
    fn read(&mut self) -> Option<&str> {
        // Reached end of line buffer: read new line
        if self.cursor == 0 {
            self.read_line();
        }
        Some(self.get_next_command())
    }
}
