use std::fmt;
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, IsTerminal, Read, Write};
use std::path::PathBuf;

use console::Key;

use crate::{dprint, dprintln, output::Output};

/// Read from argument first, if `Some`. Then read from stream.
#[allow(private_interfaces)] // Perhaps a bad practice
#[derive(Debug)]
pub struct Source {
    argument: Option<Argument>,
    stream: Stream,
}

/// Stdin or interactive terminal
#[derive(Debug)]
enum Stream {
    Stdin(Stdin),
    Terminal(Terminal),
}

/// Command-line argument
#[derive(Debug)]
struct Argument {
    buffer: String,
    /// Byte index
    cursor: usize,
}

/// Stdin which is not attached to a terminal, i.e. piped.
#[derive(Debug)]
struct Stdin {
    stdin: io::Stdin,
    /// Command must be stored somewhere to be referenced
    buffer: String,
}

/// Interactive unbuffered terminal
// TODO(feat): Support CTRL+Arrow keybinds
#[derive(Debug)]
struct Terminal {
    term: console::Term,

    buffer: String,
    /// Byte index
    cursor: usize,

    /// Visible line cursor in terminal
    visible_cursor: usize,

    history: TerminalHistory,
}

#[derive(Debug)]
struct TerminalHistory {
    list: Vec<String>,
    /// Focused item in history, or new entry if index==length
    index: usize,
    /// `None` indicates failure to open file
    file: Option<File>,
    // /// `None` indicates failure to retrieve file path
    // path: PathBuf,
}

fn echo_command_prompt(command: Option<&str>) {
    // Echo prompt and command for non-terminal source
    // Equivalent code found in terminal source
    if !Output::is_minimal() || command.is_some() {
        dprint!(Always, Normal, "\x1b[1mCommand: ");
        dprintln!(
            Always,
            Normal,
            "{}",
            command.unwrap_or("\x1b[3m(end of input)").trim()
        );
    }
}

pub trait SourceReader {
    /// `None` indicates EOF
    /// Returned string slice MAY include leading or trailing whitespace
    fn read(&mut self) -> Option<&str>;
}

impl Source {
    pub fn from(argument: Option<String>) -> Self {
        Self {
            argument: argument.map(|argument| Argument::from(argument)),
            stream: Stream::new(),
        }
    }
}

impl SourceReader for Source {
    fn read(&mut self) -> Option<&str> {
        // Always try to read from argument first
        // If argument is `None`, or if read from argument returns `None`, then read from stream
        // Note that `self.argument` cannot then be set to `None`, due to lifetime of returned value
        if let Some(argument) = &mut self.argument {
            if let Some(command) = argument.read() {
                echo_command_prompt(Some(command));
                return Some(command);
            }
        }
        self.stream.read()
    }
}

impl Stream {
    pub fn new() -> Self {
        let stdin = io::stdin();
        if stdin.is_terminal() {
            return Self::Terminal(Terminal::new());
        }
        Self::Stdin(Stdin::from(stdin))
    }
}

impl SourceReader for Stream {
    fn read(&mut self) -> Option<&str> {
        match self {
            Self::Stdin(stdin) => {
                let command = stdin.read();
                echo_command_prompt(command);
                command
            }
            // Don't echo command for terminal source, that would be redundant
            Self::Terminal(terminal) => return terminal.read(),
        }
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
            visible_cursor: 0,
            history: TerminalHistory::new(),
        }
    }

    fn is_next(&self) -> bool {
        debug_assert!(
            self.history.index <= self.history.list.len(),
            "index went past history"
        );
        self.history.index >= self.history.list.len()
    }

    /// Run before modifying `next`
    /// If focused on a historic item, clone it to `next` and update index
    fn update_next(&mut self) {
        if self.is_next() {
            return;
        }
        self.buffer = self
            .history
            .list
            .get(self.history.index)
            .expect("checked above")
            .clone();
        self.history.index = self.history.list.len();
    }

    /// Get next or historic command, from index
    fn get_current(&self) -> &str {
        if self.is_next() {
            &self.buffer
        } else {
            self.history
                .list
                .get(self.history.index)
                .expect("checked above")
        }
    }

    fn print_prompt(&mut self) {
        // Clear line, print prompt, set cursor position
        self.term.clear_line().unwrap();

        // Print prompt and current input
        // Equivalent code found in non-terminal source
        let prompt = "Command: ";
        if Output::is_minimal() {
            write!(&mut self.term, "{}", prompt).unwrap();
        } else {
            write!(&mut self.term, "\x1b[1;34m{}\x1b[0m", prompt).unwrap();
        }

        // Inline `self.get_current()` due to borrowing issues
        let current = if self.is_next() {
            &self.buffer
        } else {
            self.history
                .list
                .get(self.history.index)
                .expect("checked above")
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
                    self.visible_cursor = 0;
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
                if self.history.index > 0 {
                    self.history.index -= 1;
                    self.visible_cursor = self.get_current().len();
                }
            }
            Key::ArrowDown => {
                if self.history.index < self.history.list.len() {
                    self.history.index += 1;
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
            .list
            .last()
            .is_some_and(|previous| previous == &self.buffer)
        {
            self.history.push(self.buffer.clone());
        }
        // Always reset index to next command
        self.history.index = self.history.list.len();
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

impl TerminalHistory {
    const FILE_NAME: &str = "lace-debugger-history";

    pub fn new() -> Self {
        let mut file = Self::get_file();
        let list = Self::read_file(file.as_mut());
        let index = list.len();
        Self { list, index, file }
    }

    pub fn push(&mut self, command: String) {
        if let Some(file) = &mut self.file {
            if writeln!(file, "{}", command).is_err() {
                Self::report_error("Failed to write to file");
            }
        }
        self.list.push(command);
    }

    fn get_file() -> Option<File> {
        let Some(parent_dir) = dirs_next::cache_dir() else {
            Self::report_error(format_args!(
                "Cannot retrieve user cache directory. Eg. $XDG_CACHE_HOME"
            ));
            return None;
        };
        if !parent_dir.is_dir() {
            Self::report_error(format_args!(
                "Parent directory is not a directory: {}",
                parent_dir.display(),
            ));
            return None;
        }

        let file_path = parent_dir.join(Self::FILE_NAME);
        if file_path.exists() && !file_path.is_file() {
            Self::report_error(format_args!(
                "File exists but is not a file: {}",
                file_path.display(),
            ));
            return None;
        }

        match fs::OpenOptions::new()
            .create(true)
            .read(true)
            .append(true)
            .open(&file_path)
        {
            Ok(file) => Some(file),
            Err(_error) => {
                Self::report_error(format_args!("Failed to open file: {}", file_path.display(),));
                None
            }
        }
    }

    fn read_file(file: Option<&mut File>) -> Vec<String> {
        let Some(file) = file else {
            return Vec::new();
        };
        let mut history = Vec::new();
        for line in BufReader::new(file).lines() {
            let Ok(line) = line else {
                Self::report_error("Failed to read from file");
                break;
            };
            history.push(line);
        }
        return history;
    }

    fn report_error(message: impl fmt::Display) {
        dprintln!(
            Always,
            Error,
            "Error with debugger history file: {}",
            message,
        );
    }
}
