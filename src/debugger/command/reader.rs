use std::fmt;
use std::fs::{self, File};
use std::io::{self, BufRead as _, BufReader, IsTerminal as _, Read as _, Write as _};

use crossterm::event::{self, Event, KeyEvent};
use crossterm::{cursor, execute, terminal};

use crate::output::debugger_colors;
use crate::{dprint, dprintln, output::Output};

// TODO(feat): UTF-8 support for `Stdin`

/// Read from argument first, if `Some`. Then read from stream.
#[derive(Debug)]
pub struct CommandReader {
    argument: Option<Argument>,
    stream: Stream,
}

/// Command-line argument.
#[derive(Debug)]
struct Argument {
    buffer: String,
    /// Byte index.
    cursor: usize,
}

/// Stdin or interactive terminal.
#[derive(Debug)]
enum Stream {
    Stdin(Stdin),
    Terminal(Terminal),
}

/// Stdin which is not attached to a terminal, i.e. piped.
#[derive(Debug)]
struct Stdin {
    stdin: io::Stdin,
    /// Command must be stored somewhere to be referenced.
    buffer: String,
}

/// Interactive unbuffered terminal.
// TODO(feat): Support CTRL+Arrow keybinds
#[derive(Debug)]
struct Terminal {
    stderr: io::Stderr,
    buffer: String,
    /// Byte index.
    cursor: usize,
    /// Visible line cursor in terminal (char index, not byte index).
    visible_cursor: usize,
    /// History list and file.
    history: TerminalHistory,
}

/// All history information for `Terminal`.
#[derive(Debug)]
struct TerminalHistory {
    list: Vec<String>,
    /// Focused item in history, or new entry if index==length.
    index: usize,
    /// `None` indicates failure to open file.
    file: Option<File>,
}

/// Must be ASCII to ensure `.len() == .chars().count()`
const PROMPT: &str = "DEBUGGER> ";

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

/// A trait for objects which can yield a command, by iterating a string or reading a file.
pub trait Read {
    /// `None` indicates EOF.
    /// Returned string slice MAY include leading or trailing whitespace.
    fn read(&mut self) -> Option<&str>;
}

impl CommandReader {
    pub fn from(argument: Option<String>) -> Self {
        Self {
            argument: argument.map(Argument::from),
            stream: Stream::new(),
        }
    }
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

impl Argument {
    pub fn from(source: String) -> Self {
        Self {
            buffer: source,
            cursor: 0,
        }
    }
}

impl Read for Argument {
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

impl Stdin {
    pub fn from(stdin: io::Stdin) -> Self {
        Self {
            stdin,
            buffer: String::new(),
        }
    }

    /// `None` indicates EOF.
    fn read_char(&mut self) -> Option<char> {
        let mut buffer = [0; 1];
        let bytes_read = self
            .stdin
            .read(&mut buffer)
            .expect("failed to read character from stdin");
        if bytes_read == 0 {
            return None;
        }
        Some(buffer[0] as char)
    }
}

impl Read for Stdin {
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

#[derive(Debug)]
enum Key {
    CtrlC,
    Enter,
    Backspace,
    Delete,
    Left,
    Right,
    Up,
    Down,
    CtrlLeft,
    CtrlRight,
    Char(char),
}

impl TryFrom<Event> for Key {
    type Error = ();

    fn try_from(event: Event) -> Result<Self, Self::Error> {
        if let Event::Key(event) = event {
            if let Ok(key) = event.try_into() {
                return Ok(key);
            }
        }
        Err(())
    }
}

impl TryFrom<KeyEvent> for Key {
    type Error = ();

    fn try_from(event: KeyEvent) -> Result<Self, Self::Error> {
        use event::{KeyCode, KeyEventKind, KeyModifiers as Mod};

        if matches!(event.kind, KeyEventKind::Release) {
            return Err(());
        }

        let key = match (event.modifiers, event.code) {
            // Ctrl+C
            (Mod::CONTROL, KeyCode::Char('c')) => Key::CtrlC,

            // Backspace, Delete, Enter
            (_, KeyCode::Backspace) => Key::Backspace,
            (_, KeyCode::Delete) => Key::Delete,
            (_, KeyCode::Enter) | (_, KeyCode::Char('\n')) => Key::Enter,

            // Arrow keys
            (Mod::NONE, KeyCode::Left) => Key::Left,
            (Mod::NONE, KeyCode::Right) => Key::Right,
            (Mod::NONE, KeyCode::Up) => Key::Up,
            (Mod::NONE, KeyCode::Down) => Key::Down,

            // Ctrl + Arrow keys
            (Mod::CONTROL, KeyCode::Left) => Key::CtrlLeft,
            (Mod::CONTROL, KeyCode::Right) => Key::CtrlRight,

            // Normal character
            (Mod::NONE | Mod::SHIFT, KeyCode::Char(ch)) => Key::Char(ch),

            _ => return Err(()),
        };

        Ok(key)
    }
}

impl Terminal {
    pub fn new() -> Self {
        Self {
            stderr: io::stderr(),
            buffer: String::new(),
            cursor: 0,
            visible_cursor: 0,
            history: TerminalHistory::new(),
        }
    }

    /// Returns `true` if current command is a new command, rather than a focused history item.
    fn is_next(&self) -> bool {
        debug_assert!(
            self.history.index <= self.history.list.len(),
            "index went past history"
        );
        self.history.index >= self.history.list.len()
    }

    /// Run before modifying `next`.
    /// If focused on a historic item, clone it to `next` and update index.
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

    /// Get next or historic command, from index.
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

    /// Clear current line, draw REPL prompt and current input, and set cursor position.
    fn print_prompt(&mut self) {
        // Don't use `dprint(ln)!` in this function: we already have a handle to `stderr` and want
        // to have control over conditions and attributes for printing.

        // Equivalent to `write!(... "\r")`
        execute!(
            self.stderr,
            terminal::Clear(terminal::ClearType::CurrentLine),
            cursor::MoveToColumn(0),
        )
        .expect("failed to clear line and move cursor");

        // Print prompt and current input
        // Equivalent code found in non-terminal source
        if Output::is_minimal() {
            write!(&self.stderr, "{}", PROMPT)
        } else {
            write!(
                &self.stderr,
                "\x1b[1;{}m{}\x1b[0m",
                debugger_colors::PRIMARY,
                PROMPT
            )
        }
        .expect("failed to print debugger prompt");

        // Print current input
        // Inline `self.get_current()` due to borrowing issues
        let current = if self.is_next() {
            &self.buffer
        } else {
            self.history
                .list
                .get(self.history.index)
                .expect("checked above")
        };
        write!(self.stderr, "{}", current).expect("failed to print debugger input");

        // Set final cursor position
        execute!(
            self.stderr,
            cursor::MoveToColumn((PROMPT.len() + self.visible_cursor) as u16),
        )
        .expect("failed to move cursor");

        // Previous `execute!` call flushed output already
    }

    // Returns `None` to indicate `Ctrl+C` which must be handled explicitly by caller.
    //
    // Returns `Some(true)` indicates to break loop (EOL). Only occurs on `Key::Enter` when buffer
    // is non-empty.
    fn handle_key(&mut self, key: Key) -> Option<bool> {
        match key {
            Key::CtrlC => return None,

            Key::Enter => {
                if self.is_next() && self.buffer.trim().is_empty() {
                    self.buffer.clear();
                    self.visible_cursor = 0;
                    println!();
                } else {
                    self.update_next();
                    return Some(true);
                }
            }

            Key::Char(ch) => match ch {
                // Ignore ASCII control characters
                '\x00'..='\x1f' | '\x7f' => (),

                // Pasting should be automatically supported, since terminals simulate typing each
                // character
                _ => {
                    self.update_next();
                    insert_char_index(&mut self.buffer, self.visible_cursor, ch);
                    self.visible_cursor += 1;
                }
            },

            Key::Backspace => {
                self.update_next();
                if self.visible_cursor > 0
                    && self.visible_cursor <= self.get_current().chars().count()
                {
                    self.visible_cursor -= 1;
                    remove_char_index(&mut self.buffer, self.visible_cursor);
                }
            }
            Key::Delete => {
                self.update_next();
                if self.visible_cursor < self.get_current().chars().count() {
                    remove_char_index(&mut self.buffer, self.visible_cursor);
                }
            }

            // Left/right single character in input
            Key::Left => {
                if self.visible_cursor > 0 {
                    self.visible_cursor -= 1;
                }
            }
            Key::Right => {
                if self.visible_cursor < self.get_current().chars().count() {
                    self.visible_cursor += 1;
                }
            }

            // Left/right entire word in input
            // TODO(feat): Ctrl + Arrow keys
            Key::CtrlLeft => {
                println!("\t\tunimplemented: CtrlLeft");
            }
            Key::CtrlRight => {
                println!("\t\tunimplemented: CtrlRight");
            }

            // Back/forth through history
            Key::Up => {
                if self.history.index > 0 {
                    self.history.index -= 1;
                    self.visible_cursor = self.get_current().chars().count();
                }
            }
            Key::Down => {
                if self.history.index < self.history.list.len() {
                    self.history.index += 1;
                    self.visible_cursor = self.get_current().chars().count();
                }
            }
        }
        Some(false)
    }

    /// Read terminal events until key event is read as a valid [`Key`].
    ///
    /// Caller must ensure terminal is in raw mode.
    fn read_key() -> Key {
        assert!(
            terminal::is_raw_mode_enabled().is_ok_and(|is| is),
            "terminal must be in raw mode to read key",
        );
        let key = loop {
            let event = event::read().expect("failed to read terminal event");
            if let Ok(key) = event.try_into() {
                break key;
            }
        };
        key
    }

    /// Read keys until newline.
    ///
    /// Exit process on `Ctrl+C`.
    fn read_line_raw(&mut self) {
        debug_assert!(
            !terminal::is_raw_mode_enabled().is_ok_and(|is| is),
            "terminal should not be in raw mode before line is read",
        );
        terminal::enable_raw_mode().expect("failed to enable raw terminal");

        // Loop must `break`, not `return` to ensure cleanup -- hence flag variable
        let should_exit = loop {
            // Technically redrawing of prompt could be avoided, but this method makes it much
            // simpler and less error-prone
            self.print_prompt();
            let key = Self::read_key();
            match self.handle_key(key) {
                Some(false) => continue,
                Some(true) => break false, // EOL
                None => break true,        // Ctrl+C
            }
        };

        terminal::disable_raw_mode().expect("failed to disable raw terminal");
        println!();

        // `Ctrl+C` is read as a normal key event so must be handled explicitely
        if should_exit {
            std::process::exit(0)
        }
    }

    /// Read entire (multi-command) line from terminal.
    fn read_line(&mut self) {
        self.buffer.clear();
        self.visible_cursor = 0;

        self.read_line_raw();
        debug_assert!(
            !self.buffer.trim().is_empty(),
            "should have read characters until non-empty"
        );

        // Push to history if different to last command
        if self
            .history
            .list
            .last()
            .is_none_or(|previous| previous != &self.buffer)
        {
            self.history.push(self.buffer.clone());
        }
        // Always reset index to next command
        self.history.index = self.history.list.len();
    }

    /// Returns next command from line buffer.
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
                rest
            }
        }
    }
}

impl Read for Terminal {
    fn read(&mut self) -> Option<&str> {
        // Reached end of line buffer: read new line
        if self.cursor == 0 {
            self.read_line();
        }
        Some(self.get_next_command())
    }
}

/// Insert a character at a character index.
fn insert_char_index(string: &mut String, char_index: usize, ch: char) {
    let (byte_index, char_count) = count_chars_bytes(string, char_index);
    assert!(char_index <= char_count, "out-of-bounds char index");
    string.insert(byte_index, ch)
}
/// Remove a character at a character index.
fn remove_char_index(string: &mut String, char_index: usize) -> char {
    let (byte_index, char_count) = count_chars_bytes(string, char_index);
    assert!(char_index < char_count, "out-of-bounds char index");
    string.remove(byte_index)
}
/// Returns the byte index from a character index, and the total character count.
fn count_chars_bytes(string: &str, char_index: usize) -> (usize, usize) {
    let mut byte_index = string.len();
    let mut char_count = 0;
    for (i, (j, _)) in string.char_indices().enumerate() {
        if i == char_index {
            byte_index = j;
        }
        char_count += 1;
    }
    (byte_index, char_count)
}

impl TerminalHistory {
    const FILE_NAME: &str = "lace-debugger-history";

    pub fn new() -> Self {
        let mut file = Self::get_file();
        let list = Self::read_file(file.as_mut());
        let index = list.len();
        Self { list, index, file }
    }

    /// Push command into list and write to file.
    pub fn push(&mut self, command: String) {
        if let Some(file) = &mut self.file {
            if writeln!(file, "{}", command).is_err() {
                Self::report_error("Failed to write to file");
            }
        }
        self.list.push(command);
    }

    /// Returns empty vector if failed to read.
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
        history
    }

    /// Get file path and open file.
    ///
    /// Returns `None` if anything fails.
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
                Self::report_error(format_args!("Failed to open file: {}", file_path.display()));
                None
            }
        }
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
