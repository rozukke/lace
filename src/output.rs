use std::{cell::RefCell, str::Chars};

use colored::{ColoredString, Colorize};

use crate::runtime::RunState;

#[macro_export]
macro_rules! print_char {
    ( $ch:expr ) => {{
        crate::output::Output::Normal.print_char($ch);
    }};
}

#[macro_export]
macro_rules! dprint {
    ( $cond:expr, $fmt:literal $($tt:tt)* ) => {{
        use crate::output::Condition::*;
        let s = format!(
            $fmt
            $($tt)*
        );
        crate::output::Output::Debugger($cond).print_str(&s);
    }};
}

#[macro_export]
macro_rules! dprintln {
    ( $cond:expr ) => {{
        use crate::output::Condition::*;
        crate::output::Output::Debugger($cond).print_str("\n");
    }};
    ( $cond:expr, $fmt:literal $($tt:tt)* ) => {{
        use crate::output::Condition::*;
        let s = format!(
            concat!($fmt, "\n")
            $($tt)*
        );
        crate::output::Output::Debugger($cond).print_str(&s);
    }};
}

#[derive(Clone, Copy, Debug)]
pub enum Output {
    Normal,
    Debugger(Condition),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Condition {
    Always,
    Sometimes,
}

struct Decolored<'a> {
    chars: Chars<'a>,
}

// TODO(opt): Don't use `format!`. Possibly `format_args!` or `impl Write`
impl Output {
    thread_local! {
        static IS_LINE_START: RefCell<bool> = const { RefCell::new(true) };
        static IS_DEBUGGER_MINIMAL: RefCell<bool> = const { RefCell::new(false) };
    }

    pub fn set_line_start(new_value: bool) -> bool {
        Self::IS_LINE_START.with(|value| value.replace(new_value))
    }
    /// Private. Use [`Output::start_new_line`].
    fn is_line_start() -> bool {
        Self::IS_LINE_START.with(|value| *value.borrow())
    }
    pub fn set_debugger_minimal(new_value: bool) -> bool {
        Self::IS_DEBUGGER_MINIMAL.with(|value| value.replace(new_value))
    }
    pub fn is_debugger_minimal() -> bool {
        Self::IS_DEBUGGER_MINIMAL.with(|value| *value.borrow())
    }

    fn set_line_start_from_char(ch: char) {
        Output::set_line_start(ch == '\n');
    }
    fn set_line_start_from_str(string: &str) {
        let last = Decolored::new(string).last();
        if let Some(ch) = last {
            Output::set_line_start(ch == '\n');
        }
    }

    pub fn print_char(&self, ch: char) {
        match self {
            Self::Normal => {
                print!("{}", ch)
            }
            Self::Debugger { .. } => {
                unimplemented!("`print_char()` called on `Output::Debugger`");
            }
        }
        Self::set_line_start_from_char(ch);
    }

    pub fn print_str(&self, string: &str) {
        match self {
            Self::Normal => {
                print!("{}", string);
                Self::set_line_start_from_str(string);
            }

            Self::Debugger(condition) => match (Self::is_debugger_minimal(), *condition) {
                (false, _) => {
                    eprint!("{}", ColoredString::from(string).blue());
                    Self::set_line_start_from_str(string);
                }
                (true, Condition::Always) => {
                    eprint_colorless(string);
                    Self::set_line_start_from_str(string);
                }
                (true, Condition::Sometimes) => (),
            },
        }
    }

    pub fn start_new_line(&self) {
        if !Self::is_line_start() {
            self.print_char('\n');
        }
    }

    pub fn print_registers(&self, state: &RunState) {
        self.print_str("\x1b[2m┌────────────────────────────────────┐\x1b[0m\n");
        self.print_str(
            "\x1b[2m│        \x1b[3mhex     int    uint    char\x1b[0m\x1b[2m │\x1b[0m\n",
        );
        for i in 0..8 {
            self.print_str(&format!("\x1b[2m│\x1b[0m R{}  ", i));
            self.print_integer(state.reg(i));
            self.print_str(" \x1b[2m│\x1b[0m\n");
        }
        self.print_str("\x1b[2m└────────────────────────────────────┘\x1b[0m\n");
    }

    pub fn print_decimal(&self, value: u16) {
        self.print_str(&format!("{}", value as i16));
    }

    pub fn print_integer(&self, value: u16) {
        self.print_str(&format!("0x{:04x}  ", value));
        self.print_str(&format!("{:-6}  ", value));
        self.print_str(&format!("{:-6}  ", value as i16));
        self.print_char_display(value);
    }

    fn print_char_display(&self, value: u16) {
        self.print_str("   ");
        // Print 3 characters
        match value {
            // ASCII control characters which are arbitrarily considered significant
            0x00 => self.print_str("NUL"),
            0x08 => self.print_str("BS "),
            0x09 => self.print_str("HT "),
            0x0a => self.print_str("LF "),
            0x0b => self.print_str("VT "),
            0x0c => self.print_str("FF "),
            0x0d => self.print_str("CR "),
            0x1b => self.print_str("ESC"),
            0x7f => self.print_str("DEL"),

            // Space
            0x20 => self.print_str("[_]"),

            // Printable ASCII characters
            0x21..=0x7e => self.print_str(&format!("{:-6}", value as u8 as char)),

            // Any ASCII character not already matched (unimportant control characters)
            0x00..=0x7f => self.print_str("\x1b[2m:::\x1b[0m"),
            // Any non-ASCII character
            0x0080.. => self.print_str("\x1b[2m···\x1b[0m"),
        }
    }
}

impl<'a> Decolored<'a> {
    pub fn new(string: &'a str) -> Self {
        Self {
            chars: string.chars(),
        }
    }
}

impl<'a> Iterator for Decolored<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(ch) = self.chars.next() {
            // Skip everything between '\x1b' and 'm' (inclusive)
            if ch == '\x1b' {
                while self.chars.next().is_some_and(|ch| ch != 'm') {}
                continue;
            }
            return Some(ch);
        }
        return None;
    }
}

fn eprint_colorless(string: &str) {
    for ch in Decolored::new(string) {
        eprint!("{}", ch);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decolored() {
        assert_eq!(Decolored::new("abcdef").collect::<String>(), "abcdef");
        assert_eq!(
            Decolored::new("abc\x1b[0;2mdef\x1b[0m").collect::<String>(),
            "abcdef"
        );
        assert_eq!(Decolored::new("abc\x1b[0xyz").collect::<String>(), "abc");
        assert_eq!(
            Decolored::new("abc\x1bw[0bxyzmdef").collect::<String>(),
            "abcdef"
        );
    }
}
