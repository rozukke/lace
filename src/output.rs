use std::cell::RefCell;
use std::fmt::{self, Write as _};

use crate::runtime::RunState;

#[macro_export]
macro_rules! print_char {
    ( $ch:expr ) => {{
        crate::output::Output::Normal.print_char($ch);
    }};
}

#[macro_export]
macro_rules! dprint {
    ( $condition:expr, $category:expr, $fmt:expr $(, $($tt:tt)* )? ) => {{
        #[allow(unused_imports)]
        use crate::output::{Condition::*, Category::*};
        let output = crate::output::Output::Debugger($condition);
        output.print_category($category);
        output.print_fmt(format_args!(
            $fmt $(, $($tt)* )?
        ));
    }};

    // Trigger type error if missing condition/kind
    ( $fmt:literal $($tt:tt)* ) => {{
        crate::output::Output::Debugger($fmt);
    }};
}

#[macro_export]
macro_rules! dprintln {
    ( $condition:expr ) => {{
        #[allow(unused_imports)]
        crate::dprint!(
            $condition,
            crate::output::Category::Normal,
            "\n"
        );
    }};

    ( $condition:expr, $category:expr, $fmt:expr $(, $($tt:tt)* )? ) => {{
        crate::dprint!(
            $condition,
            $category,
            concat!($fmt, "\n")
            $(,$($tt)*)?
        );
    }};

    // Let `dprint` issue any other compiler errors
    ( $($tt:tt)* ) => {{
        crate::dprint!($($tt)*);
    }};
}

const DEBUGGER_COLOR: &str = "34";

#[derive(Clone, Copy, Debug)]
pub enum Output {
    Normal,
    Debugger(Condition),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Condition {
    Always,
    Sometimes,
}

#[derive(Clone, Copy, Debug)]
pub enum Category {
    Normal,
    Info,
    Warning,
    Error,
}

impl Output {
    thread_local! {
        static IS_MINIMAL: RefCell<bool> = const { RefCell::new(false) };
    }
    pub fn is_minimal() -> bool {
        Self::IS_MINIMAL.with(|value| *value.borrow())
    }
    pub fn set_minimal(new_value: bool) -> bool {
        Self::IS_MINIMAL.with(|value| value.replace(new_value))
    }

    pub fn start_new_line(&self) {
        if !LineTracker::is_line_start() {
            self.print_char('\n');
        }
    }

    pub fn print_char(&self, ch: char) {
        self.print_fmt(format_args!("{}", ch))
    }
    pub fn print_str(&self, string: &str) {
        self.print_fmt(format_args!("{}", string))
    }
    pub fn print_decimal(&self, value: u16) {
        self.print_fmt(format_args!("{}", value as i16));
    }

    pub fn print_fmt(&self, args: fmt::Arguments) {
        let minimal = Self::is_minimal();
        match self {
            Self::Normal => {
                NormalWriter { minimal }.write_fmt(args).unwrap();
                LineTracker.write_fmt(args).unwrap();
            }
            Self::Debugger(condition) => {
                if condition == &Condition::Sometimes && minimal {
                    return;
                }
                DebuggerWriter { minimal }.write_fmt(args).unwrap();
                LineTracker.write_fmt(args).unwrap();
            }
        }
    }

    pub fn print_category(&self, category: Category) {
        assert!(
            matches!(self, Self::Debugger(_)),
            "`Output::print_category()` called on `Output::Normal`"
        );

        match category {
            Category::Normal => (),
            Category::Info => self.print_str("  · "),
            Category::Warning => self.print_str("  * "),
            Category::Error => self.print_str("  ~ "),
        }
    }

    pub fn print_registers(&self, state: &RunState) {
        if Self::is_minimal() {
            for i in 0..8 {
                self.print_fmt(format_args!("R{} {}\n", i, state.reg(i)));
            }
            self.print_fmt(format_args!("PC {}\n", state.pc()));
            self.print_fmt(format_args!("CC {:03b}\n", state.flag() as u8));
            return;
        }

        self.print_str("\x1b[2m┌────────────────────────────────────┐\x1b[0m\n");
        self.print_str(
            "\x1b[2m│        \x1b[3mhex     int    uint    char\x1b[0m\x1b[2m │\x1b[0m\n",
        );
        for i in 0..8 {
            self.print_fmt(format_args!("\x1b[2m│\x1b[0m"));
            self.print_fmt(format_args!(" \x1b[1mR{}\x1b[0m  ", i));
            self.print_integer(state.reg(i));
            self.print_str(" \x1b[2m│\x1b[0m\n");
        }
        self.print_fmt(format_args!("\x1b[2m│\x1b[0m"));
        self.print_fmt(format_args!(" \x1b[1mPC\x1b[0m  0x{:04x}", state.pc()));
        self.print_fmt(format_args!("                "));
        self.print_fmt(format_args!(
            " \x1b[1mCC\x1b[0m  {:03b}",
            state.flag() as u8
        ));
        self.print_str(" \x1b[2m│\x1b[0m\n");
        self.print_str("\x1b[2m└────────────────────────────────────┘\x1b[0m\n");
    }

    pub fn print_integer(&self, value: u16) {
        if Self::is_minimal() {
            self.print_decimal(value);
            return;
        }
        self.print_fmt(format_args!("0x{:04x}  ", value));
        self.print_fmt(format_args!("{:-6}  ", value));
        self.print_fmt(format_args!("{:-6}  ", value as i16));
        self.print_char_display(value);
    }

    fn print_char_display(&self, value: u16) {
        debug_assert!(
            !Self::is_minimal(),
            "`print_char_display` should not be called if `--minimal`"
        );
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
            0x21..=0x7e => self.print_fmt(format_args!("{:-6}", value as u8 as char)),

            // Any ASCII character not already matched (unimportant control characters)
            0x00..=0x7f => self.print_str("\x1b[2m───\x1b[0m"),
            // Any non-ASCII character
            0x0080.. => self.print_str("\x1b[2m┄┄┄\x1b[0m"),
        }
    }
}

struct NormalWriter {
    minimal: bool,
}
impl fmt::Write for NormalWriter {
    fn write_str(&mut self, string: &str) -> fmt::Result {
        if self.minimal {
            print!("{}", Decolored::new(string));
        } else {
            print!("{}", string);
        }
        Ok(())
    }
}

struct DebuggerWriter {
    minimal: bool,
}
impl fmt::Write for DebuggerWriter {
    fn write_str(&mut self, string: &str) -> fmt::Result {
        if self.minimal {
            print!("{}", Decolored::new(string));
        } else {
            eprint!("\x1b[{}m", DEBUGGER_COLOR);
            eprint!("{}", Colored::new(DEBUGGER_COLOR, string));
            eprint!("\x1b[0m");
        }
        Ok(())
    }
}

struct LineTracker;
impl LineTracker {
    thread_local! {
        static IS_LINE_START: RefCell<bool> = const { RefCell::new(true) };
    }
    pub fn is_line_start() -> bool {
        Self::IS_LINE_START.with(|value| *value.borrow())
    }
    fn set_line_start(new_value: bool) -> bool {
        Self::IS_LINE_START.with(|value| value.replace(new_value))
    }
}
impl fmt::Write for LineTracker {
    fn write_str(&mut self, string: &str) -> fmt::Result {
        for ch in string.chars().rev() {
            let is_line_start = match ch {
                '\n' | '\r' => true,
                '\x00'..='\x1f' | '\x7f' => continue,
                _ => false,
            };
            Self::set_line_start(is_line_start);
            break;
        }

        Ok(())
    }
}

struct Colored<'a> {
    color: &'static str,
    string: &'a str,
}
impl<'a> Colored<'a> {
    pub fn new(color: &'static str, string: &'a str) -> Self {
        Self { color, string }
    }
}
impl<'a> fmt::Display for Colored<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("\x1b[")?;
        f.write_str(self.color)?;
        f.write_str("m")?;

        let mut chars = self.string.chars();
        while let Some(ch) = chars.next() {
            // Print color code in string -- everything between '\x1b' and 'm' (inclusive)
            // Re-apply global color
            if ch == '\x1b' {
                f.write_char('\x1b')?;
                while let Some(ch) = chars.next() {
                    f.write_char(ch)?;
                    if ch == 'm' {
                        break;
                    }
                }
                f.write_str("\x1b[")?;
                f.write_str(self.color)?;
                f.write_str("m")?;
                continue;
            }

            f.write_char(ch)?;
        }

        f.write_str("\x1b[0m")?;

        Ok(())
    }
}

struct Decolored<'a> {
    string: &'a str,
}
impl<'a> Decolored<'a> {
    pub fn new(string: &'a str) -> Self {
        Self { string }
    }
}
impl<'a> fmt::Display for Decolored<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut chars = self.string.chars();
        while let Some(ch) = chars.next() {
            // Skip everything between '\x1b' and 'm' (inclusive)
            if ch == '\x1b' {
                while chars.next().is_some_and(|ch| ch != 'm') {}
                continue;
            }
            f.write_char(ch)?;
        }
        Ok(())
    }
}

