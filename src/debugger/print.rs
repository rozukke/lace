use colored::{ColoredString, Colorize as _};

use crate::runtime::terminal_cursor;

pub const DEBUGGER_COLOR: u8 = 34;

pub fn print(string: String) {
    let minimal = false;

    let is_line_start = string.chars().next_back() == Some('\n');

    if minimal {
        let mut chars = string.chars();
        while let Some(ch) = chars.next() {
            if ch == '\x1b' {
                while chars.next().is_some_and(|ch| ch != 'm') {}
                continue;
            }
            eprint!("{}", ch);
        }
    } else {
        eprint!("{}", ColoredString::from(string).blue());
    }

    terminal_cursor::set_line_start(is_line_start);
}

#[macro_export]
macro_rules! dprint {
    ( $fmt:literal $($tt:tt)* ) => {{
        crate::debugger::_print(format!($fmt $($tt)*));
    }};
}

#[macro_export]
macro_rules! dprintln {
    () => {{
        crate::debugger::_print("\n".to_string());
    }};
    ( $fmt:literal $($tt:tt)* ) => {{
        crate::debugger::_print(format!(concat!($fmt, "\n") $($tt)*));
    }};
}
