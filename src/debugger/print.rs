use std::cell::RefCell;
use std::io;

use colored::{ColoredString, Colorize as _};

use crate::runtime::terminal_cursor;

thread_local! {
    static IS_MINIMAL: RefCell<bool> = const { RefCell::new(false) };
}
pub fn set_is_minimal(value: bool) {
    IS_MINIMAL.with(|minimal| *minimal.borrow_mut() = value);
}
fn is_minimal() -> bool {
    IS_MINIMAL.with(|minimal| *minimal.borrow())
}

pub fn write(f: &mut impl io::Write, string: String) -> Result<(), io::Error> {
    if is_minimal() {
        let mut chars = string.chars();
        while let Some(ch) = chars.next() {
            if ch == '\x1b' {
                while chars.next().is_some_and(|ch| ch != 'm') {}
                continue;
            }
            write!(f, "{}", ch)?;
        }
    } else {
        write!(f, "{}", ColoredString::from(string).blue())?;
    }
    Ok(())
}

pub fn print(string: String) {
    let is_line_start = string.chars().next_back() == Some('\n');
    write(&mut io::stderr(), string).expect("write to stderr should not fail");
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

#[macro_export]
macro_rules! dwrite {
    ( $f:expr, $fmt:literal $($tt:tt)* ) => {{
        crate::debugger::_write($f, format!($fmt $($tt)*))
    }};
}