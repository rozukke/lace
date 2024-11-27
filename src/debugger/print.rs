use std::cell::RefCell;

use colored::{ColoredString, Colorize as _};

use crate::runtime::terminal_cursor;

pub const DEBUGGER_COLOR: u8 = 34;

thread_local! {
    static IS_MINIMAL: RefCell<Option<bool>> = const { RefCell::new(None) };
}

pub fn set_is_minimal(value: bool) {
    IS_MINIMAL.with(|minimal| {
        let mut minimal = minimal.borrow_mut();
        debug_assert!(
            minimal.is_none(),
            "tried to initialize IS_MINIMAL multiple times"
        );
        *minimal = Some(value)
    });
}

fn is_minimal() -> bool {
    IS_MINIMAL.with(|minimal| {
        let minimal = minimal.borrow();
        minimal.unwrap_or_else(|| {
            panic!("tried to access IS_MINIMAL before initialization");
        })
    })
}

pub fn print(string: String) {
    let is_line_start = string.chars().next_back() == Some('\n');

    if is_minimal() {
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
