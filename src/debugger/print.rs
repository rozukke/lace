use std::io;

use colored::{ColoredString, Colorize as _};

use crate::runtime::terminal_line_start;

pub struct Writer;

impl io::Write for Writer {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // TODO: Make this good
        print(String::from_utf8_lossy(buf).to_string());
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

pub(super) mod is_minimal {
    use std::cell::RefCell;
    thread_local! {
        static VALUE: RefCell<bool> = const { RefCell::new(false) };
    }
    /// May be called multiple times
    pub fn set(new_value: bool) {
        VALUE.with(|value| *value.borrow_mut() = new_value);
    }
    /// May be called before `set_is_minimal`
    pub(super) fn get() -> bool {
        VALUE.with(|value| *value.borrow())
    }
}

pub fn write(f: &mut impl io::Write, string: String) -> Result<(), io::Error> {
    if !is_minimal::get() {
        write!(f, "{}", ColoredString::from(string).blue())?;
        return Ok(());
    }

    let mut chars = string.chars();
    while let Some(ch) = chars.next() {
        // Skip everything between '\x1b' and 'm' (inclusive)
        if ch == '\x1b' {
            while chars.next().is_some_and(|ch| ch != 'm') {}
            continue;
        }
        write!(f, "{}", ch)?;
    }
    Ok(())
}

pub fn print(string: String) {
    let is_line_start = string.chars().next_back() == Some('\n');
    write(&mut io::stderr(), string).expect("write to stderr should not fail");
    terminal_line_start::set(is_line_start);
}

#[macro_export]
macro_rules! dwrite {
    ( $f:expr, $fmt:literal $($tt:tt)* ) => {{
        crate::debugger::_write($f, format!($fmt $($tt)*))
    }};
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
