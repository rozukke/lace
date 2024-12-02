use colored::{ColoredString, Colorize};

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

pub mod is_minimal {
    use std::cell::RefCell;
    thread_local! {
        static VALUE: RefCell<bool> = const { RefCell::new(false) };
    }
    /// May be called multiple times
    pub fn set(new_value: bool) {
        VALUE.with(|value| *value.borrow_mut() = new_value);
    }
    /// May be called before `set_is_minimal`
    pub fn get() -> bool {
        VALUE.with(|value| *value.borrow())
    }
}

pub mod terminal_line_start {
    use std::cell::RefCell;
    thread_local! {
        static VALUE: RefCell<bool> = const { RefCell::new(true) };
    }
    pub fn set(new_value: bool) {
        VALUE.with(|value| *value.borrow_mut() = new_value);
    }
    pub fn get() -> bool {
        VALUE.with(|value| *value.borrow())
    }
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

impl Output {
    pub fn print_char(&self, ch: char) {
        match self {
            Self::Normal => {
                print!("{}", ch)
            }
            Self::Debugger { .. } => {
                unimplemented!("`print_char()` called on `Output::Debugger`");
            }
        }
        terminal_line_start::set(ch == '\n');
    }

    pub fn print_str(&self, string: &str) {
        match self {
            Self::Normal => {
                print!("{}", string);
                set_line_start(string);
            }

            Self::Debugger(condition) => match (is_minimal::get(), *condition) {
                (false, _) => {
                    eprint!("{}", ColoredString::from(string).blue());
                    set_line_start(string);
                }
                (true, Condition::Always) => {
                    eprint_colorless(string);
                    set_line_start(string);
                }
                (true, Condition::Sometimes) => (),
            },
        }
    }
}

// TODO: Add tests for `Decolored`
struct Decolored<'a> {
    chars: std::str::Chars<'a>,
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

fn set_line_start(string: &str) {
    let last = Decolored::new(string).last();
    if let Some(ch) = last {
        terminal_line_start::set(ch == '\n');
    }
}

fn eprint_colorless(string: &str) {
    for ch in Decolored::new(string) {
        eprint!("{}", ch);
    }
}
