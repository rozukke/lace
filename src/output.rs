use std::cell::RefCell;
use std::fmt::{self, Write as _};

use crate::runtime::RunState;

/// Main color used by [`Output::Debugger`].
///
/// Note that color depends on the [`Category`] used, and can be overridden.
pub const DEBUGGER_PRIMARY_COLOR: &str = "34";

/// Print to [`Output::Debugger`].
#[macro_export]
macro_rules! dprint {
    // `$fmt:expr` is required to allow `concat!` macro to be accepted
    ( $condition:expr, $category:expr, $fmt:expr $(, $($tt:tt)* )? ) => {{
        // This is not very hygenic. But makes macro more ergonomic to use.
        #[allow(unused_imports)]
        use $crate::output::{Condition::*, Category::*};

        $crate::output::Output::Debugger($crate::output::Condition::Sometimes, $category)
            .print_category($category);
        $crate::output::Output::Debugger($condition, $category)
            .print(format_args!($fmt $(, $($tt)* )?)
        );
        eprint!("\x1b[0m"); // This is not ideal here
    }};

    ( $fmt:literal $($tt:tt)* ) => {{
        compile_error!("requires condition and category arguments");
    }};
    ( $condition:expr $(, $fmt:literal $($tt:tt)* )? ) => {{
        compile_error!("requires category arguments");
    }};
    ( $( $condition:expr )? $(, $category:expr )? $(,)? ) => {{
        format!()
    }};
}

/// Print to [`Output::Debugger`], with a newline.
#[macro_export]
macro_rules! dprintln {
    ( $condition:expr $(,)? ) => {{
        $crate::dprint!(
            $condition,
            $crate::output::Category::Normal,
            "\n"
        );
    }};

    ( $condition:expr, $category:expr, $fmt:expr $(, $($tt:tt)* )? ) => {{
        $crate::dprint!(
            $condition,
            $category,
            concat!($fmt, "\n")
            $(, $($tt)* )?
        );
    }};

    ( $(,)? ) => {{
        compile_error!("requires condition argument");
    }};
    ( $condition:expr, $category:literal $(,)? ) => {{
        compile_error!("requires format string if category argument is present\
            \neither remove category argument or include format string");
    }};
    // Let `dprint` issue any other compiler errors
    ( $($tt:tt)* ) => {{
        $crate::dprint!($($tt)*);
    }};
}

/// Output channel.
#[derive(Clone, Copy, Debug)]
pub enum Output {
    /// For program output.
    /// Writes to `stdout`.
    /// No ANSI color/style attributes are applied.
    Normal,
    /// For debugger output.
    /// Writes to `stderr`.
    /// ANSI color/style attributes, and/or line decorations, will be applied depending on [`Category`].
    /// Whether or not anything is printed depends on the [`Condition`].
    Debugger(Condition, Category),
}

/// A condition of `Sometimes` will not print anything if `Output::is_minimal() == true`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Condition {
    /// Always print.
    Always,
    /// Only print if `Output::minimal() == false`.
    Sometimes,
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub enum Category {
    /// No decoration.
    #[default]
    Normal,
    /// Implies that NO change was made to memory/registers/breakpoints/etc.
    Info,
    /// Implies that a change was made to memory/registers/breakpoints/etc.
    Warning,
    /// An error occurred while parsing or executing a command.
    Error,
    /// Use `{` and `}` to delimit ANSI color/style attributes (instead of `\x1b[` and `m`)
    Special,
}

impl Output {
    thread_local! {
        /// Only access using [`Output::is_minimal`] and [`Output::set_minimal`].
        static IS_MINIMAL: RefCell<bool> = const { RefCell::new(false) };
    }
    /// Whether output willl be printed 'minimally'.
    ///
    /// Should return `true` iff `--minimal` argument was given.
    pub fn is_minimal() -> bool {
        Self::IS_MINIMAL.with(|value| *value.borrow())
    }
    /// Set whether output will be printed 'minimally'.
    ///
    /// Use this method to handle `--minimal` argument.
    pub fn set_minimal(new_value: bool) {
        Self::IS_MINIMAL.with(|value| value.replace(new_value));
    }

    /// If cursor is NOT at the start of a line, then start a new line (ie. print '\n').
    ///
    /// Relies on previously-printed strings to keep track of cursor position. This is done
    /// automatically within the [`Output`] struct, but free [`print!`], [`eprint!`], etc. calls will
    /// not track the state.
    pub fn start_new_line(&self) {
        if !LineTracker::is_line_start() {
            self.print('\n');
        }
    }

    /// Print a value, which implements [`fmt::Display`].
    pub fn print(&self, value: impl fmt::Display) {
        self.print_fmt(format_args!("{}", value));
    }

    /// Print an integer, as a *signed* decimal.
    pub fn print_decimal(&self, value: u16) {
        self.print(format_args!("{}", value as i16));
    }

    /// Print a decoration symbol, to indicate the purpose of the next message printed.
    ///
    /// Only works for [`Output::Debugger`].
    pub fn print_category(&self, category: Category) {
        debug_assert!(
            matches!(self, Self::Debugger(..)),
            "`Output::print_category()` called on `Output::Normal`"
        );
        if !matches!(self, Self::Debugger(..)) {
            return;
        }

        match category {
            Category::Normal => (),
            Category::Info => self.print("  · "),
            Category::Warning => self.print("  ➔ "),
            Category::Error => self.print("  ⨯ "),
            Category::Special => (),
        }
    }

    /// Print all registers (R0-7, PC, and CC) in a fancy table.
    ///
    /// Prints values as hex, signed decimal, unsigned decimal, and character.
    ///
    /// PC and CC will be only displayed as hex and 3-bit binary respectively.
    pub fn print_registers(&self, state: &RunState) {
        if Self::is_minimal() {
            for i in 0..8 {
                self.print(format_args!("R{} {}\n", i, state.reg(i)));
            }
            self.print(format_args!("PC {}\n", state.pc()));
            self.print(format_args!("CC {:03b}\n", state.flag() as u8));
            return;
        }

        self.print("\x1b[2m┌────────────────────────────────────┐\x1b[0m\n");
        self.print("\x1b[2m│        \x1b[3mhex     int    uint    char\x1b[0m\x1b[2m │\x1b[0m\n");

        // R0-7
        for i in 0..8 {
            self.print("\x1b[2m│\x1b[0m");
            self.print(format_args!(" \x1b[1mR\x1b[1m{}\x1b[0m  ", i));
            self.print_integer_inner(state.reg(i));
            self.print(" \x1b[2m│\x1b[0m\n");
        }

        // PC, CC
        self.print("\x1b[2m│\x1b[0m");
        self.print(" \x1b[1mPC\x1b[0m");
        self.print(format_args!("  0x{:04x}", state.pc()));
        self.print("                ");
        self.print(" \x1b[1mCC\x1b[0m");
        self.print(format_args!("  {:03b}", state.flag() as u8));
        self.print(" \x1b[2m│\x1b[0m\n");

        self.print("\x1b[2m└────────────────────────────────────┘\x1b[0m\n");
    }

    /// Prints a register as hex, signed decimal, unsigned decimal, and character, in a fancy
    /// table.
    pub fn print_integer(&self, value: u16) {
        if Self::is_minimal() {
            self.print_decimal(value);
            self.print('\n');
            return;
        }
        self.print("\x1b[2m┌────────────────────────────────┐\x1b[0m\n");
        self.print("\x1b[2m│    \x1b[3mhex     int    uint    char\x1b[0m\x1b[2m │\x1b[0m\n");
        self.print("\x1b[2m│\x1b[0m ");
        self.print_integer_inner(value);
        self.print(" \x1b[2m│\x1b[0m\n");
        self.print("\x1b[2m└────────────────────────────────┘\x1b[0m\n");
    }

    /// Prints a register as hex, signed decimal, unsigned decimal, and character.
    fn print_integer_inner(&self, value: u16) {
        if Self::is_minimal() {
            self.print_decimal(value);
            return;
        }
        self.print(format_args!("0x{:04x}", value));
        self.print(format_args!("  {:-6}", value));
        self.print(format_args!("  {:-6}", value as i16));
        self.print("     ");
        self.print_char_display(value);
    }

    /// Print a character in a descriptive way:
    ///
    /// - 'Significant' control characters display their abbreviated names.
    /// - ASCII space is displayed as `[_]`.
    /// - Printable ASCII characters are displayed normally.
    /// - Any other ASCII character is printed as `───`
    /// - Any non-ASCII (UTF-16) character is displayed as `┄┄┄`
    fn print_char_display(&self, value: u16) {
        debug_assert!(
            !Self::is_minimal(),
            "`print_display` should not be called if `--minimal`"
        );
        if Self::is_minimal() {
            return;
        }

        // Print 3 characters
        match value {
            // ASCII control characters which are arbitrarily considered significant
            0x00 => self.print("NUL"),
            0x08 => self.print("BS "),
            0x09 => self.print("HT "),
            0x0a => self.print("LF "),
            0x0b => self.print("VT "),
            0x0c => self.print("FF "),
            0x0d => self.print("CR "),
            0x1b => self.print("ESC"),
            0x7f => self.print("DEL"),

            // Space
            0x20 => self.print("[_]"),

            // Printable ASCII characters
            0x21..=0x7e => self.print(format_args!("{:-6}", value as u8 as char)),

            // Any ASCII character not already matched (unimportant control characters)
            0x00..=0x7f => self.print("\x1b[2m───\x1b[0m"),
            // Any non-ASCII character
            0x0080.. => self.print("\x1b[2m┄┄┄\x1b[0m"),
        }
    }

    /// Print a value returned by the [`format_args!`] macro.
    ///
    /// Use [`Output::print`] wrapper method.
    fn print_fmt(&self, args: fmt::Arguments) {
        let minimal = Self::is_minimal();
        match self {
            Self::Normal => {
                NormalWriter { minimal }.write_fmt(args).unwrap();
            }
            Self::Debugger(condition, category) => {
                if minimal && condition == &Condition::Sometimes {
                    return;
                }
                DebuggerWriter {
                    minimal,
                    category: *category,
                }
                .write_fmt(args)
                .unwrap();
            }
        }
    }
}

/// Writer for [`Output::Normal`]
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
        LineTracker.write_str(string).unwrap();
        Ok(())
    }
}

/// Writer for [`Output::Debugger`]
///
/// [`Condition`] must be checked by caller.
struct DebuggerWriter {
    minimal: bool,
    category: Category,
}
impl fmt::Write for DebuggerWriter {
    fn write_str(&mut self, string: &str) -> fmt::Result {
        let color = match self.category {
            Category::Normal => DEBUGGER_PRIMARY_COLOR,
            Category::Info => DEBUGGER_PRIMARY_COLOR,
            Category::Warning => "33",
            Category::Error => "31",

            // Special behaviour. Note the return at the end of this branch
            Category::Special => {
                let mut chars = string.chars();

                if self.minimal {
                    // Remove all `{...}`
                    while let Some(ch) = chars.next() {
                        if ch != '{' {
                            eprint!("{}", ch);
                            continue;
                        }
                        for ch in chars.by_ref() {
                            if ch == '}' {
                                break;
                            }
                        }
                    }
                } else {
                    // Replace `{...}` with `\x1b[...m`
                    // Acts similar to `Colored::fmt`
                    eprint!("\x1b[{}m", DEBUGGER_PRIMARY_COLOR);
                    while let Some(ch) = chars.next() {
                        if ch != '{' {
                            eprint!("{}", ch);
                            continue;
                        }

                        eprint!("\x1b[");
                        for ch in chars.by_ref() {
                            if ch == '}' {
                                break;
                            }
                            eprint!("{}", ch);
                            // Re-apply color when reset
                            if ch == '0' {
                                eprint!(";{}", DEBUGGER_PRIMARY_COLOR);
                            }
                        }
                        eprint!("m");
                    }
                }

                LineTracker.write_str(string).unwrap();
                return Ok(());
            }
        };

        if self.minimal {
            eprint!("{}", Decolored::new(string));
            return Ok(());
        }

        eprint!("{}", Colored::new(color, string));
        LineTracker.write_str(string).unwrap();
        Ok(())
    }
}

/// Tracks whether the cursor is at the start of a line (at column 1).
///
/// Uses [`fmt::Write`], in order to handle [`fmt::Arguments`], and therefore anything which implements
/// [`fmt::Display`].
///
/// If the last printable character is a newline character ('\n' or '\r'), then the state will be
/// set to `true`.
/// Otherwise it will be set to false.
/// If the string does not contain any printable characters, then the state will not change.
///
/// # Limitations
///
/// - Any non-ASCII characters will be treated as printable.
/// - The only ANSI escape sequences this supports are that of Select Graphic Rendition (color/style) of
/// the form `\x1b[`...`m`.
/// - This may not work if ANSI escape code is split across multiple format arguments or write calls.
struct LineTracker;
impl LineTracker {
    thread_local! {
        static IS_LINE_START: RefCell<bool> = const { RefCell::new(true) };
    }
    pub fn is_line_start() -> bool {
        Self::IS_LINE_START.with(|value| *value.borrow())
    }
    fn set_line_start(new_value: bool) {
        Self::IS_LINE_START.with(|value| value.replace(new_value));
    }
}
impl fmt::Write for LineTracker {
    fn write_str(&mut self, string: &str) -> fmt::Result {
        let mut chars = string.chars();
        while let Some(ch) = chars.next() {
            match ch {
                // Skip everything between '\x1b' and 'm' (inclusive)
                '\x1b' => {
                    while chars.next().is_some_and(|ch| ch != 'm') {}
                    continue;
                }
                // Line breaks start new line
                '\n' | '\r' => Self::set_line_start(true),
                // Skip other control characters
                '\x00'..='\x1f' | '\x7f' => continue,
                // Any other characters are printable
                _ => Self::set_line_start(false),
            }
        }
        Ok(())
    }
}

/// Applies an ANSI color/style attribute to a string, when displayed.
///
/// Given attributes are re-applied after any 'reset' code (`\x1b[0m`) is encountered.
struct Colored<'a> {
    /// Note: This is only `'static` for convenience. If needed, another lifetime parameter could
    /// be created.
    color: &'static str,
    string: &'a str,
}
impl<'a> Colored<'a> {
    pub fn new(color: &'static str, string: &'a str) -> Self {
        Self { color, string }
    }
}
impl fmt::Display for Colored<'_> {
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
                for ch in chars.by_ref() {
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

        // Do not reset color. This should be done by caller (Eg. `dprint!`)

        Ok(())
    }
}

/// Removes all ANSI escape codes (color codes) from a string, when displayed.
struct Decolored<'a> {
    string: &'a str,
}
impl<'a> Decolored<'a> {
    pub fn new(string: &'a str) -> Self {
        Self { string }
    }
}
impl fmt::Display for Decolored<'_> {
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
