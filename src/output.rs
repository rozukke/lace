use std::cell::RefCell;
use std::fmt::{self, Write as _};

use crate::runtime::RunState;

/// Colors used by [`Output::Debugger`].
///
/// Note that color depends on the [`Category`] used, and can be overridden.
pub mod debugger_colors {
    pub const PRIMARY: &str = "34";
    pub const WARNING: &str = "33";
    pub const ERROR: &str = "31";
}

/// Print to debugger output.
#[macro_export]
macro_rules! dprint {
    ( Alternate $(, $($tt:tt)* )? ) => {{
        compile_error!("cannot use `Alternate` with `dprint`. use `dprintln` instead");
    }};

    // `$fmt:expr` is required to allow `concat!` macro to be accepted
    ( $condition:expr, $category:expr, $fmt:expr $(, $($tt:tt)* )? ) => {{
        // This is not very hygenic. But makes macro more ergonomic to use.
        #[allow(unused_imports)]
        use $crate::output::{Condition::*, Category::*};

        $crate::output::Output::Debugger($crate::output::Condition::Sometimes, $category)
            .print_category($category);
        $crate::output::Output::Debugger($condition, $category)
            .print(format_args!($fmt $(, $($tt)* )?));
        $crate::output::Output::Debugger($condition, $category)
            .reset_style();
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

/// Print to debugger output, with a newline.
#[macro_export]
macro_rules! dprintln {
    // Choose message depending on `Output::is_minimal`
    ( Alternate, $category:expr,
      $minimal:literal,                         // If minimal
      [ $fmt:expr $(, $($tt:tt)* )? ] $(,)? // Otherwise
    ) => {{
        if $crate::output::Output::is_minimal() {
            $crate::dprint!(Always, $category, concat!($minimal, "\n"));
        } else {
            $crate::dprint!(Always, $category, $fmt $(, $($tt)* )?);
        }
    }};

    ( Alternate, $category:expr,
      [ $fmt_a:expr $(, $($tt_a:tt)* )? ] $(,)?
    ) => {{
        compile_error!("requires two format groups");
    }};
    ( Alternate, $category:expr $(, $($tt:tt)* )? ) => {{
        compile_error!("requires two format groups");
    }};
    ( Alternate $(, $($tt:tt)* )? ) => {{
        compile_error!("requires category and two format groups");
    }};

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
            // Do not use `dprintln` or `self.print` or anything
            // No attributes should be applied
            eprintln!();
            LineTracker
                .write_str("\n")
                .expect("`LineTracker::write_str` should never fail");
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
    /// Must only be called on [`Output::Debugger`].
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
                self.print(format_args!("R{} 0x{:04x}\n", i, state.reg(i)));
            }
            self.print(format_args!("PC 0x{:04x}\n", state.pc()));
            self.print(format_args!("CC 0b{:03b}\n", state.flag() as u8));
            return;
        }

        self.print("\x1b[2m┌───────────────────────────────────┐\x1b[0m\n");
        self.print("\x1b[2m│        \x1b[3mhex     int    uint    chr\x1b[0m\x1b[2m │\x1b[0m\n");

        // R0-7
        for i in 0..8 {
            self.print("\x1b[2m│\x1b[0m");
            self.print(format_args!(" \x1b[1mR\x1b[1m{}\x1b[0m  ", i));
            self.print_integer_inner(state.reg(i));
            self.print(" \x1b[2m│\x1b[0m\n");
        }

        // PC, CC
        self.print("\x1b[2m├─────────────────┬─────────────────┤\x1b[0m\n");
        self.print("\x1b[2m│\x1b[0m");
        self.print("    \x1b[1mPC\x1b[0m");
        self.print(format_args!(" 0x{:04x}", state.pc()));
        self.print("\x1b[2m    │    \x1b[0m");
        self.print(" \x1b[1mCC\x1b[0m");
        self.print(format_args!("  {:03b}", state.flag() as u8));
        self.print("     \x1b[2m│\x1b[0m\n");

        self.print("\x1b[2m└─────────────────┴─────────────────┘\x1b[0m\n");
    }

    /// Print table of breakpoints: three columns and any number of rows.
    ///
    /// `row` argument will be called with values `0..` until `None` is returned.
    ///
    /// Addresses (left column) are printed as unsigned hexadecimal integers.
    ///
    /// Label and instruction values are printed in the next two columns, truncated if their length
    /// exceeds their respective column width.
    ///
    /// Must only be called on [`Output::Debugger`].
    /// Must NOT be called if `Output::is_minimal()`.
    pub fn print_breakpoint_table<'a, F>(&self, row: F)
    where
        F: Fn(usize) -> Option<(u16, &'a str, &'a str)>,
    {
        debug_assert!(
            matches!(self, Self::Debugger(..)),
            "`Output::print_breakpoint_table()` called on `Output::Normal`"
        );
        debug_assert!(
            !Self::is_minimal(),
            "`Output::print_breakpoint_table()` should not be called if `--minimal`"
        );

        const WIDTH_ADDRESS: usize = " 0x1234 ".len(); // Note the whitespace
        const WIDTH_LABEL: usize = 14; // Arbitrary
        const WIDTH_LINE: usize = 28; // Arbitrary

        let print_line = |char_line: char, char_left: char, char_middle: char, char_right: char| {
            self.print(char_left);
            for _ in 0..WIDTH_ADDRESS {
                self.print(char_line);
            }
            self.print(char_middle);
            for _ in 0..WIDTH_LABEL {
                self.print(char_line);
            }
            self.print(char_middle);
            for _ in 0..WIDTH_LINE {
                self.print(char_line);
            }
            self.print(char_right);
            self.print('\n');
        };

        self.print("\x1b[2m");
        print_line('─', '┌', '┬', '┐');

        // Print cell value with max length
        // Replace final ' ' with ellipsis if length exceeds max
        let print_cell = |text: &str, width: usize| {
            let mut len = 0;
            for (i, ch) in text.chars().enumerate() {
                len += 1;
                if i > width - 3 {
                    self.print('…');
                    break;
                }
                self.print(ch);
            }
            for _ in len..width - 1 {
                self.print(' ');
            }
        };

        for i in 0.. {
            let Some((address, label, line)) = row(i) else {
                break;
            };
            if i > 0 {
                print_line('─', '├', '┼', '┤');
            }

            self.print("│ \x1b[0;1m");
            self.print(format_args!("0x{:04x}", address));

            self.print("\x1b[0;2m │ \x1b[0m");
            print_cell(label, WIDTH_LABEL);

            self.print("\x1b[2m│ \x1b[0m");
            print_cell(line, WIDTH_LINE);

            self.print("\x1b[2m│");
            self.print('\n');
        }

        print_line('─', '└', '┴', '┘');
        self.print("\x1b[0m");
    }

    /// Prints a register as hex, signed decimal, unsigned decimal, and character, in a fancy
    /// table.
    pub fn print_integer(&self, value: u16) {
        if Self::is_minimal() {
            self.print_decimal(value);
            self.print('\n');
            return;
        }
        self.print("\x1b[2m┌───────────────────────────────┐\x1b[0m\n");
        self.print("\x1b[2m│    \x1b[3mhex     int    uint    chr\x1b[0m\x1b[2m │\x1b[0m\n");
        self.print("\x1b[2m│\x1b[0m ");
        self.print_integer_inner(value);
        self.print(" \x1b[2m│\x1b[0m\n");
        self.print("\x1b[2m└───────────────────────────────┘\x1b[0m\n");
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
        self.print("    ");
        self.print_char_display(value);
    }

    /// Print a character in a descriptive way:
    ///
    /// - 'Significant' control characters display their abbreviated names.
    /// - ASCII space is displayed as `[_]`.
    /// - Printable ASCII characters are displayed normally.
    /// - Any other ASCII character is printed as `───`
    /// - Any non-ASCII (UTF-16) character is displayed as `┄┄┄`
    ///
    /// Must NOT be called if `Output::is_minimal()`.
    fn print_char_display(&self, value: u16) {
        debug_assert!(
            !Self::is_minimal(),
            "`Output::print_display()` should not be called if `--minimal`"
        );
        if Self::is_minimal() {
            return;
        }

        // Print 3 characters
        #[allow(clippy::match_overlapping_arm)]
        match value {
            // ASCII control characters which are arbitrarily considered significant
            // ᴀʙᴄᴅᴇꜰɢʜɪᴊᴋʟᴍɴᴏᴘꞯʀꜱᴛᴜᴠᴡxʏᴢ
            0x00 => self.print("ɴᴜʟ"),
            0x08 => self.print(" ʙꜱ"),
            0x09 => self.print(" ʜᴛ"),
            0x0a => self.print(" ʟꜰ"),
            0x0b => self.print(" ᴠᴛ"),
            0x0c => self.print(" ꜰꜰ"),
            0x0d => self.print(" ᴄʀ"),
            0x1b => self.print("ᴇꜱᴄ"),
            0x7f => self.print("ᴅᴇʟ"),

            // Space
            0x20 => self.print("[_]"),

            // Printable ASCII characters
            0x21..=0x7e => self.print(format_args!("{:^3}", value as u8 as char)),

            // Unimportant control characters and non-ASCII characters
            _ => self.print("\x1b[2m───\x1b[0m"),
        }
    }

    /// Print a value returned by the [`format_args!`] macro.
    ///
    /// Use [`Output::print`] wrapper method.
    fn print_fmt(&self, args: fmt::Arguments) {
        let minimal = Self::is_minimal();
        match self {
            Self::Normal => {
                NormalWriter { minimal }
                    .write_fmt(args)
                    .expect("`NormalWriter::write_fmt` should never fail");
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
                .expect("`DebuggerWriter::write_fmt` should never fail");
            }
        }
    }

    /// Resets all ANSI color/style attributes for debugger output.
    ///
    /// Must only be called on [`Output::Debugger`].
    pub fn reset_style(&self) {
        debug_assert!(
            matches!(self, Self::Debugger(..)),
            "`Output::reset_style()` called on `Output::Normal`"
        );
        if Self::is_minimal() {
            return;
        }
        // Bypass debugger writer
        eprint!("\x1b[0m");
    }
}

/// Writer for [`Output::Normal`]
struct NormalWriter {
    minimal: bool,
}
impl fmt::Write for NormalWriter {
    /// Must never fail.
    fn write_str(&mut self, string: &str) -> fmt::Result {
        if self.minimal {
            print!("{}", Decolored::new(string));
        } else {
            print!("{}", string);
        }
        LineTracker
            .write_str(string)
            .expect("`LineTracker::write_str` should never fail");
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
    /// Must never fail.
    fn write_str(&mut self, string: &str) -> fmt::Result {
        let color = match self.category {
            Category::Normal => debugger_colors::PRIMARY,
            Category::Info => debugger_colors::PRIMARY,
            Category::Warning => debugger_colors::WARNING,
            Category::Error => debugger_colors::ERROR,

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
                    eprint!("\x1b[{}m", debugger_colors::PRIMARY);
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
                                eprint!(";{}", debugger_colors::PRIMARY);
                            }
                        }
                        eprint!("m");
                    }
                }

                LineTracker
                    .write_str(string)
                    .expect("`LineTracker::write_str` should never fail");
                return Ok(());
            }
        };

        if self.minimal {
            eprint!("{}", Decolored::new(string));
        } else {
            eprint!("{}", Colored::new(color, string));
        }

        LineTracker
            .write_str(string)
            .expect("`LineTracker::write_str` should never fail");
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
///   the form `\x1b[`...`m`.
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
    /// Must never fail.
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
