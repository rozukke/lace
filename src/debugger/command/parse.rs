use integer::parse_integer;

use super::error;
use super::{CommandName, Label, Location, MemoryLocation};
use crate::symbol::Register;

pub use naive::NaiveType;

// TODO(doc): Update doc comments for parsing functions!!!

// TODO(refactor): `.peek` -> `.next_if` where possible

mod integer {
    use super::{error, CharIter};

    #[derive(Clone, Copy, Debug)]
    enum Sign {
        Positive = 1,
        Negative = -1,
    }

    #[derive(Clone, Copy, Debug)]
    pub enum Radix {
        Binary = 2,
        Octal = 8,
        Decimal = 10,
        Hex = 16,
    }

    impl Radix {
        /// Parse a single digit in a given radix.
        pub fn parse_digit(&self, ch: char) -> Option<u8> {
            Some(match self {
                Self::Binary => match ch {
                    '0' => 0,
                    '1' => 1,
                    _ => return None,
                },
                Self::Octal => match ch {
                    '0'..='7' => ch as u8 - b'0',
                    _ => return None,
                },
                Self::Decimal => match ch {
                    '0'..='9' => ch as u8 - b'0',
                    _ => return None,
                },
                Self::Hex => match ch {
                    '0'..='9' => ch as u8 - b'0',
                    'a'..='f' => ch as u8 - b'a' + 10,
                    'A'..='F' => ch as u8 - b'A' + 10,
                    _ => return None,
                },
            })
        }
    }

    /// Try to convert an `i32` into `i16`.
    pub fn int_as_i16(integer: i32) -> Result<i16, error::Value> {
        integer
            .try_into()
            .map_err(|_| error::Value::IntegerTooLarge {
                max: i16::MAX as u16,
            })
    }
    /// Try to convert an `i32` into `u16`.
    pub fn int_as_u16(integer: i32) -> Result<u16, error::Value> {
        integer
            .try_into()
            .map_err(|_| error::Value::IntegerTooLarge { max: u16::MAX })
    }
    /// Try to convert an `i32` into `u16`. Cast negative values.
    pub fn int_as_u16_cast(integer: i32) -> Result<u16, error::Value> {
        if integer < 0 {
            Ok(int_as_i16(integer)? as u16)
        } else {
            int_as_u16(integer)
        }
    }

    /// Parse and consume the next integer argument.
    ///
    /// Extremely liberal in accepted syntax.
    ///
    /// Accepts:
    ///  - Decimal (optional "#"), hex ("x"/"X"), octal ("o"/"O"), and binary ("b"/"B").
    ///  - Optional single zero before non-decimal radix prefix. Eg. "0x4".
    ///  - Leading zeros after prefix and sign. Eg. "0x0004", "#-03".
    ///  - Sign character before xor after radix prefix. Eg. "-#2", "x+4".
    ///
    /// Returns `Ok(None)` (not an integer) for:
    ///  - Empty token.
    ///  - Non-decimal radix prefix, with no zero before it, and non-digits after it. Eg. "xLabel", "o".
    ///
    /// Returns `Err` (invalid integer and invalid token) for:
    ///  - Invalid digits for the given radix.
    ///  - Decimal radix prefix "#" with zeros before it. Eg. "0#2".
    ///  - Decimal radix prefix "#" with no digits after it. Eg. "#".
    ///  - Multiple sign characters (before or after prefix).
    ///  - Missing sign character "-" or "+", if `require_sign == true`.
    ///  - Multiple zeros before radix prefix. Eg. "00x4".
    ///  - Absolute value out of bounds for `i32`. (Does *NOT* check if integer fits in specific bit size).
    pub fn parse_integer(string: &str, require_sign: bool) -> Result<Option<i32>, error::Value> {
        // Useful for parsing label/pc offset
        if string.is_empty() {
            return Ok(None);
        }

        let mut chars: CharIter = string.chars().peekable();

        // Take sign BEFORE prefix
        let first_sign = take_sign(&mut chars);

        let prefix = match take_prefix(&mut chars)? {
            PrefixResult::Integer(prefix) => prefix,
            // Bypass normal parsing
            // The string must be "0" so no concerns about trailing characters
            PrefixResult::SingleZero => return Ok(Some(0)),

            PrefixResult::NonInteger => {
                // Sign was already given, so it must be an invalid token
                if first_sign.is_some() {
                    return Err(error::Value::MalformedInteger {});
                }
                return Ok(None);
            }
        };

        // Take sign AFTER prefix
        let second_sign = take_sign(&mut chars);

        // Reconcile multiple sign characters
        let sign = match (first_sign, second_sign) {
            (Some(sign), None) => Some(sign),
            (None, Some(sign)) => Some(sign),
            (None, None) => {
                if require_sign {
                    return Err(error::Value::MalformedInteger {});
                }
                None
            }
            // Disallow multiple sign characters: "-x-...", "++...", etc
            (Some(_), Some(_)) => return Err(error::Value::MalformedInteger {}),
        };

        // Check next character is digit
        // Character must be checked against radix here to prevent valid non-integer tokens returning `Err`
        if chars
            .peek()
            .is_none_or(|ch| prefix.radix.parse_digit(*ch).is_none())
        {
            // Sign, pre-prefix zeros, or non-alpha prefix ("#") were given, so it must be an invalid integer token
            if sign.is_some() || prefix.leading_zeros || prefix.non_alpha {
                return Err(error::Value::MalformedInteger {});
            }
            return Ok(None);
        };

        // Take digits until non-digit character
        // Note that this loop handles post-prefix leading zeros like any other digit
        let mut integer: i32 = 0;
        for ch in chars.by_ref() {
            // Invalid digit will always return `Err`
            // Valid non-integer tokens should trigger early return before this loop
            let Some(digit) = prefix.radix.parse_digit(ch) else {
                return Err(error::Value::MalformedInteger {});
            };

            // Re-checked later on convert to smaller int types
            if integer > i32::MAX / prefix.radix as i32 {
                return Err(error::Value::IntegerTooLarge {
                    max: i16::MAX as u16,
                });
            }

            integer *= prefix.radix as i32;
            integer += digit as i32;
        }

        assert!(
            chars.next().is_none(),
            "should have looped until end of argument, or early-returned `Err`",
        );

        // TODO(fix): I think there is an edge case here for overflow
        if let Some(sign) = sign {
            integer *= sign as i32;
        }

        Ok(Some(integer))
    }

    fn take_sign(chars: &mut CharIter) -> Option<Sign> {
        let sign = match chars.peek() {
            Some('+') => Sign::Positive,
            Some('-') => Sign::Negative,
            _ => return None,
        };
        chars.next();
        Some(sign)
    }

    /// Helper struct for retaining syntax information when parsing integer prefix.
    struct Prefix {
        /// Radix corresponding to prefix character.
        radix: Radix,
        /// Whether prefix character is preceeded by zeros.
        leading_zeros: bool,
        /// Whether prefix character is a symbol (i.e. "#").
        non_alpha: bool,
    }

    /// Helper struct similar to `Option<Prefix>` but also handles "0" case.
    enum PrefixResult {
        /// Normal integer with (explicit or implicit) prefix.
        Integer(Prefix),
        /// Special case to handle "0".
        SingleZero,
        /// This token is not an integer, but not necessary invalid (yet).
        NonInteger,
    }

    fn take_prefix(chars: &mut CharIter) -> Result<PrefixResult, error::Value> {
        // Only take ONE leading zero here
        // Disallow "00x..." etc.
        let leading_zeros = match chars.peek() {
            Some('0') => {
                chars.next();
                true
            }
            _ => false,
        };

        // Take optional prefix
        let mut consume_char = true;
        let (radix, non_alpha) = match chars.peek() {
            Some('b' | 'B') => (Radix::Binary, false),
            Some('o' | 'O') => (Radix::Octal, false),
            Some('x' | 'X') => (Radix::Hex, false),

            Some('#') => {
                // Disallow "0#..."
                if leading_zeros {
                    return Err(error::Value::MalformedInteger {});
                }
                (Radix::Decimal, true)
            }

            // No prefix
            Some('0'..='9') => {
                consume_char = false; // Leave initial digit, to be parsed by caller
                (Radix::Decimal, false)
            }

            // Disallow "0-..." and "0+..."
            // Disallow "--...", "-+...", etc
            // Any legal pre-prefix sign character would have already been consumed
            Some('-' | '+') => {
                return Err(error::Value::MalformedInteger {});
            }

            // Special case for "0"
            // Only a single "leading" zero (no sign or prefix)
            None if leading_zeros => {
                return Ok(PrefixResult::SingleZero);
            }

            _ => {
                return Ok(PrefixResult::NonInteger);
            }
        };

        if consume_char {
            chars.next();
        }

        Ok(PrefixResult::Integer(Prefix {
            radix,
            leading_zeros,
            non_alpha,
        }))
    }
}

mod name {
    use super::{super::error, ArgIter, CommandName};

    // TODO(feat): Add more aliases (such as undocumented typo aliases)
    #[rustfmt::skip]
    const COMMANDS: CommandNameList = &[
        (CommandName::Help,        &["help", "--help", "h", "-h"]),
        (CommandName::Continue,    &["continue", "cont", "c"]), // or 'proceed'
        (CommandName::Finish,      &["finish", "fin", "f"]),
        (CommandName::Exit,        &["exit"]),
        (CommandName::Quit,        &["quit", "q"]),
        (CommandName::Registers,   &["registers", "reg", "r"]),
        (CommandName::Reset,       &["reset"]),
        (CommandName::Step,        &["progress", "p"]), // or 'advance'
        (CommandName::Next,        &["next", "n"]),
        (CommandName::Get,         &["get", "g"]),
        (CommandName::Set,         &["set", "s"]),
        (CommandName::Jump,        &["jump", "j"]),
        (CommandName::Source,      &["assembly", "asm", "a"]), // or 'source'
        (CommandName::Eval,        &["eval", "e"]),
        (CommandName::BreakList,   &["breaklist", "bl"]),
        (CommandName::BreakAdd,    &["breakadd", "ba"]),
        (CommandName::BreakRemove, &["breakremove", "br"]),
    ];
    const BREAK_COMMAND: CandidateList = &["break", "b"];
    #[rustfmt::skip]
    const BREAK_SUBCOMMANDS: CommandNameList = &[
        (CommandName::BreakList,   &["list", "l"]),
        (CommandName::BreakAdd,    &["add", "a"]),
        (CommandName::BreakRemove, &["remove", "r"]),
    ];

    /// A [`CommandName`] with a list of name candidates.
    type CommandNameList<'a> = &'a [(CommandName, CandidateList<'a>)];
    /// List of single-word aliases for a command or subcommand.
    type CandidateList<'a> = &'a [&'a str];

    /// Returns the first [`CommandName`], which has a corresponding candidate which matches `name`
    /// (case insensitive).
    ///
    /// Returns `None` if no match was found.
    fn find_name_match(name: &str, commands: CommandNameList) -> Option<CommandName> {
        for (command, candidates) in commands {
            if name_matches(name, candidates) {
                return Some(*command);
            }
        }
        None
    }

    /// Returns `true` if `name` matchs any item of `candidates` (case insensitive).
    fn name_matches(name: &str, candidates: CandidateList) -> bool {
        for candidate in candidates {
            if name.eq_ignore_ascii_case(candidate) {
                return true;
            }
        }
        false
    }

    pub fn parse_command_name(args: &mut ArgIter) -> Result<CommandName, error::Command> {
        let command_name = args.next_str();
        // Command source should always return a string containing non-whitespace
        // characters, so initial command name should always exist.
        debug_assert!(command_name.is_some(), "missing command name");
        let command_name = command_name.unwrap_or("");

        if let Some(command) = find_name_match(command_name, COMMANDS) {
            return Ok(command);
        };

        // This could be written a bit nicer. But it doesn't seem necessary.
        if name_matches(command_name, BREAK_COMMAND) {
            // Normalize name and get as `'static`
            // Only used for errors
            let command_name = BREAK_COMMAND[0]; // Array must be non-empty if this branch is being ran

            let Some(subcommand_name) = args.next_str() else {
                return Err(error::Command::MissingSubcommand { command_name });
            };
            let Some(command) = find_name_match(subcommand_name, BREAK_SUBCOMMANDS) else {
                return Err(error::Command::InvalidSubcommand {
                    command_name,
                    subcommand_name: subcommand_name.to_string(),
                });
            };
            return Ok(command);
        }

        Err(error::Command::InvalidCommand {
            command_name: command_name.to_string(),
        })
    }
}

mod naive {
    use std::fmt;

    use super::{integer::Radix, label_can_contain, label_can_start_with};

    #[derive(Debug, PartialEq)]
    pub enum NaiveType {
        Integer,
        Register,
        Label,
        PCOffset,
    }

    impl NaiveType {
        pub fn as_str(&self) -> &'static str {
            match self {
                NaiveType::Integer => "integer",
                NaiveType::Register => "register",
                NaiveType::Label => "label",
                NaiveType::PCOffset => "PC offset",
            }
        }

        /// If string does not start with `[-+#0-9]`, then all characters need to be checked, to ensure
        /// labels starting with `[bBoOxX]` don't get classified as integers.
        ///
        /// - `^[-+#0-9]`
        /// - or `^[bB][-+]?[01]+$`
        /// - or `^[oO][-+]?[0-7]+$`
        /// - or `^[xX][-+]?[0-9a-fA-F]+$`
        fn is_str_integer(string: &str) -> bool {
            let mut chars = string.chars().peekable();
            if chars
                .peek()
                .is_some_and(|ch| matches!(ch, '-' | '+' | '#' | '0'..='9'))
            {
                return true;
            }
            let radix = match chars.next() {
                Some('b' | 'B') => Radix::Binary,
                Some('o' | 'O') => Radix::Octal,
                Some('x' | 'X') => Radix::Hex,
                _ => return false,
            };
            if matches!(chars.peek(), Some('-' | '+')) {
                chars.next();
            }
            if chars.peek().is_none() {
                return false;
            }
            for ch in chars {
                if radix.parse_digit(ch).is_none() {
                    return false;
                }
            }
            true
        }

        /// - `^[rR][0-7]$`
        fn is_str_register(string: &str) -> bool {
            let mut chars = string.chars();
            chars.next().is_some_and(|ch| matches!(ch, 'r' | 'R'))
                && chars.next().is_some_and(|ch| matches!(ch, '0'..='7'))
                && !chars.next().is_some_and(label_can_contain)
        }

        /// - `^[a-zA-Z_]
        fn is_str_label(string: &str) -> bool {
            string.chars().next().is_some_and(label_can_start_with)
        }

        /// - `^\^`
        fn is_str_pc_offset(string: &str) -> bool {
            string.chars().next().is_some_and(|ch| ch == '^')
        }
    }

    impl fmt::Display for NaiveType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.as_str())
        }
    }

    impl TryFrom<&str> for NaiveType {
        type Error = ();

        fn try_from(string: &str) -> Result<Self, Self::Error> {
            // Ordered by approximate function speed
            if Self::is_str_pc_offset(string) {
                return Ok(Self::PCOffset);
            }
            if Self::is_str_register(string) {
                return Ok(Self::Register);
            }
            if Self::is_str_integer(string) {
                return Ok(Self::Integer);
            }
            if Self::is_str_label(string) {
                return Ok(Self::Label);
            }
            Err(())
        }
    }
}

pub struct ArgIter<'a> {
    buffer: &'a str,
    /// Byte index.
    cursor: usize,

    /// Amount of arguments requested (successfully or not).
    ///
    /// Must only be incremented by [`Self::next_argument`].
    arg_count: u8,
}

impl<'a> From<&'a str> for ArgIter<'a> {
    fn from(buffer: &'a str) -> Self {
        Self {
            buffer,
            cursor: 0,
            arg_count: 0,
        }
    }
}

macro_rules! check_naive_type {
    ( $pattern:pat, $expected_type:expr, ($argument_name:expr, $argument:expr) ) => {{
        #[allow(unused_imports)]
        use NaiveType::*;

        match NaiveType::try_from($argument) {
            // Types match
            Ok($pattern) => (),

            // Could not discern type
            // Either an invalid type (which should be handled after a genuine parse attempt for
            // each type, as `MalformedValue`), or a bug in the naive check (which the user can
            // easily ignore)
            Err(()) => (),

            Ok(__naive_type) => {
                return Err(error::Argument::InvalidValue {
                    argument_name: $argument_name,
                    string: $argument.to_string(),

                    error: error::Value::MismatchedType {
                        expected_type: $expected_type,
                        actual_type: __naive_type,
                    },
                });
            }
        };
    }};
}

impl<'a> ArgIter<'a> {
    // Do not `impl Iterator`. This method should be private
    fn next_str(&mut self) -> Option<&'a str> {
        let mut start = self.cursor;
        let mut length = 0;
        let mut is_start = true;

        for ch in self.buffer[self.cursor..].chars() {
            debug_assert!(
                !matches!(ch, ';' | '\n'),
                "semicolons/newlines should have been handled already"
            );

            // Skip leading whitespace
            if is_start && ch == ' ' {
                start += ch.len_utf8();
                continue;
            }
            is_start = false;

            if matches!(ch, ' ' | ';' | '\n') {
                break;
            }
            length += ch.len_utf8();
        }

        let end = start + length;
        if start == end {
            return None;
        }

        let argument = &self.buffer[start..end];
        self.cursor = end;
        Some(argument)
    }

    /// Consume the rest of the command as one string.
    ///
    /// Leading/trailing whitespace is trimmed.
    ///
    /// Used for `eval` command.
    pub fn collect_rest(&mut self) -> &'a str {
        let start = self.cursor;
        self.cursor = self.buffer.len();
        self.buffer[start..].trim()
    }

    pub fn arg_count(&self) -> u8 {
        // TODO: Increment argument count in parsing methods
        self.arg_count
    }

    /// Parse and consume command name.
    ///
    /// Considers multi-word command names (i.e. subcommands) as one name. Eg. `break add`.
    ///
    /// Assumes line is non-empty.
    pub fn get_command_name(&mut self) -> Result<CommandName, error::Command> {
        name::parse_command_name(self)
    }

    /// Parse and consume next integer argument. Use default result value if argument is `None`.
    fn next_integer_inner(
        &mut self,
        argument_name: &'static str,
        default: Result<u16, error::Argument>,
    ) -> Result<u16, error::Argument> {
        let Some(argument) = self.next_str() else {
            return default;
        };

        check_naive_type!(
            Integer,
            "integer, label, register, or PC offset",
            (argument_name, argument)
        );

        let integer =
            parse_integer(argument, false).map_err(wrap_invalid_value(argument_name, argument))?;

        if let Some(integer) = integer {
            let integer = integer::int_as_u16_cast(integer)
                .map_err(wrap_invalid_value(argument_name, argument))?;
            return Ok(integer);
        };

        Err(error::Argument::InvalidValue {
            argument_name,
            string: argument.to_string(),
            error: error::Value::MalformedValue {},
        })
    }

    /// Parse and consume next integer argument.
    pub fn next_integer(
        &mut self,
        argument_name: &'static str,
        expected_count: u8,
    ) -> Result<u16, error::Argument> {
        self.next_integer_inner(
            argument_name,
            Err(error::Argument::MissingArgument {
                argument_name,
                expected_count,
                actual_count: 99, // TODO
            }),
        )
    }

    /// Parse and consume next positive integer argument, defaulting to `1`.
    ///
    /// Non-positive values will also be converted to `1`.
    pub fn next_positive_integer_or_default(
        &mut self,
        argument_name: &'static str,
    ) -> Result<u16, error::Argument> {
        self.next_integer_inner(argument_name, Ok(1))
            .map(|value| value.max(1)) // 0 -> 1
    }

    /// Parse and consume next [`Location`] argument: a register or [`MemoryLocation`].
    pub fn next_location(
        &mut self,
        argument_name: &'static str,
        expected_count: u8,
    ) -> Result<Location<'a>, error::Argument> {
        let Some(argument) = self.next_str() else {
            return Err(error::Argument::MissingArgument {
                argument_name,
                expected_count,
                actual_count: 99,
            });
        };

        // Don't perform a naive type check here
        // All current valid types are allowed here, and any invalid value will be handled before
        // the end of this function (as `MalformedValue`)

        if let Some(register) =
            parse_register(argument).map_err(wrap_invalid_value(argument_name, argument))?
        {
            return Ok(Location::Register(register));
        };
        parse_memory_location(argument_name, argument).map(Location::Memory)
    }

    /// Parse and consume next [`MemoryLocation`] argument. Use default result value if argument is `None`.
    fn next_memory_location_inner(
        &mut self,
        argument_name: &'static str,
        default: Result<MemoryLocation<'a>, error::Argument>,
    ) -> Result<MemoryLocation<'a>, error::Argument> {
        let Some(argument) = self.next_str() else {
            return default;
        };

        check_naive_type!(
            Integer | Label | PCOffset,
            "integer, label, or PC offset",
            (argument_name, argument)
        );

        parse_memory_location(argument_name, argument)
    }

    /// Parse and consume next [`MemoryLocation`] argument.
    pub fn next_memory_location(
        &mut self,
        argument_name: &'static str,
        expected_count: u8,
    ) -> Result<MemoryLocation<'a>, error::Argument> {
        self.next_memory_location_inner(
            argument_name,
            Err(error::Argument::MissingArgument {
                argument_name,
                expected_count,
                actual_count: 99,
            }),
        )
    }

    /// Parse and consume next [`MemoryLocation`] argument, defaulting to program counter.
    /// ([`MemoryLocation::PCOffset`]).
    pub fn next_memory_location_or_default(
        &mut self,
        argument_name: &'static str,
    ) -> Result<MemoryLocation<'a>, error::Argument> {
        self.next_memory_location_inner(argument_name, Ok(MemoryLocation::PCOffset(0)))
    }

    /// Returns an error if the command contains any arguments which haven't been consumed.
    pub fn expect_end(&mut self, expected: u8, actual: u8) -> Result<(), error::Argument> {
        if self.next_str().is_none() {
            Ok(())
        } else {
            Err(error::Argument::TooManyArguments {
                expected_count: expected,
                actual_count: actual,
            })
        }
    }
}

fn wrap_invalid_value<'a>(
    argument_name: &'static str,
    argument: &'a str,
) -> impl Fn(error::Value) -> error::Argument + 'a {
    move |error| error::Argument::InvalidValue {
        argument_name,
        string: argument.to_string(),
        error,
    }
}

fn parse_memory_location<'a>(
    argument_name: &'static str,
    argument: &'a str,
) -> Result<MemoryLocation<'a>, error::Argument> {
    // TODO(refactor): Create function to create `error::Argument::InvalidValue` from parts
    if let Some(address) =
        parse_integer(argument, false).map_err(wrap_invalid_value(argument_name, argument))?
    {
        let address =
            integer::int_as_u16(address).map_err(wrap_invalid_value(argument_name, argument))?;
        return Ok(MemoryLocation::Address(address));
    };

    if let Some(label) =
        parse_label(argument).map_err(wrap_invalid_value(argument_name, argument))?
    {
        return Ok(MemoryLocation::Label(label));
    };

    if let Some(offset) =
        parse_pc_offset(argument).map_err(wrap_invalid_value(argument_name, argument))?
    {
        return Ok(MemoryLocation::PCOffset(offset));
    };

    Err(error::Argument::InvalidValue {
        argument_name,
        string: argument.to_string(),
        error: error::Value::MalformedValue {},
    })
}

/// Returns `true` if the given character can appear at the start of a label.
fn label_can_start_with(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
}
/// Returns `true` if the given character can appear as a subsequent character of a label.
fn label_can_contain(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

type CharIter<'a> = std::iter::Peekable<std::str::Chars<'a>>;

/// Track byte length of consumed characters.
struct ByteCounted<'a> {
    inner: CharIter<'a>,
    len: usize,
}

impl Iterator for ByteCounted<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.inner.next()?;
        self.len += ch.len_utf8();
        Some(ch)
    }
}

impl<'a> ByteCounted<'a> {
    pub fn from(inner: CharIter<'a>) -> Self {
        Self { inner, len: 0 }
    }
    pub fn len(&self) -> usize {
        self.len
    }
    pub fn peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }
}

fn parse_label(string: &str) -> Result<Option<Label>, error::Value> {
    let mut chars = ByteCounted::from(string.chars().peekable());

    // Check first character can begin a label
    if !chars.next().is_some_and(label_can_start_with) {
        return Ok(None);
    };
    // Take characters until non-alphanumeric
    while chars.peek().copied().is_some_and(label_can_contain) {
        chars.next();
    }

    let length = chars.len();
    let (name, offset_str) = string.split_at(length);

    let offset = if offset_str.is_empty() {
        0
    } else {
        match parse_integer(offset_str, true)? {
            Some(offset) => integer::int_as_i16(offset)?,
            None => return Err(error::Value::MalformedLabel {}),
        }
    };

    Ok(Some(Label { name, offset }))
}

fn parse_pc_offset(string: &str) -> Result<Option<i16>, error::Value> {
    if string.chars().next().is_none_or(|ch| ch != '^') {
        return Ok(None);
    }
    let offset_str = &string['^'.len_utf8()..];

    let offset = if offset_str.is_empty() {
        0
    } else {
        match parse_integer(offset_str, true)? {
            Some(offset) => integer::int_as_i16(offset)?,
            None => return Err(error::Value::MalformedInteger {}),
        }
    };

    Ok(Some(offset))
}

fn parse_register(string: &str) -> Result<Option<Register>, error::Value> {
    let mut chars = string.chars();

    match chars.next() {
        Some('r' | 'R') => (),
        _ => return Ok(None),
    }

    let Some(digit) = chars.next() else {
        return Ok(None);
    };
    let register = match digit {
        '0' => Register::R0,
        '1' => Register::R1,
        '2' => Register::R2,
        '3' => Register::R3,
        '4' => Register::R4,
        '5' => Register::R5,
        '6' => Register::R6,
        '7' => Register::R7,
        _ => return Ok(None),
    };

    if let Some(ch) = chars.next() {
        // Possibly the start of a label
        if label_can_contain(ch) {
            return Ok(None);
        } else {
            return Err(error::Value::MalformedRegister {});
        }
    }

    Ok(Some(register))
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! label {
        ( $name:expr $(, $offset:expr )? $(,)? ) => {
            Label {
                name: $name.into(),
                offset: label!(@offset $($offset)?),
            }
        };
        (@offset $offset:expr) => { $offset };
        (@offset) => { 0 };
    }

    #[test]
    fn many_arguments_works() {
        let line = "  name  -54  r3 0x5812 Foo name2  Baz+0x04 4209";
        let mut iter = ArgIter::from(line);

        let argument_name = "dummy";
        let expected_count = 99;

        assert_eq!(iter.next_str(), Some("name"));
        assert_eq!(iter.next_integer(argument_name, 99), Ok(-54i16 as u16));
        assert_eq!(
            iter.next_location(argument_name, 99),
            Ok(Location::Register(Register::R3)),
        );
        assert_eq!(iter.next_integer(argument_name, expected_count), Ok(0x5812));
        assert_eq!(
            iter.next_memory_location(argument_name, expected_count),
            Ok(MemoryLocation::Label(label!("Foo", 0))),
        );
        assert_eq!(iter.next_str(), Some("name2"));
        assert_eq!(
            iter.next_memory_location(argument_name, expected_count),
            Ok(MemoryLocation::Label(label!("Baz", 0x04))),
        );
        assert_eq!(iter.next_integer(argument_name, expected_count), Ok(4209));
        assert_eq!(iter.next_str(), None);
        assert_eq!(iter.expect_end(expected_count, 100), Ok(()));
    }

    macro_rules! expect_tokens {
        ( . $method:ident ($($args:tt)*), $input:expr, $($expected:tt)* ) => {{
            eprintln!("Test input: <{}>", $input);
            let mut iter = ArgIter::from($input);
            let result = iter.$method($($args)*);
            expect_tokens!(@expected result, $($expected)*);
        }};
        ( $function:ident ($($args:tt)*), $input:expr, $($expected:tt)* ) => {{
            eprintln!("Test input: <{}>", $input);
            let result = $function($input, $($args)*);
            expect_tokens!(@expected result, $($expected)*);
        }};
        (@expected $result:expr, Err(_) $(,)?) => {
            assert!($result.is_err());
        };
        (@expected $result:expr, $expected:expr $(,)?) => {
            assert_eq!($result, $expected, stringify!($expected));
        };
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic]
    fn semicolon_fails_assert() {
        let argument_name = "dummy";
        let expected_count = 99;
        expect_tokens!(.next_integer(argument_name, expected_count), "  ;  ", Err(_));
    }

    #[test]
    fn next_location_works() {
        let argument_name = "dummy";
        let expected_count = 99;
        macro_rules! expect_location { ( $($x:tt)* ) => {
            expect_tokens!(.next_location(argument_name, expected_count), $($x)*);
        }}

        expect_location!("", Err(_));
        expect_location!("R7+1", Err(_));
        expect_location!(
            "a",
            Ok(Location::Memory(MemoryLocation::Label(label!("a")))),
        );
        expect_location!(
            "rn",
            Ok(Location::Memory(MemoryLocation::Label(label!("rn")))),
        );
        expect_location!(
            "r8",
            Ok(Location::Memory(MemoryLocation::Label(label!("r8")))),
        );
        expect_location!(
            "R0n",
            Ok(Location::Memory(MemoryLocation::Label(label!("R0n")))),
        );
        expect_location!(
            "r0n",
            Ok(Location::Memory(MemoryLocation::Label(label!("r0n")))),
        );
        expect_location!("r0", Ok(Location::Register(Register::R0)));
        expect_location!("R7", Ok(Location::Register(Register::R7)));
    }

    #[test]
    fn next_integer_token_works() {
        macro_rules! expect_integer { ( $require_sign:expr, $($x:tt)* ) => {
            expect_tokens!(parse_integer($require_sign), $($x)*);
        }}

        // These tests cover all edge cases which I can think of
        // Invalid or non-integers
        expect_integer!(false, "", Ok(None)); // Non-integer
        expect_integer!(false, "a", Ok(None));
        expect_integer!(false, "z", Ok(None));
        expect_integer!(false, "&", Ok(None));
        expect_integer!(false, ",", Ok(None));
        expect_integer!(false, "b2", Ok(None));
        expect_integer!(false, "o8", Ok(None));
        expect_integer!(false, "xg", Ok(None));
        expect_integer!(false, "b", Ok(None));
        expect_integer!(false, "o", Ok(None));
        expect_integer!(false, "x", Ok(None));
        expect_integer!(false, "-", Err(_)); // Invalid integers
        expect_integer!(false, "+", Err(_));
        expect_integer!(false, "#", Err(_));
        expect_integer!(false, "#-", Err(_));
        expect_integer!(false, "-#", Err(_));
        expect_integer!(false, "-#-", Err(_));
        expect_integer!(false, "-#-24", Err(_));
        expect_integer!(false, "0#0", Err(_));
        expect_integer!(false, "0#24", Err(_));
        expect_integer!(false, "-0#24", Err(_));
        expect_integer!(false, "0#-24", Err(_));
        expect_integer!(false, "-0#-24", Err(_));
        expect_integer!(false, "x-", Err(_));
        expect_integer!(false, "-x", Err(_));
        expect_integer!(false, "-x-", Err(_));
        expect_integer!(false, "-x-24", Err(_));
        expect_integer!(false, "0x", Err(_));
        expect_integer!(false, "0x-", Err(_));
        expect_integer!(false, "-0x", Err(_));
        expect_integer!(false, "-0x-", Err(_));
        expect_integer!(false, "-0x-24", Err(_));
        expect_integer!(false, "0-x24", Err(_));
        expect_integer!(false, "00x4", Err(_));
        expect_integer!(false, "##", Err(_)); // Invalid digit for decimal base
        expect_integer!(false, "-##", Err(_));
        expect_integer!(false, "#b", Err(_));
        expect_integer!(false, "#-b", Err(_));
        expect_integer!(false, "-#b", Err(_));
        expect_integer!(false, "0b2", Err(_)); // Invalid digit for base
        expect_integer!(false, "0o8", Err(_));
        expect_integer!(false, "0xg", Err(_));
        expect_integer!(false, "-b2", Err(_));
        expect_integer!(false, "-o8", Err(_));
        expect_integer!(false, "-xg", Err(_));
        expect_integer!(false, "b-2", Err(_));
        expect_integer!(false, "o-8", Err(_));
        expect_integer!(false, "x-g", Err(_));
        expect_integer!(false, "--4", Err(_)); // Multiple sign characters
        expect_integer!(false, "-+4", Err(_));
        expect_integer!(false, "++4", Err(_));
        expect_integer!(false, "+-4", Err(_));
        expect_integer!(false, "#--4", Err(_));
        expect_integer!(false, "#-+4", Err(_));
        expect_integer!(false, "#++4", Err(_));
        expect_integer!(false, "#+-4", Err(_));
        expect_integer!(false, "-#-4", Err(_));
        expect_integer!(false, "-#+4", Err(_));
        expect_integer!(false, "+#+4", Err(_));
        expect_integer!(false, "+#-4", Err(_));
        expect_integer!(false, "--#4", Err(_));
        expect_integer!(false, "-+#4", Err(_));
        expect_integer!(false, "++#4", Err(_));
        expect_integer!(false, "+-#4", Err(_));
        expect_integer!(true, "--4", Err(_));
        expect_integer!(true, "#--4", Err(_));
        expect_integer!(true, "+#-4", Err(_));
        expect_integer!(true, "+-#4", Err(_));
        expect_integer!(true, "#4", Err(_)); // Missing sign character
        expect_integer!(true, "x4", Err(_));
        // Simple bounds check (it is not supposed to be super accurate)
        expect_integer!(false, "x80000000", Err(_));
        expect_integer!(false, "x7fffffff", Ok(Some(0x7fffffff)));
        expect_integer!(false, "x-7fffffff", Ok(Some(-0x7fffffff)));
        expect_integer!(false, "x-80000000", Err(_));
        // Decimal
        expect_integer!(false, "0", Ok(Some(0)));
        expect_integer!(false, "00", Ok(Some(0)));
        expect_integer!(false, "#0", Ok(Some(0)));
        expect_integer!(false, "#00", Ok(Some(0)));
        expect_integer!(false, "-#0", Ok(Some(0)));
        expect_integer!(false, "+#0", Ok(Some(0)));
        expect_integer!(false, "-#00", Ok(Some(0)));
        expect_integer!(false, "#-0", Ok(Some(0)));
        expect_integer!(false, "#+0", Ok(Some(0)));
        expect_integer!(false, "#-00", Ok(Some(0)));
        expect_integer!(false, "4", Ok(Some(4)));
        expect_integer!(false, "+4", Ok(Some(4)));
        expect_integer!(false, "4284", Ok(Some(4284)));
        expect_integer!(false, "004284", Ok(Some(4284)));
        expect_integer!(false, "#4", Ok(Some(4)));
        expect_integer!(false, "#4284", Ok(Some(4284)));
        expect_integer!(false, "#004284", Ok(Some(4284)));
        expect_integer!(false, "-4", Ok(Some(-4)));
        expect_integer!(false, "+4", Ok(Some(4)));
        expect_integer!(false, "-4284", Ok(Some(-4284)));
        expect_integer!(false, "-004284", Ok(Some(-4284)));
        expect_integer!(false, "-#4", Ok(Some(-4)));
        expect_integer!(false, "+#4", Ok(Some(4)));
        expect_integer!(false, "-#4284", Ok(Some(-4284)));
        expect_integer!(false, "-#004284", Ok(Some(-4284)));
        expect_integer!(false, "#-4", Ok(Some(-4)));
        expect_integer!(false, "#+4", Ok(Some(4)));
        expect_integer!(false, "#-4284", Ok(Some(-4284)));
        expect_integer!(false, "#-004284", Ok(Some(-4284)));
        expect_integer!(true, "-4", Ok(Some(-4)));
        expect_integer!(true, "+4", Ok(Some(4)));
        expect_integer!(true, "-4284", Ok(Some(-4284)));
        expect_integer!(true, "-004284", Ok(Some(-4284)));
        expect_integer!(true, "-#4", Ok(Some(-4)));
        expect_integer!(true, "+#4", Ok(Some(4)));
        expect_integer!(true, "-#4284", Ok(Some(-4284)));
        expect_integer!(true, "-#004284", Ok(Some(-4284)));
        expect_integer!(true, "#-4", Ok(Some(-4)));
        expect_integer!(true, "#+4", Ok(Some(4)));
        expect_integer!(true, "#-4284", Ok(Some(-4284)));
        expect_integer!(true, "#-004284", Ok(Some(-4284)));
        expect_integer!(true, "4", Err(_));
        expect_integer!(true, "4284", Err(_));
        expect_integer!(true, "004284", Err(_));
        expect_integer!(true, "#4", Err(_));
        expect_integer!(true, "#4284", Err(_));
        expect_integer!(true, "#004284", Err(_));
        expect_integer!(true, "#4", Err(_));
        // Hex
        expect_integer!(false, "x0", Ok(Some(0x0)));
        expect_integer!(false, "x00", Ok(Some(0x0)));
        expect_integer!(false, "0x0", Ok(Some(0x0)));
        expect_integer!(false, "0x00", Ok(Some(0x0)));
        expect_integer!(false, "-x0", Ok(Some(0x0)));
        expect_integer!(false, "+x0", Ok(Some(0x0)));
        expect_integer!(false, "-x00", Ok(Some(0x0)));
        expect_integer!(false, "0x-0", Ok(Some(0x0)));
        expect_integer!(false, "0x-00", Ok(Some(0x0)));
        expect_integer!(false, "-0x0", Ok(Some(0x0)));
        expect_integer!(false, "-0x00", Ok(Some(0x0)));
        expect_integer!(false, "x4", Ok(Some(0x4)));
        expect_integer!(false, "x004", Ok(Some(0x4)));
        expect_integer!(false, "x429", Ok(Some(0x429)));
        expect_integer!(false, "0x4", Ok(Some(0x4)));
        expect_integer!(false, "0x004", Ok(Some(0x4)));
        expect_integer!(false, "0x429", Ok(Some(0x429)));
        expect_integer!(false, "-x4", Ok(Some(-0x4)));
        expect_integer!(false, "+x4", Ok(Some(0x4)));
        expect_integer!(false, "-x004", Ok(Some(-0x4)));
        expect_integer!(false, "-x429", Ok(Some(-0x429)));
        expect_integer!(false, "-0x4", Ok(Some(-0x4)));
        expect_integer!(false, "+0x4", Ok(Some(0x4)));
        expect_integer!(false, "-0x004", Ok(Some(-0x4)));
        expect_integer!(false, "-0x429", Ok(Some(-0x429)));
        expect_integer!(false, "x-4", Ok(Some(-0x4)));
        expect_integer!(false, "x-004", Ok(Some(-0x4)));
        expect_integer!(false, "x+004", Ok(Some(0x4)));
        expect_integer!(false, "x-429", Ok(Some(-0x429)));
        expect_integer!(false, "-0x4", Ok(Some(-0x4)));
        expect_integer!(false, "-0x004", Ok(Some(-0x4)));
        expect_integer!(false, "-0x429", Ok(Some(-0x429)));
        expect_integer!(false, "+0x429", Ok(Some(0x429)));
        expect_integer!(true, "-x4", Ok(Some(-0x4)));
        expect_integer!(true, "+x4", Ok(Some(0x4)));
        expect_integer!(true, "-x004", Ok(Some(-0x4)));
        expect_integer!(true, "-x429", Ok(Some(-0x429)));
        expect_integer!(true, "-0x4", Ok(Some(-0x4)));
        expect_integer!(true, "+0x4", Ok(Some(0x4)));
        expect_integer!(true, "-0x004", Ok(Some(-0x4)));
        expect_integer!(true, "-0x429", Ok(Some(-0x429)));
        expect_integer!(true, "x-4", Ok(Some(-0x4)));
        expect_integer!(true, "x-004", Ok(Some(-0x4)));
        expect_integer!(true, "x+004", Ok(Some(0x4)));
        expect_integer!(true, "x-429", Ok(Some(-0x429)));
        expect_integer!(true, "-0x4", Ok(Some(-0x4)));
        expect_integer!(true, "-0x004", Ok(Some(-0x4)));
        expect_integer!(true, "-0x429", Ok(Some(-0x429)));
        expect_integer!(true, "+0x429", Ok(Some(0x429)));
        expect_integer!(true, "x4", Err(_));
        expect_integer!(true, "x004", Err(_));
        expect_integer!(true, "x429", Err(_));
        expect_integer!(true, "0x4", Err(_));
        expect_integer!(true, "0x004", Err(_));
        expect_integer!(true, "0x429", Err(_));
        expect_integer!(true, "x4", Err(_));
        expect_integer!(true, "x004", Err(_));
        expect_integer!(true, "x429", Err(_));
        expect_integer!(true, "0x4", Err(_));
        expect_integer!(true, "0x004", Err(_));
        expect_integer!(true, "0x429", Err(_));
        expect_integer!(true, "0x429", Err(_));
        // Octal (0o427==0x117)
        expect_integer!(false, "o0", Ok(Some(0x0)));
        expect_integer!(false, "o00", Ok(Some(0x0)));
        expect_integer!(false, "0o0", Ok(Some(0x0)));
        expect_integer!(false, "0o00", Ok(Some(0x0)));
        expect_integer!(false, "-o0", Ok(Some(0x0)));
        expect_integer!(false, "-o00", Ok(Some(0x0)));
        expect_integer!(false, "o-0", Ok(Some(0x0)));
        expect_integer!(false, "o-00", Ok(Some(0x0)));
        expect_integer!(false, "-0o0", Ok(Some(0x0)));
        expect_integer!(false, "-0o00", Ok(Some(0x0)));
        expect_integer!(false, "0o-0", Ok(Some(0x0)));
        expect_integer!(false, "0o-00", Ok(Some(0x0)));
        expect_integer!(false, "o4", Ok(Some(0x4)));
        expect_integer!(false, "o004", Ok(Some(0x4)));
        expect_integer!(false, "o427", Ok(Some(0x117)));
        expect_integer!(false, "0o4", Ok(Some(0x4)));
        expect_integer!(false, "0o004", Ok(Some(0x4)));
        expect_integer!(false, "0o427", Ok(Some(0x117)));
        expect_integer!(false, "-o4", Ok(Some(-0x4)));
        expect_integer!(false, "-o004", Ok(Some(-0x4)));
        expect_integer!(false, "-o427", Ok(Some(-0x117)));
        expect_integer!(false, "-0o4", Ok(Some(-0x4)));
        expect_integer!(false, "-0o004", Ok(Some(-0x4)));
        expect_integer!(false, "-0o427", Ok(Some(-0x117)));
        expect_integer!(false, "o-4", Ok(Some(-0x4)));
        expect_integer!(false, "o-004", Ok(Some(-0x4)));
        expect_integer!(false, "o-427", Ok(Some(-0x117)));
        expect_integer!(false, "0o-4", Ok(Some(-0x4)));
        expect_integer!(false, "0o-004", Ok(Some(-0x4)));
        expect_integer!(false, "0o-427", Ok(Some(-0x117)));
        // Binary
        expect_integer!(false, "b0", Ok(Some(0b0)));
        expect_integer!(false, "b00", Ok(Some(0b0)));
        expect_integer!(false, "0b0", Ok(Some(0b0)));
        expect_integer!(false, "0b00", Ok(Some(0b0)));
        expect_integer!(false, "-b0", Ok(Some(0b0)));
        expect_integer!(false, "-b00", Ok(Some(0b0)));
        expect_integer!(false, "b-0", Ok(Some(0b0)));
        expect_integer!(false, "b-00", Ok(Some(0b0)));
        expect_integer!(false, "-0b0", Ok(Some(0b0)));
        expect_integer!(false, "-0b00", Ok(Some(0b0)));
        expect_integer!(false, "0b-0", Ok(Some(0b0)));
        expect_integer!(false, "0b-00", Ok(Some(0b0)));
        expect_integer!(false, "b1", Ok(Some(0b1)));
        expect_integer!(false, "b101", Ok(Some(0b101)));
        expect_integer!(false, "b00101", Ok(Some(0b101)));
        expect_integer!(false, "0b1", Ok(Some(0b1)));
        expect_integer!(false, "0b101", Ok(Some(0b101)));
        expect_integer!(false, "0b00101", Ok(Some(0b101)));
        expect_integer!(false, "-b1", Ok(Some(-0b1)));
        expect_integer!(false, "-b101", Ok(Some(-0b101)));
        expect_integer!(false, "-b00101", Ok(Some(-0b101)));
        expect_integer!(false, "b-1", Ok(Some(-0b1)));
        expect_integer!(false, "b-101", Ok(Some(-0b101)));
        expect_integer!(false, "b-00101", Ok(Some(-0b101)));
        expect_integer!(false, "-0b1", Ok(Some(-0b1)));
        expect_integer!(false, "-0b101", Ok(Some(-0b101)));
        expect_integer!(false, "-0b00101", Ok(Some(-0b101)));
        expect_integer!(false, "0b-1", Ok(Some(-0b1)));
        expect_integer!(false, "0b-101", Ok(Some(-0b101)));
        expect_integer!(false, "0b-00101", Ok(Some(-0b101)));
    }

    #[test]
    fn next_label_token_works() {
        macro_rules! expect_label { ( $($x:tt)* ) => {
            expect_tokens!(parse_label(), $($x)*);
        }}

        expect_label!("", Ok(None));
        expect_label!("0x1283", Ok(None));
        expect_label!("!@*)#", Ok(None));
        expect_label!("0Foo", Ok(None));
        expect_label!("Foo!", Err(_));
        expect_label!("F", Ok(Some(label!("F"))));
        expect_label!("Foo", Ok(Some(label!("Foo"))));
        expect_label!("_Foo", Ok(Some(label!("_Foo"))));
        expect_label!("F_oo12", Ok(Some(label!("F_oo12"))));
        expect_label!("Foo12_", Ok(Some(label!("Foo12_"))));
        expect_label!("Foo+0", Ok(Some(label!("Foo", 0))));
        expect_label!("Foo-0", Ok(Some(label!("Foo", 0))));
        expect_label!("Foo+4", Ok(Some(label!("Foo", 4))));
        expect_label!("Foo-43", Ok(Some(label!("Foo", -43))));
        expect_label!("Foo+", Err(_));
        expect_label!("Foo-", Err(_));
        expect_label!("Foo", Ok(Some(label!("Foo"))));
        expect_label!("Foo+4", Ok(Some(label!("Foo", 4))));
        expect_label!("Foo+", Err(_));
        expect_label!("Foo-", Err(_));
        expect_label!("Foo+0x034", Ok(Some(label!("Foo", 0x34))));
        expect_label!("Foo-0o4", Ok(Some(label!("Foo", -4))));
        expect_label!("Foo-#24", Ok(Some(label!("Foo", -24))));
        expect_label!("Foo+#024", Ok(Some(label!("Foo", 24))));
    }
}
