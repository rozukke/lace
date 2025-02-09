mod integer;
mod label;
mod naive;
mod name;

use std::ops::Deref;

use self::integer::Integer;
use super::{error, CommandName, Label, Location, MemoryLocation};
use crate::symbol::Register;

pub use self::naive::NaiveType;

// TODO(refactor): `.peek` -> `.next_if` where possible

type CharIter<'a> = std::iter::Peekable<std::str::Chars<'a>>;

/// Iterator over a command string which yields command-name or argument values.
pub struct Arguments<'a> {
    buffer: &'a str,
    /// Byte index.
    cursor: usize,

    /// Amount of arguments requested (successfully or not).
    ///
    /// Must only be incremented by [`Self::next_str`].
    arg_count: u8,
}

impl<'a> From<&'a str> for Arguments<'a> {
    fn from(buffer: &'a str) -> Self {
        Self {
            buffer,
            cursor: 0,
            arg_count: 0,
        }
    }
}

/// Try to parse a single argument string as an argument value.
///
/// Not used to read [`CommandName`].
trait TryParse<'a>: Sized {
    /// Parse argument string as a specific type.
    fn try_parse(string: &'a str) -> Result<Option<Self>, error::Value>;
}

/// Preliminary type check to bypass normal value parsing if type is known to be mismatched.
///
/// Useful to avoid register values parsing as labels, when requesting a [`MemoryLocation`] (which
/// does not accept registers, and so would otherwise parse "R0" as a label).
///
/// Please do not worry about performance: this method could be slightly faster or slower but it
/// would be negligible.
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

impl<'a> Arguments<'a> {
    /// Amount of arguments requested (successfully or not).
    ///
    /// Does not include [`CommandName`] argument(s).
    pub fn arg_count(&self) -> u8 {
        self.arg_count
    }

    /// Get next argument string, incrementing `arg_count`.
    ///
    /// Not used to read [`CommandName`].
    //
    // Do not `impl Iterator`. This method should be private
    fn next_str(&mut self) -> Option<&'a str> {
        let argument = self.next_str_name()?;
        self.arg_count += 1;
        Some(argument)
    }

    /// Get next argument string, WITHOUT incrementing `arg_count`.
    ///
    /// Only used to read [`CommandName`].
    //
    // TODO(rename): next_str_name
    fn next_str_name(&mut self) -> Option<&'a str> {
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

    /// Take the rest of the command as one string.
    ///
    /// Leading/trailing whitespace is trimmed.
    ///
    /// Only used for "eval" command currently, but could be used for future commands.
    pub fn collect_rest(&mut self) -> &'a str {
        // Do not increment `arg_count`
        let start = self.cursor;
        self.cursor = self.buffer.len();
        self.buffer[start..].trim()
    }

    /// Returns an error if the command string has any arguments left.
    pub fn expect_end(
        &mut self,
        expected_count: u8,
        actual_count: u8,
    ) -> Result<(), error::Argument> {
        if self.next_str().is_none() {
            Ok(())
        } else {
            Err(error::Argument::TooManyArguments {
                expected_count,
                actual_count,
            })
        }
    }

    /// Parse next argument as a `u16`.
    pub fn next_integer(
        &mut self,
        argument_name: &'static str,
        expected_count: u8,
    ) -> Result<u16, error::Argument> {
        let actual_count = self.arg_count();
        self.next_integer_or(
            argument_name,
            Err(error::Argument::MissingArgument {
                argument_name,
                expected_count,
                actual_count,
            }),
        )
    }

    /// Parse next argument as a positive `u16`, defaulting to `1` if no argument is given.
    ///
    /// Non-positive values will be converted to `1`.
    pub fn next_positive_integer_or_default(
        &mut self,
        argument_name: &'static str,
    ) -> Result<u16, error::Argument> {
        self.next_integer_or(argument_name, Ok(1))
            .map(|value| value.max(1)) // 0 -> 1
    }

    /// Parse next argument as a `u16`. Use default `Result` value if no argument is given.
    //
    // Do not change `default` param to a function unless lazy evaluation is ACTUALLY desirable
    fn next_integer_or(
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

        let integer = Integer::try_parse(argument)
            .map_err(error::Argument::invalid_value(argument_name, argument))?;

        if let Some(integer) = integer {
            let integer = integer
                .as_u16_cast()
                .map_err(error::Argument::invalid_value(argument_name, argument))?;
            return Ok(integer);
        };

        Err(error::Argument::invalid_value(argument_name, argument)(
            error::Value::MalformedValue {},
        ))
    }

    /// Parse next argument as a [`Location`]: a [`Register`] or [`MemoryLocation`].
    pub fn next_location(
        &mut self,
        argument_name: &'static str,
        expected_count: u8,
    ) -> Result<Location<'a>, error::Argument> {
        let actual_count = self.arg_count();
        let Some(argument) = self.next_str() else {
            return Err(error::Argument::MissingArgument {
                argument_name,
                expected_count,
                actual_count,
            });
        };

        // Don't perform a preliminary type check here
        // All current valid types are allowed here, and any invalid value will be handled before
        // the end of this function (as `MalformedValue`)

        // TODO(refactor): Can this be moved to `Location::try_parse` ?

        if let Some(register) = Register::try_parse(argument)
            .map_err(error::Argument::invalid_value(argument_name, argument))?
        {
            return Ok(Location::Register(register));
        };

        MemoryLocation::try_parse(argument)
            // `Ok(None)` -> `Err(...)`
            .and_then(|opt| opt.ok_or(error::Value::MalformedValue {}))
            .map_err(error::Argument::invalid_value(argument_name, argument))
            .map(Location::Memory)
    }

    /// Parse next argument as a [`MemoryLocation`].
    pub fn next_memory_location(
        &mut self,
        argument_name: &'static str,
        expected_count: u8,
    ) -> Result<MemoryLocation<'a>, error::Argument> {
        let actual_count = self.arg_count();
        self.next_memory_location_or(
            argument_name,
            Err(error::Argument::MissingArgument {
                argument_name,
                expected_count,
                actual_count,
            }),
        )
    }

    /// Parse next argument as a [`MemoryLocation`], defaulting to current value of program counter
    /// if no argument is given.
    ///
    /// Program counter is returned as [`MemoryLocation::PCOffset`], so will always be correct.
    pub fn next_memory_location_or_default(
        &mut self,
        argument_name: &'static str,
    ) -> Result<MemoryLocation<'a>, error::Argument> {
        self.next_memory_location_or(argument_name, Ok(MemoryLocation::PCOffset(0)))
    }

    /// Parse next argument as a [`MemoryLocation`]. Use default `Result` value if no argument is
    /// given.
    // Do not change `default` param to a function unless lazy evaluation is ACTUALLY desirable
    fn next_memory_location_or(
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

        MemoryLocation::try_parse(argument)
            // `Ok(None)` -> `Err(...)`
            .and_then(|opt| opt.ok_or(error::Value::MalformedValue {}))
            .map_err(error::Argument::invalid_value(argument_name, argument))
    }
}

impl<'a> TryParse<'a> for Register {
    /// Parse argument string as a [`Register`].
    fn try_parse(string: &'a str) -> Result<Option<Self>, error::Value> {
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
            if label::can_contain(ch) {
                return Ok(None);
            } else {
                return Err(error::Value::MalformedRegister {});
            }
        }

        Ok(Some(register))
    }
}

impl<'a> TryParse<'a> for MemoryLocation<'a> {
    /// Parse argument string as a [`MemoryLocation`].
    fn try_parse(argument: &'a str) -> Result<Option<MemoryLocation<'a>>, error::Value> {
        // Ideally order of these checks should match that of [`NaiveType::try_from`] (except for
        // registers)
        // Do not change order unless there is a good reason
        if let Some(offset) = PCOffset::try_parse(argument)? {
            return Ok(Some(MemoryLocation::PCOffset(*offset)));
        };
        // `Integer` must be checked before `Label` to handle prefixes without preceeding zero
        if let Some(address) = Integer::try_parse(argument)? {
            let address = address.as_u16()?;
            return Ok(Some(MemoryLocation::Address(address)));
        };
        if let Some(label) = Label::try_parse(argument)? {
            return Ok(Some(MemoryLocation::Label(label)));
        };
        Ok(None)
    }
}

/// Wrapper type to implement [`TryParse`] for program counter offset.
struct PCOffset(i16);
impl<'a> TryParse<'a> for PCOffset {
    /// Parse argument string as a [`PCOffset`].
    fn try_parse(string: &'a str) -> Result<Option<Self>, error::Value> {
        if string.chars().next().is_none_or(|ch| ch != '^') {
            return Ok(None);
        }
        let offset_str = &string['^'.len_utf8()..];

        let offset = if offset_str.is_empty() {
            0
        } else {
            match Integer::try_parse(offset_str)? {
                Some(offset) => offset.as_i16()?,
                None => return Err(error::Value::MalformedInteger {}),
            }
        };

        Ok(Some(Self(offset)))
    }
}
impl Deref for PCOffset {
    type Target = i16;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::label::parse_label;
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
        let mut iter = Arguments::from(line);

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
            let mut iter = Arguments::from($input);
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
