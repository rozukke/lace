mod integer;
mod label;
mod naive;
mod name;

use std::ops::Deref;

use self::integer::Integer;
use super::{error, CommandName, Label, Location, MemoryLocation};
use crate::symbol::Register;

pub use self::naive::NaiveType;

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
    fn next_argument_str(&mut self) -> Option<&'a str> {
        let argument = self.next_token_str()?;
        self.arg_count += 1;
        Some(argument)
    }

    /// Get next argument string, WITHOUT incrementing `arg_count`.
    ///
    /// Only used to read [`CommandName`].
    fn next_token_str(&mut self) -> Option<&'a str> {
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
    pub fn get_rest(&mut self) -> &'a str {
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
        if self.next_argument_str().is_none() {
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
            Err(error::Argument::Missing {
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
        let Some(argument) = self.next_argument_str() else {
            return default;
        };

        Self::check_naive_type(&[NaiveType::Integer], "integer", argument_name, argument)?;

        let integer = Integer::try_parse(argument)
            .map_err(error::Argument::invalid_value(argument_name, argument))?;

        if let Some(integer) = integer {
            let integer = integer
                .as_u16_cast()
                .map_err(error::Argument::invalid_value(argument_name, argument))?;
            return Ok(integer);
        };

        Err(error::Argument::invalid_value(argument_name, argument)(
            error::Value::Malformed {},
        ))
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
            Err(error::Argument::Missing {
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
        let Some(argument) = self.next_argument_str() else {
            return default;
        };

        Self::check_naive_type(
            &[NaiveType::Integer, NaiveType::Label, NaiveType::PCOffset],
            "integer, label, register, or PC offset",
            argument_name,
            argument,
        )?;

        MemoryLocation::try_parse(argument)
            // `Ok(None)` -> `Err(...)`
            .and_then(|opt| opt.ok_or(error::Value::Malformed {}))
            .map_err(error::Argument::invalid_value(argument_name, argument))
    }

    /// Parse next argument as a [`Location`]: a [`Register`] or [`MemoryLocation`].
    pub fn next_location(
        &mut self,
        argument_name: &'static str,
        expected_count: u8,
    ) -> Result<Location<'a>, error::Argument> {
        let actual_count = self.arg_count();
        let Some(argument) = self.next_argument_str() else {
            return Err(error::Argument::Missing {
                argument_name,
                expected_count,
                actual_count,
            });
        };

        // Don't perform a preliminary type check here
        // All current valid types are allowed here, and any invalid value will be handled before
        // the end of this function (as `MalformedValue`)

        Location::try_parse(argument)
            // `Ok(None)` -> `Err(...)`
            .and_then(|opt| opt.ok_or(error::Value::Malformed {}))
            .map_err(error::Argument::invalid_value(argument_name, argument))
    }

    /// Preliminary type check to bypass normal value parsing if type is known to be mismatched.
    ///
    /// Useful to avoid register values parsing as labels, when requesting a [`MemoryLocation`]
    /// (which does not accept registers, and so would otherwise parse "R0" as a label).
    ///
    /// Please do not worry about performance: this method could be slightly faster or slower but
    /// it would be negligible.
    fn check_naive_type(
        accepted_types: &[NaiveType],
        expected_type: &'static str,
        argument_name: &'static str,
        argument: &str,
    ) -> Result<(), error::Argument> {
        let Ok(actual_type) = NaiveType::try_from(argument) else {
            // Could not discern type:
            // Either an invalid type (which should be handled after a genuine parse attempt for
            // each type, as `MalformedValue`), or a bug in the naive check (which the user can
            // easily ignore)
            return Ok(());
        };

        if accepted_types.contains(&actual_type) {
            return Ok(());
        }

        Err(error::Argument::InvalidValue {
            argument_name,
            string: argument.to_string(),

            error: error::Value::MismatchedType {
                expected_type,
                actual_type,
            },
        })
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

impl<'a> TryParse<'a> for Location<'a> {
    /// Parse argument string as a [`Location`].
    fn try_parse(string: &'a str) -> Result<Option<Self>, error::Value> {
        // Try to parse as register, otherwise try as `MemoryLocation`
        if let Some(register) = Register::try_parse(string)? {
            return Ok(Some(Location::Register(register)));
        };
        Ok(MemoryLocation::try_parse(string)?.map(Location::Memory))
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
    use super::*;

    #[test]
    fn many_arguments_works() {
        let line = "  name  -54  r3 0x5812 Foo name2  Baz+0x04 4209";
        let mut iter = Arguments::from(line);

        let argument_name = "dummy";
        let expected_count = 99;

        assert_eq!(iter.next_argument_str(), Some("name"));
        assert_eq!(iter.next_integer(argument_name, 99), Ok(-54i16 as u16));
        assert_eq!(
            iter.next_location(argument_name, 99),
            Ok(Location::Register(Register::R3)),
        );
        assert_eq!(iter.next_integer(argument_name, expected_count), Ok(0x5812));
        assert_eq!(
            iter.next_memory_location(argument_name, expected_count),
            Ok(MemoryLocation::Label(Label::new("Foo", 0))),
        );
        assert_eq!(iter.next_argument_str(), Some("name2"));
        assert_eq!(
            iter.next_memory_location(argument_name, expected_count),
            Ok(MemoryLocation::Label(Label::new("Baz", 0x04))),
        );
        assert_eq!(iter.next_integer(argument_name, expected_count), Ok(4209));
        assert_eq!(iter.next_argument_str(), None);
        assert_eq!(iter.expect_end(expected_count, 100), Ok(()));
    }

    #[test]
    #[should_panic]
    #[cfg(debug_assertions)]
    fn semicolon_fails_assert() {
        let argument_name = "dummy";
        let expected_count = 99;
        let mut iter = Arguments::from("  ;  ");
        let _ = iter.next_integer(argument_name, expected_count);
    }

    #[test]
    fn try_parse_location() {
        fn expect_location(input: &str, expected: Result<Option<Location>, ()>) {
            println!("{:?}", input);
            let result = Location::try_parse(input).map_err(|_| ());
            assert_eq!(result, expected);
        }

        expect_location("", Ok(None));
        expect_location("!@#", Ok(None));
        expect_location("!@#", Ok(None));

        expect_location("-0x1283", Err(()));
        expect_location("12312031283", Err(()));
        expect_location("^-", Err(()));
        expect_location("^a", Err(()));
        expect_location("ra$", Err(()));
        expect_location("a!", Err(()));
        expect_location("a--23", Err(()));
        expect_location("a-x-23", Err(()));

        expect_location("r0", Ok(Some(Location::Register(Register::R0))));
        expect_location("R6", Ok(Some(Location::Register(Register::R6))));
        expect_location("R7", Ok(Some(Location::Register(Register::R7))));

        expect_location(
            "123",
            Ok(Some(Location::Memory(MemoryLocation::Address(123)))),
        );
        expect_location("#1", Ok(Some(Location::Memory(MemoryLocation::Address(1)))));
        expect_location(
            "0xdeAD",
            Ok(Some(Location::Memory(MemoryLocation::Address(0xdead)))),
        );
        expect_location(
            "+o17",
            Ok(Some(Location::Memory(MemoryLocation::Address(0o17)))),
        );
        expect_location(
            "xaf",
            Ok(Some(Location::Memory(MemoryLocation::Address(0xaf)))),
        );

        expect_location(
            "r8",
            Ok(Some(Location::Memory(MemoryLocation::Label(Label::new(
                "r8", 0,
            ))))),
        );
        expect_location(
            "xag",
            Ok(Some(Location::Memory(MemoryLocation::Label(Label::new(
                "xag", 0,
            ))))),
        );
        expect_location(
            "foo+1",
            Ok(Some(Location::Memory(MemoryLocation::Label(Label::new(
                "foo", 1,
            ))))),
        );
        expect_location(
            "foo-0x01",
            Ok(Some(Location::Memory(MemoryLocation::Label(Label::new(
                "foo", -1,
            ))))),
        );

        expect_location("^", Ok(Some(Location::Memory(MemoryLocation::PCOffset(0)))));
        expect_location(
            "^0x7ffF",
            Ok(Some(Location::Memory(MemoryLocation::PCOffset(0x7fff)))),
        );
        expect_location(
            "^+#19",
            Ok(Some(Location::Memory(MemoryLocation::PCOffset(19)))),
        );
    }

    #[test]
    fn try_parse_memory_location() {
        fn expect_memory_location(input: &str, expected: Result<Option<MemoryLocation>, ()>) {
            println!("{:?}", input);
            let result = MemoryLocation::try_parse(input).map_err(|_| ());
            assert_eq!(result, expected);
        }

        expect_memory_location("", Ok(None));
        expect_memory_location("!@#", Ok(None));
        expect_memory_location("!@#", Ok(None));

        expect_memory_location("-0x1283", Err(()));
        expect_memory_location("12312031283", Err(()));
        expect_memory_location("^-", Err(()));
        expect_memory_location("^a", Err(()));
        expect_memory_location("ra$", Err(()));
        expect_memory_location("a!", Err(()));
        expect_memory_location("a--23", Err(()));
        expect_memory_location("a-x-23", Err(()));

        expect_memory_location("123", Ok(Some(MemoryLocation::Address(123))));
        expect_memory_location("#1", Ok(Some(MemoryLocation::Address(1))));
        expect_memory_location("0xdeAD", Ok(Some(MemoryLocation::Address(0xdead))));
        expect_memory_location("+o17", Ok(Some(MemoryLocation::Address(0o17))));
        expect_memory_location("xaf", Ok(Some(MemoryLocation::Address(0xaf))));

        // Caller is responsible for avoiding registers parsing as labels
        expect_memory_location("r0", Ok(Some(MemoryLocation::Label(Label::new("r0", 0)))));

        expect_memory_location("r8", Ok(Some(MemoryLocation::Label(Label::new("r8", 0)))));
        expect_memory_location("xag", Ok(Some(MemoryLocation::Label(Label::new("xag", 0)))));
        expect_memory_location(
            "foo+1",
            Ok(Some(MemoryLocation::Label(Label::new("foo", 1)))),
        );
        expect_memory_location(
            "foo-0x01",
            Ok(Some(MemoryLocation::Label(Label::new("foo", -1)))),
        );

        expect_memory_location("^", Ok(Some(MemoryLocation::PCOffset(0))));
        expect_memory_location("^0x7ffF", Ok(Some(MemoryLocation::PCOffset(0x7fff))));
        expect_memory_location("^+#19", Ok(Some(MemoryLocation::PCOffset(19))));
    }

    #[test]
    fn try_parse_register() {
        fn expect_register(input: &str, expected: Result<Option<Register>, ()>) {
            println!("{:?}", input);
            let result = Register::try_parse(input).map_err(|_| ());
            assert_eq!(result, expected);
        }

        expect_register("", Ok(None));
        expect_register("r8", Ok(None));
        expect_register("r1a", Ok(None));
        expect_register("R00", Ok(None));
        expect_register("r1@", Err(()));
        expect_register("R6...", Err(()));
        expect_register("r0", Ok(Some(Register::R0)));
        expect_register("r7", Ok(Some(Register::R7)));
        expect_register("R7", Ok(Some(Register::R7)));
    }

    #[test]
    fn try_parse_pc_offset() {
        fn expect_pc_offset(input: &str, expected: Result<Option<i16>, ()>) {
            println!("{:?}", input);
            let result = match PCOffset::try_parse(input) {
                Ok(opt) => Ok(opt.map(|integer| *integer)),
                Err(_) => Err(()),
            };
            assert_eq!(result, expected);
        }

        expect_pc_offset("", Ok(None));
        expect_pc_offset("0x1", Ok(None));
        expect_pc_offset("xaa", Ok(None));
        expect_pc_offset("!@#", Ok(None));

        expect_pc_offset("^0x8000", Err(()));
        expect_pc_offset("^-", Err(()));
        expect_pc_offset("^+", Err(()));
        expect_pc_offset("^#", Err(()));
        expect_pc_offset("^abc", Err(()));
        expect_pc_offset("^123abc", Err(()));
        expect_pc_offset("^x", Err(()));
        expect_pc_offset("^-x", Err(()));
        expect_pc_offset("^+x", Err(()));
        expect_pc_offset("^00x1", Err(()));

        expect_pc_offset("^", Ok(Some(0)));
        expect_pc_offset("^123", Ok(Some(123)));
        expect_pc_offset("^0x7ffF", Ok(Some(0x7fff)));
        expect_pc_offset("^-o123", Ok(Some(-0o123)));
        expect_pc_offset("^+#19", Ok(Some(19)));
    }
}
