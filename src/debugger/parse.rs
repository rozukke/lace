use super::command::{CommandName, Error, Label, Location, MemoryLocation};
use crate::symbol::Register;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
enum Argument {
    Register(Register),
    Integer(i32),
    Label(Label),
}

#[derive(Clone, Copy, Debug)]
enum Sign {
    Positive = 1,
    Negative = -1,
}

#[derive(Clone, Copy, Debug)]
enum Radix {
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

/// Try to convert an `i32` into another integer type.
fn resize_int<T: TryFrom<i32>>(integer: i32) -> Result<T> {
    integer.try_into().map_err(|_| Error::IntegerTooLarge)
}

/// Returns `true` if `name` matchs any item of `candidates` (case insensitive).
fn matches(name: &str, candidates: &[&str]) -> bool {
    for candidate in candidates {
        if name.eq_ignore_ascii_case(candidate) {
            return true;
        }
    }
    false
}

/// Returns the first [`CommandName`], which has a corresponding candidate which matches `name`(case insensitive).
///
/// Returns `None` if no match was found.
fn find_match(name: &str, commands: &[(CommandName, &[&str])]) -> Option<CommandName> {
    for (command, candidates) in commands {
        if matches(name, candidates) {
            return Some(*command);
        }
    }
    None
}

pub struct CommandIter<'a> {
    buffer: &'a str,
    /// Characters before this index have been successfully parsed.
    base: usize,
    /// Characters between base..head are currently being parsed.
    head: usize,
}

impl<'a> CommandIter<'a> {
    pub fn from(buffer: &'a str) -> Self {
        Self {
            buffer,
            base: 0,
            head: 0,
        }
    }

    /// Parse and consume command name.
    ///
    /// Considers multi-word command names (i.e. subcommands) as one name. Eg. `break add`.
    pub fn get_command_name(&mut self) -> Result<CommandName> {
        let name = self.next_command_name_part();
        // Command source should always return a string containing non-whitespace
        // characters, so initial command name should always exist.
        debug_assert!(name.is_some(), "missing command name");
        let name = name.unwrap_or("");

        // TODO(feat): Add more aliases (such as undocumented typo aliases)
        let commands: &[(_, &[_])] = &[
            (CommandName::Help, &["help", "--help", "h", "-h"]),
            (CommandName::Continue, &["continue", "cont", "c"]),
            (CommandName::Finish, &["finish", "fin", "f"]),
            (CommandName::Exit, &["exit", "e"]),
            (CommandName::Quit, &["quit", "q"]),
            (CommandName::Registers, &["registers", "reg", "r"]),
            (CommandName::Reset, &["reset"]),
            (CommandName::Step, &["step", "t"]),
            (CommandName::Next, &["next", "n"]),
            (CommandName::Get, &["get", "g"]),
            (CommandName::Set, &["set", "s"]),
            (CommandName::Jump, &["jump", "j"]),
            (CommandName::Source, &["source", "o"]),
            (CommandName::Eval, &["eval", "v"]),
            (CommandName::BreakList, &["breaklist", "bl"]),
            (CommandName::BreakAdd, &["breakadd", "ba"]),
            (CommandName::BreakRemove, &["breakremove", "br"]),
        ];

        if let Some(command) = find_match(name, commands) {
            return Ok(command);
        };

        // This could be written a bit nicer. But it doesn't seem necessary.
        if matches(name, &["break", "b"]) {
            let name = name.to_string();
            let Some(subname) = self.next_command_name_part() else {
                return Err(Error::MissingSubcommand { name });
            };

            if let Some(command) = find_match(
                subname,
                &[
                    (CommandName::BreakList, &["list", "l"]),
                    (CommandName::BreakAdd, &["add", "a"]),
                    (CommandName::BreakRemove, &["remove", "r"]),
                ],
            ) {
                return Ok(command);
            }

            return Err(Error::InvalidSubcommand {
                name,
                subname: subname.to_string(),
            });
        }

        Err(Error::InvalidCommandName {
            name: name.to_string(),
        })
    }

    /// Parse and consume next integer argument.
    pub fn next_integer(&mut self, name: CommandName) -> Result<u16> {
        Ok(match self.next_argument()? {
            Some(Argument::Integer(count)) => resize_int(count)?,
            None => return Err(Error::MissingArgument { name }),
            _ => return Err(Error::WrongArgumentKind { name }),
        })
    }

    /// Parse and consume next positive integer argument, defaulting to `1`.
    pub fn next_positive_integer_or_default(&mut self, name: CommandName) -> Result<u16> {
        Ok(match self.next_argument()? {
            Some(Argument::Integer(count)) => resize_int(count.max(1))?,
            None => 1,
            _ => return Err(Error::WrongArgumentKind { name }),
        })
    }

    /// Parse and consume next [`Location`] argument: a register or [`MemoryLocation`].
    pub fn next_location(&mut self, name: CommandName) -> Result<Location> {
        Ok(match self.next_argument()? {
            Some(Argument::Register(register)) => Location::Register(register),
            Some(Argument::Integer(address)) => {
                Location::Memory(MemoryLocation::Address(resize_int(address)?))
            }
            Some(Argument::Label(label)) => Location::Memory(MemoryLocation::Label(label)),
            None => return Err(Error::MissingArgument { name }),
        })
    }

    /// Parse and consume next [`MemoryLocation`] argument.
    pub fn next_memory_location(&mut self, name: CommandName) -> Result<MemoryLocation> {
        Ok(match self.next_argument()? {
            Some(Argument::Integer(address)) => MemoryLocation::Address(resize_int(address)?),
            Some(Argument::Label(label)) => MemoryLocation::Label(label),
            None => return Err(Error::MissingArgument { name }),
            _ => return Err(Error::WrongArgumentKind { name }),
        })
    }

    /// Parse and consume next [`MemoryLocation`] argument, defaulting to program counter
    /// ([`MemoryLocation::PC`]).
    pub fn next_memory_location_or_default(&mut self, name: CommandName) -> Result<MemoryLocation> {
        Ok(match self.next_argument()? {
            Some(Argument::Integer(address)) => MemoryLocation::Address(resize_int(address)?),
            Some(Argument::Label(label)) => MemoryLocation::Label(label),
            None => MemoryLocation::PC,
            _ => return Err(Error::WrongArgumentKind { name }),
        })
    }

    /// Returns an error if the command contains any arguments which haven't been consumed.
    pub fn expect_end_of_command(&mut self, name: CommandName) -> Result<()> {
        self.skip_whitespace();
        let ch = self.peek();
        debug_assert!(
            !matches!(ch, Some(';' | '\n')),
            "semicolons/newlines should have been handled already"
        );
        if !matches!(ch, None | Some(';' | '\n')) {
            return Err(Error::TooManyArguments { name });
        }
        Ok(())
    }

    /// Consume the rest of the command as one string.
    ///
    /// Used for `eval` command.
    pub fn collect_rest(&mut self) -> String {
        let rest = self.buffer[self.head..].trim().to_string();
        self.head = self.buffer.len();
        rest
    }

    /// Get next character at head, WITHOUT incrementing head.
    fn peek(&self) -> Option<char> {
        if self.head >= self.buffer.len() {
            return None;
        }
        let next = self.buffer[self.head..].chars().next()?;
        Some(next)
    }
    /// Get next character at head, incrementing head.
    fn next(&mut self) -> Option<char> {
        let next = self.peek()?;
        self.head += next.len_utf8();
        Some(next)
    }

    /// Get characters between base..head, WITHOUT updating base.
    fn get(&self) -> &str {
        assert!(self.base <= self.head, "base exceeded head");
        &self.buffer[self.base..self.head]
    }
    /// Get characters between base..head, updating base.
    fn take(&mut self) -> &str {
        assert!(self.base <= self.head, "base exceeded head");
        let slice = &self.buffer[self.base..self.head];
        self.set_base();
        slice
    }

    /// Update base to head.
    fn set_base(&mut self) {
        self.base = self.head;
    }
    /// Backtrack head to base.
    fn reset_head(&mut self) {
        self.head = self.base;
    }

    fn is_end_of_argument(&self) -> bool {
        let ch = self.peek();
        debug_assert!(
            !matches!(ch, Some(';' | '\n')),
            "semicolons/newlines should have been handled already"
        );
        matches!(ch, None | Some(' ' | ';' | '\n'))
    }

    /// Consume all whitespace before next non-whitespace character.
    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(|ch| ch.is_whitespace()) {
            self.next();
        }
        self.set_base();
    }

    /// Used for both main command and subcommand (eg. `break add`).
    fn next_command_name_part(&mut self) -> Option<&str> {
        self.skip_whitespace();
        self.reset_head();

        while self.peek().is_some_and(|ch| !ch.is_whitespace()) {
            self.next();
        }

        if self.get().is_empty() {
            return None;
        }
        Some(self.take())
    }

    /// Parse and consume the next [`Argument`].
    fn next_argument(&mut self) -> Result<Option<Argument>> {
        debug_assert!(
            self.head == self.base,
            "should have been called with head==base"
        );
        self.reset_head();
        self.skip_whitespace();

        if self.is_end_of_argument() {
            return Ok(None);
        }
        if let Some(register) = self.next_register() {
            return Ok(Some(Argument::Register(register)));
        }
        if let Some(integer) = self.next_integer_token(false)? {
            return Ok(Some(Argument::Integer(integer)));
        }
        if let Some(label) = self.next_label_token()? {
            return Ok(Some(Argument::Label(label)));
        }
        Err(Error::MalformedArgument)
    }

    /// Parse and consume the next [`Register`] argument.
    fn next_register(&mut self) -> Option<Register> {
        self.reset_head();
        // Don't skip whitespace

        if !self.next().is_some_and(|ch| ch == 'r' || ch == 'R') {
            return None;
        }
        let register = match self.next()? {
            '0' => Register::R0,
            '1' => Register::R1,
            '2' => Register::R2,
            '3' => Register::R3,
            '4' => Register::R4,
            '5' => Register::R5,
            '6' => Register::R6,
            '7' => Register::R7,
            _ => return None,
        };

        // Possibly the start of a label
        if !self.is_end_of_argument() {
            return None;
        }
        self.set_base();
        Some(register)
    }

    /// Parse and consume the next integer argument.
    ///
    /// Extremely liberal in accepted syntax.
    ///
    /// Accepts:
    ///  - Decimal (optional `#`), hex (`x`/`X`), octal (`o`/`O`), and binary (`b`/`B`)
    ///  - Optional single zero before non-decimal radix prefix. Eg. `0x4`
    ///  - Leading zeros after prefix and sign. Eg. `0x0004`, `#-03`
    ///  - Sign character before xor after radix prefix. Eg. `-#2`, `x+4`
    ///
    /// Returns `Ok(None)` (not an integer) for:
    ///  - Empty token
    ///  - Non-decimal radix prefix, with no zero before it, and non-digits after it. Eg. `xLabel`, `o`
    ///
    /// Returns `Err` (invalid integer and invalid token) for:
    ///  - Invalid digits for the given radix
    ///  - Decimal radix prefix `#` with zeros before it. Eg. `0#2`
    ///  - Decimal radix prefix `#` with no digits after it. Eg. `#`
    ///  - Multiple sign characters (before or after prefix)
    ///  - Missing sign character '-' or '+', if `require_sign == true`
    ///  - Multiple zeros before radix prefix. Eg. `00x4`
    ///  - Absolute value out of bounds for `i32`. (Does *NOT* check if integer fits in specific bit size)
    fn next_integer_token(&mut self, require_sign: bool) -> Result<Option<i32>> {
        self.reset_head();
        // Don't skip whitespace

        // Take sign BEFORE prefix
        let first_sign: Option<Sign> = self.next_integer_sign();

        // Take optional prefix
        let Some((radix, has_leading_zeros, prefix_is_symbol)) = self.next_integer_prefix()? else {
            // Sign was already given, so it must be an invalid token
            if first_sign.is_some() {
                return Err(Error::MalformedInteger);
            }
            return Ok(None);
        };

        // Take sign AFTER prefix
        let second_sign = self.next_integer_sign();
        let sign = match (first_sign, second_sign) {
            (Some(sign), None) => Some(sign),
            (None, Some(sign)) => Some(sign),
            (None, None) => {
                if require_sign {
                    return Err(Error::MalformedInteger);
                }
                None
            }
            // Disallow multiple sign characters: '-x-...', '++...', etc
            (Some(_), Some(_)) => return Err(Error::MalformedInteger),
        };

        // Check next character is digit
        if self
            .peek().is_none_or(|ch| radix.parse_digit(ch).is_none())
        {
            // Sign, '#', or pre-prefix zeros were given, so it must be an invalid integer token
            if sign.is_some() || has_leading_zeros || prefix_is_symbol {
                return Err(Error::MalformedInteger);
            }
            return Ok(None);
        };

        // Take digits until non-digit character
        // Note that this loop handles post-prefix leading zeros like any other digit
        let mut integer: i32 = 0;
        while let Some(ch) = self.peek() {
            if self.is_end_of_argument() {
                break;
            }
            let Some(digit) = radix.parse_digit(ch) else {
                return Err(Error::MalformedInteger);
            };
            self.next();

            // Re-checked later on convert to smaller int types
            if integer > i32::MAX / radix as i32 {
                return Err(Error::IntegerTooLarge);
            }

            integer *= radix as i32;
            integer += digit as i32;
        }
        if let Some(sign) = sign {
            integer *= sign as i32;
        }

        if !self.is_end_of_argument() {
            return Err(Error::MalformedInteger);
        }
        self.set_base();
        Ok(Some(integer))
    }

    /// Consume the sign character for an integer.
    /// Must only be called by `next_token_integer`.
    fn next_integer_sign(&mut self) -> Option<Sign> {
        // Don't reset head
        // Don't skip whitespace

        let sign = match self.peek() {
            Some('-') => Sign::Negative,
            Some('+') => Sign::Positive,
            _ => return None,
        };

        self.next();
        self.set_base();
        Some(sign)
    }

    /// Get radix from integer prefix.
    /// Must only be called by `next_token_integer`.
    ///
    /// Returns radix, whether leading zeros are included, and whether radix prefix is a
    /// non-alphabetic symbol (i.e. `#`).
    // TODO(refactor): Possibly return a named struct type
    fn next_integer_prefix(&mut self) -> Result<Option<(Radix, bool, bool)>> {
        // Don't reset head
        // Don't skip whitespace

        // Take single leading zero before prefix
        let has_leading_zeros = self.peek().is_some_and(|ch| ch == '0');
        if has_leading_zeros {
            self.next();
        }

        // Number is all zeroes (no radix prefix)
        // Zeroes were taken as leading zeros
        if has_leading_zeros && self.is_end_of_argument() {
            self.reset_head();
            return Ok(Some((Radix::Decimal, true, false)));
        }

        let mut next_char = true; // Whether to increment head for prefix character
        let (radix, prefix_is_symbol) = match self.peek() {
            Some('#') => {
                // Disallow '0#...'
                if has_leading_zeros {
                    return Err(Error::MalformedInteger);
                }
                (Radix::Decimal, true)
            }
            // Allow 'b...' or 'x...'
            // Caller must check next characters are valid digits in the radix, so as to not parse
            // non-integer tokens like 'xLabel' as integers (and fail)
            Some('b' | 'B') => (Radix::Binary, false),
            Some('o' | 'O') => (Radix::Octal, false),
            Some('x' | 'X') => (Radix::Hex, false),
            // No prefix. Don't skip character
            Some('0'..='9') => {
                next_char = false;
                (Radix::Decimal, false)
            }
            Some('-' | '+') => {
                // Disallow '0-...' and '0+...'
                // Disallow '--...', '-+...', etc
                return Err(Error::MalformedInteger);
            }
            // Not an integer
            _ => return Ok(None),
        };
        if next_char {
            self.next(); // Skip prefix character
        }

        // Don't set base; might not be an integer yet
        Ok(Some((radix, has_leading_zeros, prefix_is_symbol)))
    }

    /// Returns `true` if the given character can appear at the start of a label.
    fn label_can_start_with(ch: char) -> bool {
        matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
    }
    /// Returns `true` if the given character can appear as a subsequent character of a label.
    fn label_can_contain(ch: char) -> bool {
        matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
    }

    /// Consume the next [`Label`] argument.
    fn next_label_token(&mut self) -> Result<Option<Label>> {
        self.reset_head();
        // Don't skip whitespace

        // Check first character can begin a label
        if !self.next().is_some_and(Self::label_can_start_with) {
            return Ok(None);
        };
        // Take characters until non-alphanumeric
        while self.peek().is_some_and(Self::label_can_contain) {
            self.next();
        }

        let name = self.take().to_string();
        let offset = resize_int(self.next_integer_token(true)?.unwrap_or(0))?;

        if !self.is_end_of_argument() {
            return Err(Error::MalformedLabel);
        }
        self.set_base();
        Ok(Some(Label { name, offset }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn many_arguments_works() {
        let line = "  name  -54  r3 0x5812 Foo name2  Bar+0x04 4209";
        let mut iter = CommandIter::from(line);

        assert_eq!(iter.next_command_name_part(), Some("name"));
        assert_eq!(iter.next_argument(), Ok(Some(Argument::Integer(-54))));
        assert_eq!(
            iter.next_argument(),
            Ok(Some(Argument::Register(Register::R3)))
        );
        assert_eq!(iter.next_argument(), Ok(Some(Argument::Integer(0x5812))));
        assert_eq!(
            iter.next_argument(),
            Ok(Some(Argument::Label(Label {
                name: "Foo".into(),
                offset: 0,
            })))
        );
        assert_eq!(iter.next_command_name_part(), Some("name2"));
        assert_eq!(
            iter.next_argument(),
            Ok(Some(Argument::Label(Label {
                name: "Bar".into(),
                offset: 0x04,
            })))
        );
        assert_eq!(iter.next_argument(), Ok(Some(Argument::Integer(4209))));
        assert_eq!(iter.next_argument(), Ok(None));
        assert_eq!(iter.next_argument(), Ok(None));
    }

    macro_rules! expect_tokens {
        ( $method:ident ($($args:tt)*), $input:expr, $($expected:tt)* ) => {{
            eprintln!("Test input: <{}>", $input);
            let mut iter = CommandIter::from($input);
            let result = iter.$method($($args)*);
            expect_tokens!(@expected result, $($expected)*);
        }};
        (@expected $result:expr, Err(_)) => {
            assert!($result.is_err());
        };
        (@expected $result:expr, $expected:expr) => {
            assert_eq!($result, $expected, stringify!($expected));
        };
    }

    macro_rules! label {
        ( $name:expr $(, $offset:expr )? $(,)? ) => {
            Label {
                name: ($name).into(),
                offset: label!(@offset $($offset)?),
            }
        };
        (@offset $offset:expr) => { $offset };
        (@offset) => { 0 };
    }

    #[test]
    fn next_argument_works() {
        macro_rules! expect_argument { ( $($x:tt)* ) => {
            expect_tokens!(next_argument(), $($x)*);
        }}
        expect_argument!("", Ok(None));
        expect_argument!("   ", Ok(None));
        expect_argument!("r0", Ok(Some(Argument::Register(Register::R0))));
        expect_argument!("   R3  Foo", Ok(Some(Argument::Register(Register::R3))));
        expect_argument!("123", Ok(Some(Argument::Integer(123))));
        expect_argument!("  123  ", Ok(Some(Argument::Integer(123))));
        expect_argument!("123 Foo", Ok(Some(Argument::Integer(123))));
        expect_argument!("0x-853", Ok(Some(Argument::Integer(-0x853))));
        expect_argument!("Foo  ", Ok(Some(Argument::Label(label!("Foo")))));
        expect_argument!("Foo-23", Ok(Some(Argument::Label(label!("Foo", -23)))));
        expect_argument!("  Foo 23", Ok(Some(Argument::Label(label!("Foo")))));
    }

    #[test]
    #[should_panic]
    fn semicolon_panics() {
        expect_tokens!(next_argument(), "  ;  ", Err(_));
    }

    #[test]
    fn next_register_works() {
        macro_rules! expect_register { ( $($x:tt)* ) => {
            expect_tokens!(next_register(), $($x)*);
        }}

        expect_register!("", None);
        expect_register!("a", None);
        expect_register!("rn", None);
        expect_register!("r8", None);
        expect_register!("R0n", None);
        expect_register!("r0n", None);
        expect_register!("r0", Some(Register::R0));
        expect_register!("R7", Some(Register::R7));
    }

    #[test]
    fn next_integer_token_works() {
        macro_rules! expect_integer { ( $require_sign:expr, $($x:tt)* ) => {
            expect_tokens!(next_integer_token($require_sign), $($x)*);
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
            expect_tokens!(next_label_token(), $($x)*);
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
        expect_label!("Foo  ", Ok(Some(label!("Foo"))));
        expect_label!("Foo+4  ", Ok(Some(label!("Foo", 4))));
        expect_label!("Foo-4  !!", Ok(Some(label!("Foo", -4))));
        expect_label!("Foo+  ", Err(_));
        expect_label!("Foo-  ", Err(_));
        expect_label!("Foo -4", Ok(Some(label!("Foo"))));
        expect_label!("Foo +4", Ok(Some(label!("Foo"))));
        expect_label!("Foo+0x034", Ok(Some(label!("Foo", 0x34))));
        expect_label!("Foo-0o4", Ok(Some(label!("Foo", -4))));
        expect_label!("Foo-#24", Ok(Some(label!("Foo", -24))));
        expect_label!("Foo+#024", Ok(Some(label!("Foo", 24))));
    }
}
