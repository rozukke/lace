use crate::symbol::Register;

#[allow(dead_code)]
#[derive(Debug)]
pub enum Command {
    Step {
        count: u16,
    },
    Next {
        count: u16,
    },
    Continue,
    Finish,
    Quit,
    Exit,
    BreakList,
    BreakAdd {
        location: MemoryLocation,
    },
    BreakRemove {
        location: MemoryLocation,
    },
    Get {
        location: Location,
    },
    Set {
        location: Location,
        value: u16,
    },
    Registers,
    Reset,
    Source {
        count: u16,
        location: MemoryLocation,
    },
    Eval(EvalInstruction),
}

// `Location::Memory(MemoryLocation::PC)` is valid, but should not be constructed
#[derive(Debug)]
pub enum Location {
    Register(Register),
    Memory(MemoryLocation),
}

#[derive(Debug)]
pub enum MemoryLocation {
    PC,
    Address(u16),
    Label(Label),
}

#[derive(Debug, PartialEq)]
pub struct Label {
    name: String,
    offset: i16,
}

#[derive(Debug)]
pub enum EvalInstruction {}

// TODO(refactor): Rename these variants
#[derive(Debug, PartialEq)]
pub enum Error {
    MissingCommandName,
    InvalidCommandName,
    MissingArgument,
    InvalidArgumentKind,
    TooManyArguments,
    InvalidArgument,
    InvalidInteger,
    InvalidLabel,
    IntegerTooLarge,
}

#[derive(Debug, PartialEq)]
enum Argument {
    Register(Register),
    Integer(i32),
    Label(Label),
}

#[derive(Clone, Copy, Debug)]
enum Radix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hex = 16,
}

type Result<T> = std::result::Result<T, Error>;

impl TryFrom<&str> for Command {
    type Error = Error;

    fn try_from(line: &str) -> std::result::Result<Self, Self::Error> {
        let mut iter = CommandIter::from(line);

        // TODO(fix): Check bounds for integer arguments
        // TODO(feat): Add more aliases (such as undocumented typo aliases)
        let name = iter.take_command_name()?;
        let command = match name.to_lowercase().as_str() {
            "continue" | "c" => Self::Continue,
            "finish" | "f" => Self::Finish,
            "exit" | "e" => Self::Exit,
            "quit" | "q" => Self::Quit,
            "registers" | "r" => Self::Registers,
            "reset" => Self::Reset,

            "step" | "t" => {
                let count = iter.take_positive_integer_optional()?;
                Self::Step { count }
            }
            "next" | "n" => {
                let count = iter.take_positive_integer_optional()?;
                Self::Next { count }
            }
            "get" | "g" => {
                let location = iter.take_location()?;
                Self::Get { location }
            }
            "set" | "s" => {
                let location = iter.take_location()?;
                let value = iter.take_integer_raw()?;
                Self::Set { location, value }
            }
            "source" => {
                let count = iter.take_positive_integer_optional()?;
                let location = iter.take_memory_location_or_pc()?;
                Self::Source { count, location }
            }

            "break" | "b" => {
                let subname = iter.take_command_name()?;
                match subname.to_lowercase().as_str() {
                    "list" | "l" => Self::BreakList,
                    "add" | "a" => {
                        let location = iter.take_memory_location_or_pc()?;
                        Self::BreakAdd { location }
                    }
                    "remove" | "r" => {
                        let location = iter.take_memory_location_or_pc()?;
                        Self::BreakRemove { location }
                    }
                    _ => return Err(Error::InvalidCommandName),
                }
            }
            "breaklist" | "bl" => Self::BreakList,
            "breakadd" | "ba" => {
                let location = iter.take_memory_location_or_pc()?;
                Self::BreakAdd { location }
            }
            "breakremove" | "br" => {
                let location = iter.take_memory_location_or_pc()?;
                Self::BreakRemove { location }
            }

            "eval" => {
                eprintln!("unimplemented: eval command");
                return Err(Error::InvalidCommandName);
            }

            _ => return Err(Error::InvalidCommandName),
        };

        // All commands except `eval`
        if iter.take_argument()?.is_some() {
            return Err(Error::TooManyArguments);
        }

        Ok(command)
    }
}

struct CommandIter<'a> {
    buffer: &'a str,
    /// Characters before this index have been successfully parsed
    base: usize,
    /// Characters between base..head are currently being parsed
    head: usize,
}

impl<'a> CommandIter<'a> {
    pub fn from(line: &'a str) -> Self {
        Self {
            buffer: line,
            base: 0,
            head: 0,
        }
    }

    /// Get next character at head, incrementing head
    fn next(&mut self) -> Option<char> {
        // TODO(fix): This logic might be incorrect
        if self.head >= self.buffer.len() {
            return None;
        }
        let next = self.buffer[self.head..].chars().next()?;
        self.head += next.len_utf8();
        Some(next)
    }

    /// Get next character at head, WITHOUT incrementing head
    fn peek(&self) -> Option<char> {
        if self.head >= self.buffer.len() {
            return None;
        }
        let next = self.buffer[self.head..].chars().next()?;
        Some(next)
    }

    /// Get characters between base..head, setting base <- head
    fn take(&mut self) -> &str {
        assert!(self.base <= self.head, "base exceeded head");
        let slice = &self.buffer[self.base..self.head];
        self.set_base();
        slice
    }

    /// Get characters between base..head, WITHOUT setting base <- head
    fn get(&self) -> &str {
        assert!(self.base <= self.head, "base exceeded head");
        &self.buffer[self.base..self.head]
    }

    /// Set base <- head
    fn set_base(&mut self) {
        self.base = self.head;
    }

    /// Set head <- base
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

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if !ch.is_whitespace() {
                break;
            }
            self.next();
        }
        self.set_base();
    }

    pub fn take_command_name(&mut self) -> Result<&str> {
        self.skip_whitespace();
        self.reset_head();

        while let Some(ch) = self.peek() {
            if !ch.is_alphanumeric() {
                break;
            }
            self.next();
        }

        if self.get().is_empty() {
            return Err(Error::MissingCommandName);
        }
        Ok(self.take())
    }

    pub fn take_argument(&mut self) -> Result<Option<Argument>> {
        debug_assert!(
            self.head == self.base,
            "should have been called with head==base"
        );
        self.reset_head();
        self.skip_whitespace();

        if self.is_end_of_argument() {
            return Ok(None);
        }
        if let Some(register) = self.take_register() {
            return Ok(Some(Argument::Register(register)));
        }
        if let Some(integer) = self.take_token_integer(true)? {
            return Ok(Some(Argument::Integer(integer)));
        }
        if let Some(label) = self.take_token_label()? {
            return Ok(Some(Argument::Label(label)));
        }
        Err(Error::InvalidArgument)
    }

    pub fn take_integer_raw(&mut self) -> Result<u16> {
        Ok(match self.take_argument()? {
            None => return Err(Error::MissingArgument),
            Some(Argument::Integer(count)) => check_bounds(count)?,
            _ => return Err(Error::InvalidArgumentKind),
        })
    }

    pub fn take_positive_integer_optional(&mut self) -> Result<u16> {
        Ok(match self.take_argument()? {
            None => 1,
            Some(Argument::Integer(count)) => check_bounds(count.max(1))?,
            _ => return Err(Error::InvalidArgumentKind),
        })
    }

    pub fn take_location(&mut self) -> Result<Location> {
        Ok(match self.take_argument()? {
            None => return Err(Error::MissingArgument),
            Some(Argument::Register(register)) => Location::Register(register),
            Some(Argument::Integer(address)) => {
                Location::Memory(MemoryLocation::Address(check_bounds(address)?))
            }
            Some(Argument::Label(label)) => Location::Memory(MemoryLocation::Label(label)),
        })
    }

    pub fn take_memory_location_or_pc(&mut self) -> Result<MemoryLocation> {
        Ok(match self.take_argument()? {
            None => MemoryLocation::PC,
            Some(Argument::Integer(address)) => MemoryLocation::Address(check_bounds(address)?),
            Some(Argument::Label(label)) => MemoryLocation::Label(label),
            _ => return Err(Error::InvalidArgumentKind),
        })
    }

    fn take_register(&mut self) -> Option<Register> {
        self.reset_head();
        // Don't skip whitespace

        self.next().filter(|ch| *ch == 'r' || *ch == 'R')?;
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

        if !self.is_end_of_argument() {
            return None;
        }
        self.set_base();
        Some(register)
    }

    /// Extremely liberal in accepted syntax.
    ///
    /// Parses:
    ///  - Decimal (optional `#`), hex (`x`/`X`), octal (`o`/`O`), and binary (`b`/`B`)
    ///  - Optional single zero before non-decimal base prefix. Eg. `0x4`
    ///  - Leading zeros after prefix. Eg. `0x0004`, `#-03`
    ///  - Negative sign character before XOR after base prefix. Eg. `-#2`, `x-4`
    ///
    /// Returns `Ok(None)` for:
    ///  - Empty token
    ///  - Non-decimal base prefix, with zero before it, and invalid digits after it. Eg. `xLabel`, `o`
    ///
    /// Returns `Err` for:
    ///  - Violations of basic integer requirements, such as invalid digits
    ///  - Decimal base prefix `#` with zeros before it. Eg. `0#2`
    ///  - Decimal base prefix `#` with invalide or no digits after it. Eg. `#a`, `#`
    ///  - Multiple '-' characters (before or after prefix)
    ///  - Positive sign character '+'
    ///  - Negative sign character '-' if `allow_sign == false`
    ///  - Multiple zeros before base prefix. Eg. `00x4`
    ///  - Integers out of bounds of `i32`. (Does *NOT* check if integer fits in specific bit size)
    fn take_token_integer(&mut self, allow_sign: bool) -> Result<Option<i32>> {
        self.reset_head();
        // Don't skip whitespace

        // Take '-' BEFORE prefix
        let mut is_signed = false;
        if self.peek().is_some_and(|ch| ch == '-') {
            self.next();
            if !allow_sign {
                return Err(Error::InvalidInteger);
            }
            is_signed = true;
        }

        let Some((radix, has_leading_zeros, prefix_is_symbol)) = self.take_integer_prefix()? else {
            // '-' was given, so it must be an invalid token
            if is_signed {
                return Err(Error::InvalidInteger);
            }
            return Ok(None);
        };

        // Take '-' AFTER prefix
        if self.peek().is_some_and(|ch| ch == '-') {
            self.next();
            if !allow_sign {
                return Err(Error::InvalidInteger);
            }
            // Disallow '-x-...' and '--...'
            if is_signed {
                return Err(Error::InvalidInteger);
            }
            is_signed = true;
        }

        // TODO(refactor): This could be incorporated into the loop
        // Check next character is digit
        if !self
            .peek()
            .is_some_and(|ch| radix.parse_digit(ch).is_some())
        {
            // '-', '#', or pre-prefix zeros were given, so it must be an invalid integer token
            if is_signed || has_leading_zeros || prefix_is_symbol {
                return Err(Error::InvalidInteger);
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
                return Err(Error::InvalidInteger);
            };
            self.next();

            // Re-checked later on convert to smaller int types
            if integer > i32::MAX / radix as i32 {
                return Err(Error::IntegerTooLarge);
            }

            integer *= radix as i32;
            integer += digit as i32;
        }
        if is_signed {
            integer *= -1;
        }

        if !self.is_end_of_argument() {
            return Err(Error::InvalidInteger);
        }
        self.set_base();
        Ok(Some(integer))
    }

    /// Get radix from integer prefix
    /// Should only be called by `take_token_integer`
    /// Returns radix, whether leading zeros are included, and whether base prefix is a
    /// non-alphabetic symbol (i.e. `#`)
    fn take_integer_prefix(&mut self) -> Result<Option<(Radix, bool, bool)>> {
        // Don't reset head
        // Don't skip whitespace

        // Take single leading zero before prefix
        let has_leading_zeros = if self.peek().is_some_and(|ch| ch == '0') {
            self.next();
            true
        } else {
            false
        };

        // '0' or '0000...' (without base prefix)
        // Zeroes were taken as leading zeros
        if has_leading_zeros && self.is_end_of_argument() {
            self.reset_head();
            return Ok(Some((Radix::Decimal, true, false)));
        }

        let (radix, take_char, prefix_is_symbol) = match self.peek() {
            Some('#') => {
                // Disallow '0#...'
                if has_leading_zeros {
                    return Err(Error::InvalidInteger);
                }
                (Radix::Decimal, true, true)
            }
            // Allow 'b...' or 'x...'
            // Caller must check next characters are valid digits in the base, so as to not parse
            // non-integer tokens like 'xLabel' as integers (and fail)
            Some('b' | 'B') => (Radix::Binary, true, false),
            Some('o' | 'O') => (Radix::Octal, true, false),
            Some('x' | 'X') => (Radix::Hex, true, false),
            // No prefix. Don't skip character
            Some('0'..='9') => (Radix::Decimal, false, false),
            // Disallow '0-...'
            Some('-') => {
                if has_leading_zeros {
                    return Err(Error::InvalidInteger);
                }
                unreachable!("sign character should have already been taken by caller");
            }
            // Not recognized as an integer
            _ => return Ok(None),
        };

        if take_char {
            self.next(); // Skip prefix character
        }
        Ok(Some((radix, has_leading_zeros, prefix_is_symbol)))
    }

    fn take_token_label(&mut self) -> Result<Option<Label>> {
        self.reset_head();
        // Don't skip whitespace

        // Check first character can begin a label
        if !self
            .next()
            .is_some_and(|ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_'))
        {
            return Ok(None);
        };
        // Take characters until non-alphanumeric
        while let Some(ch) = self.peek() {
            if !matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_') {
                break;
            }
            self.next();
        }

        let name = self.get().to_string();

        // TODO(refactor): There is a cleaner way to do this
        let offset = match self.peek() {
            Some('+') => {
                self.next();
                self.base = self.head;
                let Some(offset) = self.take_token_integer(false)? else {
                    return Err(Error::InvalidLabel);
                };
                offset
            }
            Some('-') => {
                self.next();
                self.base = self.head;
                let Some(offset) = self.take_token_integer(false)? else {
                    return Err(Error::InvalidLabel);
                };
                -offset
            }
            _ => 0,
        };

        let offset: i16 = check_bounds(offset)?;

        if !self.is_end_of_argument() {
            return Err(Error::InvalidLabel);
        }
        self.set_base();
        Ok(Some(Label { name, offset }))
    }
}

fn check_bounds<T: TryFrom<i32>>(integer: i32) -> Result<T> {
    integer.try_into().map_err(|_| Error::IntegerTooLarge)
}

impl Radix {
    pub fn parse_digit(&self, ch: char) -> Option<u8> {
        Some(match self {
            Self::Binary => match ch {
                '0' => 0,
                '1' => 1,
                _ => return None,
            },
            Self::Octal => match ch {
                '0'..='7' => ch as u8 - '0' as u8,
                _ => return None,
            },
            Self::Decimal => match ch {
                '0'..='9' => ch as u8 - '0' as u8,
                _ => return None,
            },
            Self::Hex => match ch {
                '0'..='9' => ch as u8 - '0' as u8,
                'a'..='f' => ch as u8 - 'a' as u8 + 10,
                'A'..='F' => ch as u8 - 'A' as u8 + 10,
                _ => return None,
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn take_many_arguments_works() {
        let line = "  name  -54  r3 0x5812 Foo  Bar+0x04 4209";
        let mut iter = CommandIter::from(line);

        assert_eq!(iter.take_command_name(), Ok("name"));
        assert_eq!(iter.take_argument(), Ok(Some(Argument::Integer(-54))));
        assert_eq!(
            iter.take_argument(),
            Ok(Some(Argument::Register(Register::R3)))
        );
        assert_eq!(iter.take_argument(), Ok(Some(Argument::Integer(0x5812))));
        assert_eq!(
            iter.take_argument(),
            Ok(Some(Argument::Label(Label {
                name: "Foo".into(),
                offset: 0,
            })))
        );
        assert_eq!(
            iter.take_argument(),
            Ok(Some(Argument::Label(Label {
                name: "Bar".into(),
                offset: 0x04,
            })))
        );
        assert_eq!(iter.take_argument(), Ok(Some(Argument::Integer(4209))));
        assert_eq!(iter.take_argument(), Ok(None));
    }

    macro_rules! expect_tokens {
        ( $method:ident ($($args:tt)*), $input:expr, $($expected:tt)* ) => {{
            eprintln!("{}", $input);
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

    #[test]
    fn take_argument_works() {
        macro_rules! expect_argument { ( $($x:tt)* ) => {
            expect_tokens!(take_argument(), $($x)*);
        }}
        expect_argument!("", Ok(None));
    }

    #[test]
    fn take_register_works() {
        macro_rules! expect_register { ( $($x:tt)* ) => {
            expect_tokens!(take_register(), $($x)*);
        }}

        expect_register!("", None);
        expect_register!("a", None);
        expect_register!("rn", None);
        expect_register!("r8", None);
        expect_register!("r0n", None);
        expect_register!("r0n", None);
        expect_register!("r0", Some(Register::R0));
        expect_register!("r7", Some(Register::R7));
    }

    #[test]
    fn take_integer_works() {
        macro_rules! expect_integer { ( $allow_sign:expr, $($x:tt)* ) => {
            expect_tokens!(take_token_integer($allow_sign), $($x)*);
        }}

        // These tests cover all edge cases which I can think of
        // Invalid or non-integers
        expect_integer!(true, "", Ok(None)); // Non-integer
        expect_integer!(true, "a", Ok(None));
        expect_integer!(true, "z", Ok(None));
        expect_integer!(true, "+", Ok(None));
        expect_integer!(true, ",", Ok(None));
        expect_integer!(true, "b2", Ok(None));
        expect_integer!(true, "o8", Ok(None));
        expect_integer!(true, "xg", Ok(None));
        expect_integer!(true, "b", Ok(None));
        expect_integer!(true, "o", Ok(None));
        expect_integer!(true, "x", Ok(None));
        expect_integer!(true, "-", Err(_)); // Invalid integers
        expect_integer!(true, "#", Err(_));
        expect_integer!(true, "#-", Err(_));
        expect_integer!(true, "-#", Err(_));
        expect_integer!(true, "-#-", Err(_));
        expect_integer!(true, "-#-24", Err(_));
        expect_integer!(true, "0#0", Err(_));
        expect_integer!(true, "0#24", Err(_));
        expect_integer!(true, "-0#24", Err(_));
        expect_integer!(true, "0#-24", Err(_));
        expect_integer!(true, "-0#-24", Err(_));
        expect_integer!(true, "x-", Err(_));
        expect_integer!(true, "-x", Err(_));
        expect_integer!(true, "-x-", Err(_));
        expect_integer!(true, "-x-24", Err(_));
        expect_integer!(true, "0x", Err(_));
        expect_integer!(true, "0x-", Err(_));
        expect_integer!(true, "-0x", Err(_));
        expect_integer!(true, "-0x-", Err(_));
        expect_integer!(true, "-0x-24", Err(_));
        expect_integer!(true, "0-x24", Err(_));
        expect_integer!(true, "00x4", Err(_));
        expect_integer!(true, "##", Err(_)); // Invalid digit for decimal base
        expect_integer!(true, "-##", Err(_));
        expect_integer!(true, "#b", Err(_));
        expect_integer!(true, "#-b", Err(_));
        expect_integer!(true, "-#b", Err(_));
        expect_integer!(true, "0b2", Err(_)); // Invalid digit for base
        expect_integer!(true, "0o8", Err(_));
        expect_integer!(true, "0xg", Err(_));
        expect_integer!(true, "-b2", Err(_));
        expect_integer!(true, "-o8", Err(_));
        expect_integer!(true, "-xg", Err(_));
        expect_integer!(true, "b-2", Err(_));
        expect_integer!(true, "o-8", Err(_));
        expect_integer!(true, "x-g", Err(_));
        // Simple bounds check (it is not supposed to be super accurate)
        expect_integer!(true, "x80000000", Err(_));
        expect_integer!(true, "x7fffffff", Ok(Some(0x7fffffff)));
        expect_integer!(true, "x-7fffffff", Ok(Some(-0x7fffffff)));
        expect_integer!(true, "x-80000000", Err(_));
        // Decimal
        expect_integer!(true, "0", Ok(Some(0)));
        expect_integer!(true, "00", Ok(Some(0)));
        expect_integer!(true, "#0", Ok(Some(0)));
        expect_integer!(true, "#00", Ok(Some(0)));
        expect_integer!(true, "-#0", Ok(Some(0)));
        expect_integer!(true, "-#00", Ok(Some(0)));
        expect_integer!(true, "#-0", Ok(Some(0)));
        expect_integer!(true, "#-00", Ok(Some(0)));
        expect_integer!(true, "4", Ok(Some(4)));
        expect_integer!(true, "4284", Ok(Some(4284)));
        expect_integer!(true, "004284", Ok(Some(4284)));
        expect_integer!(true, "#4", Ok(Some(4)));
        expect_integer!(true, "#4284", Ok(Some(4284)));
        expect_integer!(true, "#004284", Ok(Some(4284)));
        expect_integer!(true, "-4", Ok(Some(-4)));
        expect_integer!(true, "-4284", Ok(Some(-4284)));
        expect_integer!(true, "-004284", Ok(Some(-4284)));
        expect_integer!(true, "-#4", Ok(Some(-4)));
        expect_integer!(true, "-#4284", Ok(Some(-4284)));
        expect_integer!(true, "-#004284", Ok(Some(-4284)));
        expect_integer!(true, "#-4", Ok(Some(-4)));
        expect_integer!(true, "#-4284", Ok(Some(-4284)));
        expect_integer!(true, "#-004284", Ok(Some(-4284)));
        expect_integer!(false, "-4", Err(_));
        expect_integer!(false, "-4284", Err(_));
        expect_integer!(false, "-004284", Err(_));
        expect_integer!(false, "-#4", Err(_));
        expect_integer!(false, "-#4284", Err(_));
        expect_integer!(false, "-#004284", Err(_));
        expect_integer!(false, "#-4", Err(_));
        expect_integer!(false, "#-4284", Err(_));
        expect_integer!(false, "#-004284", Err(_));
        // Hex
        expect_integer!(true, "x0", Ok(Some(0x0)));
        expect_integer!(true, "x00", Ok(Some(0x0)));
        expect_integer!(true, "0x0", Ok(Some(0x0)));
        expect_integer!(true, "0x00", Ok(Some(0x0)));
        expect_integer!(true, "-x0", Ok(Some(0x0)));
        expect_integer!(true, "-x00", Ok(Some(0x0)));
        expect_integer!(true, "0x-0", Ok(Some(0x0)));
        expect_integer!(true, "0x-00", Ok(Some(0x0)));
        expect_integer!(true, "-0x0", Ok(Some(0x0)));
        expect_integer!(true, "-0x00", Ok(Some(0x0)));
        expect_integer!(true, "x4", Ok(Some(0x4)));
        expect_integer!(true, "x004", Ok(Some(0x4)));
        expect_integer!(true, "x429", Ok(Some(0x429)));
        expect_integer!(true, "0x4", Ok(Some(0x4)));
        expect_integer!(true, "0x004", Ok(Some(0x4)));
        expect_integer!(true, "0x429", Ok(Some(0x429)));
        expect_integer!(true, "-x4", Ok(Some(-0x4)));
        expect_integer!(true, "-x004", Ok(Some(-0x4)));
        expect_integer!(true, "-x429", Ok(Some(-0x429)));
        expect_integer!(true, "-0x4", Ok(Some(-0x4)));
        expect_integer!(true, "-0x004", Ok(Some(-0x4)));
        expect_integer!(true, "-0x429", Ok(Some(-0x429)));
        expect_integer!(true, "x-4", Ok(Some(-0x4)));
        expect_integer!(true, "x-004", Ok(Some(-0x4)));
        expect_integer!(true, "x-429", Ok(Some(-0x429)));
        expect_integer!(true, "-0x4", Ok(Some(-0x4)));
        expect_integer!(true, "-0x004", Ok(Some(-0x4)));
        expect_integer!(true, "-0x429", Ok(Some(-0x429)));
        expect_integer!(false, "-x4", Err(_));
        expect_integer!(false, "-x004", Err(_));
        expect_integer!(false, "-x429", Err(_));
        expect_integer!(false, "-0x4", Err(_));
        expect_integer!(false, "-0x004", Err(_));
        expect_integer!(false, "-0x429", Err(_));
        expect_integer!(false, "x-4", Err(_));
        expect_integer!(false, "x-004", Err(_));
        expect_integer!(false, "x-429", Err(_));
        expect_integer!(false, "-0x4", Err(_));
        expect_integer!(false, "-0x004", Err(_));
        expect_integer!(false, "-0x429", Err(_));
        // Octal (0o427==0x117)
        expect_integer!(true, "o0", Ok(Some(0x0)));
        expect_integer!(true, "o00", Ok(Some(0x0)));
        expect_integer!(true, "0o0", Ok(Some(0x0)));
        expect_integer!(true, "0o00", Ok(Some(0x0)));
        expect_integer!(true, "-o0", Ok(Some(0x0)));
        expect_integer!(true, "-o00", Ok(Some(0x0)));
        expect_integer!(true, "o-0", Ok(Some(0x0)));
        expect_integer!(true, "o-00", Ok(Some(0x0)));
        expect_integer!(true, "-0o0", Ok(Some(0x0)));
        expect_integer!(true, "-0o00", Ok(Some(0x0)));
        expect_integer!(true, "0o-0", Ok(Some(0x0)));
        expect_integer!(true, "0o-00", Ok(Some(0x0)));
        expect_integer!(true, "o4", Ok(Some(0x4)));
        expect_integer!(true, "o004", Ok(Some(0x4)));
        expect_integer!(true, "o427", Ok(Some(0x117)));
        expect_integer!(true, "0o4", Ok(Some(0x4)));
        expect_integer!(true, "0o004", Ok(Some(0x4)));
        expect_integer!(true, "0o427", Ok(Some(0x117)));
        expect_integer!(true, "-o4", Ok(Some(-0x4)));
        expect_integer!(true, "-o004", Ok(Some(-0x4)));
        expect_integer!(true, "-o427", Ok(Some(-0x117)));
        expect_integer!(true, "-0o4", Ok(Some(-0x4)));
        expect_integer!(true, "-0o004", Ok(Some(-0x4)));
        expect_integer!(true, "-0o427", Ok(Some(-0x117)));
        expect_integer!(true, "o-4", Ok(Some(-0x4)));
        expect_integer!(true, "o-004", Ok(Some(-0x4)));
        expect_integer!(true, "o-427", Ok(Some(-0x117)));
        expect_integer!(true, "0o-4", Ok(Some(-0x4)));
        expect_integer!(true, "0o-004", Ok(Some(-0x4)));
        expect_integer!(true, "0o-427", Ok(Some(-0x117)));
        expect_integer!(false, "-o4", Err(_));
        expect_integer!(false, "-o004", Err(_));
        expect_integer!(false, "-o427", Err(_));
        expect_integer!(false, "-0o4", Err(_));
        expect_integer!(false, "-0o004", Err(_));
        expect_integer!(false, "-0o427", Err(_));
        expect_integer!(false, "o-4", Err(_));
        expect_integer!(false, "o-004", Err(_));
        expect_integer!(false, "o-427", Err(_));
        expect_integer!(false, "0o-4", Err(_));
        expect_integer!(false, "0o-004", Err(_));
        expect_integer!(false, "0o-427", Err(_));
        // Binary
        expect_integer!(true, "b0", Ok(Some(0b0)));
        expect_integer!(true, "b00", Ok(Some(0b0)));
        expect_integer!(true, "0b0", Ok(Some(0b0)));
        expect_integer!(true, "0b00", Ok(Some(0b0)));
        expect_integer!(true, "-b0", Ok(Some(0b0)));
        expect_integer!(true, "-b00", Ok(Some(0b0)));
        expect_integer!(true, "b-0", Ok(Some(0b0)));
        expect_integer!(true, "b-00", Ok(Some(0b0)));
        expect_integer!(true, "-0b0", Ok(Some(0b0)));
        expect_integer!(true, "-0b00", Ok(Some(0b0)));
        expect_integer!(true, "0b-0", Ok(Some(0b0)));
        expect_integer!(true, "0b-00", Ok(Some(0b0)));
        expect_integer!(true, "b1", Ok(Some(0b1)));
        expect_integer!(true, "b101", Ok(Some(0b101)));
        expect_integer!(true, "b00101", Ok(Some(0b101)));
        expect_integer!(true, "0b1", Ok(Some(0b1)));
        expect_integer!(true, "0b101", Ok(Some(0b101)));
        expect_integer!(true, "0b00101", Ok(Some(0b101)));
        expect_integer!(true, "-b1", Ok(Some(-0b1)));
        expect_integer!(true, "-b101", Ok(Some(-0b101)));
        expect_integer!(true, "-b00101", Ok(Some(-0b101)));
        expect_integer!(true, "b-1", Ok(Some(-0b1)));
        expect_integer!(true, "b-101", Ok(Some(-0b101)));
        expect_integer!(true, "b-00101", Ok(Some(-0b101)));
        expect_integer!(true, "-0b1", Ok(Some(-0b1)));
        expect_integer!(true, "-0b101", Ok(Some(-0b101)));
        expect_integer!(true, "-0b00101", Ok(Some(-0b101)));
        expect_integer!(true, "0b-1", Ok(Some(-0b1)));
        expect_integer!(true, "0b-101", Ok(Some(-0b101)));
        expect_integer!(true, "0b-00101", Ok(Some(-0b101)));
        expect_integer!(false, "-b1", Err(_));
        expect_integer!(false, "-b101", Err(_));
        expect_integer!(false, "-b00101", Err(_));
        expect_integer!(false, "b-1", Err(_));
        expect_integer!(false, "b-101", Err(_));
        expect_integer!(false, "b-00101", Err(_));
        expect_integer!(false, "-0b1", Err(_));
        expect_integer!(false, "-0b101", Err(_));
        expect_integer!(false, "-0b00101", Err(_));
        expect_integer!(false, "0b-1", Err(_));
        expect_integer!(false, "0b-101", Err(_));
        expect_integer!(false, "0b-00101", Err(_));
    }

    #[test]
    fn take_label_works() {
        macro_rules! expect_label { ( $($x:tt)* ) => {
            expect_tokens!(take_token_label(), $($x)*);
        }}
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
        expect_label!("Foo-4", Ok(Some(label!("Foo", -4))));
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
