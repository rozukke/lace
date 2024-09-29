use crate::symbol::Register;

#[derive(Debug)]
pub enum Command {
    Step {
        count: usize,
    },
    Next {
        count: usize,
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
        count: usize,
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
pub enum CommandError {
    MissingCommandName,
    InvalidCommandName,
    MissingArgument,
    InvalidArgumentKind,
    TooManyArguments,
    InvalidArgument,
    InvalidInteger,
    InvalidLabel,
}

#[derive(Debug, PartialEq)]
enum Argument {
    Register(Register),
    Integer(i16),
    Label(Label),
}

#[derive(Clone, Copy, Debug)]
enum Radix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hex = 16,
}

type Result<T> = std::result::Result<T, CommandError>;

impl TryFrom<&str> for Command {
    type Error = CommandError;

    fn try_from(line: &str) -> std::result::Result<Self, Self::Error> {
        let mut iter = CommandIter::from(line);

        let name = iter.take_command_name()?;
        // println!("<{}>", name);

        // TODO(fix): Check bounds for integer arguments
        // TODO(feat): Add more aliases (such as undocumented typo aliases)
        let command = match name.to_lowercase().as_str() {
            "continue" | "c" => Self::Continue,
            "finish" | "f" => Self::Finish,
            "exit" | "e" => Self::Exit,
            "quit" | "q" => Self::Quit,
            "registers" | "r" => Self::Registers,
            "reset" => Self::Reset,

            "step" | "t" => {
                let count = iter.take_integer_or_default(1)? as usize;
                Self::Step { count }
            }
            "next" | "n" => {
                let count = iter.take_integer_or_default(1)? as usize;
                Self::Step { count }
            }
            "get" | "g" => {
                let location = iter.take_location()?;
                Self::Get { location }
            }
            "set" | "s" => {
                let location = iter.take_location()?;
                let value = iter.take_integer()? as u16;
                Self::Set { location, value }
            }
            "source" => {
                let count = iter.take_integer_or_default(1)? as usize;
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
                        Self::BreakAdd { location }
                    }
                    _ => return Err(CommandError::InvalidCommandName),
                }
            }
            "breaklist" | "bl" => Self::BreakList,
            "breakadd" | "ba" => {
                let location = iter.take_memory_location_or_pc()?;
                Self::BreakAdd { location }
            }
            "breakremove" | "br" => {
                let location = iter.take_memory_location_or_pc()?;
                Self::BreakAdd { location }
            }

            "eval" => {
                eprintln!("unimplemented: eval command");
                return Err(CommandError::InvalidCommandName);
            }

            _ => return Err(CommandError::InvalidCommandName),
        };

        // All commands except `eval`
        if iter.take_argument()?.is_some() {
            return Err(CommandError::TooManyArguments);
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
        matches!(self.peek(), None | Some(' ' | ';'))
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

        println!("{} / {}", self.base, self.head);
        if self.get().is_empty() {
            return Err(CommandError::MissingCommandName);
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
        Err(CommandError::InvalidArgument)
    }

    pub fn take_integer(&mut self) -> Result<i16> {
        Ok(match self.take_argument()? {
            None => return Err(CommandError::MissingArgument),
            Some(Argument::Integer(count)) => count.max(1),
            _ => return Err(CommandError::InvalidArgumentKind),
        })
    }

    pub fn take_integer_or_default(&mut self, default: i16) -> Result<i16> {
        Ok(match self.take_argument()? {
            None => default,
            Some(Argument::Integer(count)) => count.max(1),
            _ => return Err(CommandError::InvalidArgumentKind),
        })
    }

    pub fn take_location(&mut self) -> Result<Location> {
        Ok(match self.take_argument()? {
            None => return Err(CommandError::MissingArgument),
            Some(Argument::Register(register)) => Location::Register(register),
            Some(Argument::Integer(address)) => {
                Location::Memory(MemoryLocation::Address(address as u16))
            }
            Some(Argument::Label(label)) => Location::Memory(MemoryLocation::Label(label)),
        })
    }

    pub fn take_memory_location_or_pc(&mut self) -> Result<MemoryLocation> {
        Ok(match self.take_argument()? {
            None => MemoryLocation::PC,
            Some(Argument::Integer(address)) => MemoryLocation::Address(address as u16),
            Some(Argument::Label(label)) => MemoryLocation::Label(label),
            _ => return Err(CommandError::InvalidArgumentKind),
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

    fn take_token_integer(&mut self, allow_sign: bool) -> Result<Option<i16>> {
        // Don't reset head
        // Don't skip whitespace
        for radix in Radix::ALL {
            if let Some(integer) = self.take_token_integer_with_radix(allow_sign, *radix)? {
                return Ok(Some(integer));
            }
        }
        Ok(None)
    }

    fn take_token_integer_with_radix(
        &mut self,
        allow_sign: bool,
        radix: Radix,
    ) -> Result<Option<i16>> {
        self.reset_head();
        // Don't skip whitespace

        // Take `-` before prefix
        let mut is_signed = false;
        if self.peek().is_some_and(|ch| ch == '-') {
            self.next();
            if !allow_sign {
                return Err(CommandError::InvalidInteger);
            }
            is_signed = true;
        }

        // Take leading zeros (before prefix)
        if self.peek().is_some_and(|ch| ch == '0') {
            self.next();
        }

        if let Some(prefix) = radix.prefix() {
            // Take prefix
            if !self
                .next()
                .is_some_and(|ch| ch.to_ascii_lowercase() == prefix)
            {
                return Ok(None);
            }
            // Take `-` after prefix
            if self.peek().is_some_and(|ch| ch == '-') {
                self.next();
                if !allow_sign {
                    return Err(CommandError::InvalidInteger);
                }
                // Disallow `-x-`
                if is_signed {
                    return Err(CommandError::InvalidInteger);
                }
                is_signed = true;
            }
        }

        // Check next character is digit
        if !self
            .peek()
            .is_some_and(|ch| radix.parse_digit(ch).is_some())
        {
            return Ok(None);
        };

        let mut integer: i32 = 0;
        while let Some(ch) = self.peek() {
            if self.is_end_of_argument() {
                break;
            }
            let Some(digit) = radix.parse_digit(ch) else {
                return Err(CommandError::InvalidInteger);
            };
            self.next();
            integer *= radix as i32;
            integer += digit as i32;

            // Not a very robust check
            if integer > u16::MAX as i32 {
                return Err(CommandError::InvalidInteger);
            }
        }
        if is_signed {
            integer *= -1;
        }

        if !self.is_end_of_argument() {
            return Err(CommandError::InvalidInteger);
        }
        self.set_base();
        Ok(Some(integer as i16))
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
                    return Err(CommandError::InvalidLabel);
                };
                offset
            }
            Some('-') => {
                self.next();
                self.base = self.head;
                let Some(offset) = self.take_token_integer(false)? else {
                    return Err(CommandError::InvalidLabel);
                };
                -offset
            }
            _ => 0,
        };

        if !self.is_end_of_argument() {
            return Err(CommandError::InvalidLabel);
        }
        self.set_base();
        Ok(Some(Label { name, offset }))
    }
}

impl Radix {
    /// Ordered by parsing priority
    pub const ALL: &'static [Self] = &[Self::Hex, Self::Decimal, Self::Binary, Self::Octal];

    pub fn prefix(&self) -> Option<char> {
        Some(match self {
            Self::Decimal => return None,
            Self::Binary => 'b',
            Self::Octal => 'o',
            Self::Hex => 'x',
        })
    }

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
                'a'..='z' => ch as u8 - 'a' as u8 + 10,
                'A'..='Z' => ch as u8 - 'A' as u8 + 10,
                _ => return None,
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn take_arguments_works() {
        let line = "  name  -54  r3 0x5812 Foo  Bar+0x04; 4209";
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
        assert_eq!(iter.take_argument(), Ok(None));
    }
}
