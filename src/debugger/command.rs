#[derive(Debug)]
pub enum Command {
    Step { count: usize },
    Stop,
    Quit,
}

// TODO(refactor): Rename these variants
#[derive(Debug)]
pub enum CommandError {
    MissingCommandName,
    InvalidCommandName,
    MissingArgument,
    InvalidArgumentKind,
    TooManyArguments,
    InvalidArgument,
    InvalidInteger,
}

enum CommandArgument {
    Register(u8),
    Integer(i16),
    Label { name: String, offset: u16 },
}

#[derive(Clone, Copy)]
enum Radix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hex = 16,
}

impl TryFrom<&str> for Command {
    type Error = CommandError;

    fn try_from(mut line: &str) -> Result<Self, Self::Error> {
        let name = Command::take_command_name(&mut line)?;
        println!("<{}>", name);
        println!("<{}>", line);

        let command = match name.to_lowercase().as_str() {
            "step" => {
                let count = match Command::try_take_argument(&mut line)? {
                    None => 1,
                    Some(CommandArgument::Integer(count)) => count.max(1) as usize,
                    _ => return Err(CommandError::InvalidArgumentKind),
                };
                Self::Step { count }
            }

            _ => return Err(CommandError::InvalidCommandName),
        };

        Ok(command)
    }
}

impl Command {
    fn take_whitespace<'a>(line: &mut &'a str) {
        let mut start = 0;
        for ch in line.chars() {
            if !ch.is_whitespace() {
                break;
            }
            start += 1;
        }
        *line = line.split_at(start).1;
    }

    fn take_command_name<'a>(line: &mut &'a str) -> Result<&'a str, CommandError> {
        Self::take_whitespace(line);

        let mut end = 0;
        for ch in line.chars() {
            if !ch.is_alphanumeric() {
                break;
            }
            end += 1;
        }

        if end == 0 {
            return Err(CommandError::MissingCommandName);
        }

        let (name, rest) = line.split_at(end);
        *line = rest;
        Ok(name)
    }

    fn try_take_argument(line: &mut &str) -> Result<Option<CommandArgument>, CommandError> {
        Self::take_whitespace(line);
        if let Some(integer) = Self::try_take_integer(line)? {
            return Ok(Some(CommandArgument::Integer(integer)));
        }
        todo!();
    }

    fn try_take_integer(line: &mut &str) -> Result<Option<i16>, CommandError> {
        for radix in Radix::ALL {
            if let Some(integer) = Self::try_take_integer_radix(line, *radix)? {
                return Ok(Some(integer));
            }
        }
        Ok(None)
    }

    fn try_take_integer_radix(line: &mut &str, radix: Radix) -> Result<Option<i16>, CommandError> {
        let mut chars = line.chars().peekable();

        // Take `-` before prefix
        let mut is_signed = false;
        if chars.peek().is_some_and(|ch| *ch == '-') {
            chars.next();
            is_signed = true;
        }

        // Take leading zeros (before prefix)
        if chars.peek().is_some_and(|ch| *ch == '0') {
            chars.next();
        }

        if let Some(prefix) = radix.prefix() {
            // Take prefix
            if !chars
                .next()
                .is_some_and(|ch| ch.to_ascii_lowercase() == prefix)
            {
                return Ok(None);
            }
            // Take `-` after prefix
            if chars.peek().is_some_and(|ch| *ch == '-') {
                chars.next();
                // Disallow `-x-`
                if is_signed {
                    return Err(CommandError::InvalidInteger);
                }
                is_signed = true;
            }
        }

        // Check next character is digit
        if !chars
            .peek()
            .is_some_and(|ch| radix.parse_digit(*ch).is_some())
        {
            return Ok(None);
        };

        let mut integer: i32 = 0;
        while let Some(ch) = chars.next() {
            if ch.is_ascii_whitespace() {
                break;
            };

            let Some(digit) = radix.parse_digit(ch) else {
                return Err(CommandError::InvalidInteger);
            };
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
        Ok(Some(integer as i16))
    }
}

impl Radix {
    /// Ordered by parsing priority
    const ALL: &'static [Self] = &[Self::Hex, Self::Decimal, Self::Binary, Self::Octal];

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
