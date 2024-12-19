use std::fmt;

use super::parse::CommandIter;
use crate::symbol::Register;

#[allow(dead_code)]
#[derive(Debug)]
pub enum Command {
    Help,
    Step { count: u16 },
    Next,
    Continue,
    Finish,
    Quit,
    Exit,
    BreakList,
    BreakAdd { location: MemoryLocation },
    BreakRemove { location: MemoryLocation },
    Get { location: Location },
    Set { location: Location, value: u16 },
    Jump { location: MemoryLocation },
    Registers,
    Reset,
    Source { location: MemoryLocation },
    // This can be `String` bc it will be allocated later regardless to get a &'static str
    // Unless parsing code is changed, and can accept a non-static string
    Eval { instruction: String },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum CommandName {
    Help,
    Step,
    Next,
    Continue,
    Finish,
    Quit,
    Exit,
    BreakList,
    BreakAdd,
    BreakRemove,
    Get,
    Set,
    Jump,
    Registers,
    Reset,
    Source,
    Eval,
}

impl fmt::Display for CommandName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Help => write!(f, "help"),
            Self::Step => write!(f, "step"),
            Self::Next => write!(f, "next"),
            Self::Continue => write!(f, "continue"),
            Self::Finish => write!(f, "finish"),
            Self::Quit => write!(f, "quit"),
            Self::Exit => write!(f, "exit"),
            Self::BreakList => write!(f, "break list"),
            Self::BreakAdd => write!(f, "break add"),
            Self::BreakRemove => write!(f, "break remove"),
            Self::Get => write!(f, "get"),
            Self::Set => write!(f, "set"),
            Self::Jump => write!(f, "jump"),
            Self::Registers => write!(f, "registers"),
            Self::Reset => write!(f, "reset"),
            Self::Source => write!(f, "source"),
            Self::Eval => write!(f, "eval"),
        }
    }
}

/// Register or memory location.
/// `Location::Memory(MemoryLocation::PC)` is valid, but should not be
/// constructed, as it would imply using `get`/`set` with the PC, which is not
/// possible.
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

/// Label with word offset.
#[derive(Debug, PartialEq)]
pub struct Label {
    pub name: String,
    pub offset: i16,
}

/// Error parsing a command.
// TODO(opt): Most `String` fields could be `&str` (with difficulty, no doubt)
#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidCommand {
        name: String,
    },
    MissingSubcommand {
        name: String,
    },
    InvalidSubcommand {
        name: String,
        subname: String,
    },
    InvalidArgument {
        name: CommandName,
        error: ArgumentError,
    },
}

#[derive(Debug, PartialEq)]
pub enum ArgumentError {
    MissingArgument { argument: &'static str },
    TooManyArguments {},
    WrongArgumentType { argument: &'static str },
    MalformedArgument {},
    MalformedInteger {},
    MalformedLabel {},
    IntegerTooLarge {},
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCommand { name } => write!(f, "Not a command: `{}`.", name),
            Self::InvalidSubcommand { name, subname } => write!(
                f,
                "Invalid subcommand `{}` for command `{}`.",
                subname, name
            ),
            Self::MissingSubcommand { name } => {
                write!(f, "Missing subcommand for `{}`.", name)
            }
            Self::InvalidArgument { name, error } => match error {
                ArgumentError::MissingArgument { argument } => {
                    write!(f, "Missing argument `{}` for command `{}`.", argument, name)
                }
                ArgumentError::TooManyArguments {} => {
                    write!(f, "Too many arguments for command `{}`.", name)
                }
                ArgumentError::WrongArgumentType { argument } => {
                    write!(
                        f,
                        "Invalid type for argument `{}` for command `{}`.",
                        argument, name
                    )
                }
                ArgumentError::MalformedArgument {} => {
                    write!(f, "Malformed argument for command `{}`.", name)
                }
                ArgumentError::MalformedInteger {} => {
                    write!(f, "Malformed integer argument for command `{}`.", name)
                }
                ArgumentError::MalformedLabel {} => {
                    write!(f, "Malformed label argument for command `{}`.", name)
                }
                ArgumentError::IntegerTooLarge {} => {
                    write!(f, "Integer argument too large for command `{}`.", name)
                }
            },
        }
    }
}

impl TryFrom<&str> for Command {
    type Error = Error;

    fn try_from(line: &str) -> std::result::Result<Self, Self::Error> {
        let mut iter = CommandIter::from(line);

        let name = iter.get_command_name()?;
        Command::parse_arguments(name, iter).map_err(|error| Error::InvalidArgument { name, error })
    }
}

impl Command {
    fn parse_arguments(
        name: CommandName,
        mut iter: CommandIter<'_>,
    ) -> Result<Command, ArgumentError> {
        let command = match name {
            CommandName::Help => Self::Help,
            CommandName::Continue => Self::Continue,
            CommandName::Finish => Self::Finish,
            CommandName::Exit => Self::Exit,
            CommandName::Quit => Self::Quit,
            CommandName::Registers => Self::Registers,
            CommandName::Reset => Self::Reset,

            CommandName::Step => {
                let count = iter.next_positive_integer_or_default("count")?;
                Self::Step { count }
            }
            CommandName::Next => Self::Next,

            CommandName::Get => {
                let location = iter.next_location("location")?;
                Self::Get { location }
            }
            CommandName::Set => {
                let location = iter.next_location("location")?;
                let value = iter.next_integer("value")?;
                Self::Set { location, value }
            }

            CommandName::Jump => {
                let location = iter.next_memory_location("location")?;
                Self::Jump { location }
            }

            CommandName::BreakList => Self::BreakList,
            CommandName::BreakAdd => {
                let location = iter.next_memory_location_or_default("location")?;
                Self::BreakAdd { location }
            }
            CommandName::BreakRemove => {
                let location = iter.next_memory_location_or_default("location")?;
                Self::BreakRemove { location }
            }

            CommandName::Source => {
                let location = iter.next_memory_location_or_default("location")?;
                Self::Source { location }
            }

            CommandName::Eval => {
                let instruction = iter.collect_rest();
                if instruction.is_empty() {
                    return Err(ArgumentError::MissingArgument {
                        argument: "instruction",
                    });
                }
                Self::Eval { instruction }
            }
        };

        // All commands except `eval`
        iter.expect_end_of_command()?;

        Ok(command)
    }
}
