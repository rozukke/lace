use std::fmt;

use super::parse::CommandIter;
use crate::symbol::Register;

#[allow(dead_code)]
#[derive(Debug)]
pub enum Command {
    Help,
    Step {
        count: u16,
    },
    Next,
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
    // This can be `String` bc it will be allocated later regardless to get a &'static str
    // Unless parsing code is changed, and can accept a non-static string
    Eval {
        instruction: String,
    },
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
            Self::Registers => write!(f, "registers"),
            Self::Reset => write!(f, "reset"),
            Self::Source => write!(f, "source"),
            Self::Eval => write!(f, "eval"),
        }
    }
}

/// `Location::Memory(MemoryLocation::PC)` is valid, but should not be constructed
// TODO(refactor): This could be renamed to `Storage` or something similar
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
    pub name: String,
    pub offset: i16,
}

// TODO(refactor): Rename these variants
// TODO(opt): Most `String` fields could be `&str` (with difficulty, no doubt)
#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidCommandName { name: String },
    InvalidSubcommand { name: String, subname: String },
    MissingSubcommand { name: String },
    MissingArgument { name: CommandName },
    TooManyArguments { name: CommandName },
    WrongArgumentKind { name: CommandName },
    MalformedArgument,
    MalformedInteger,
    MalformedLabel,
    IntegerTooLarge,
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCommandName { name } => write!(f, "Not a command: `{}`.", name),
            Self::InvalidSubcommand { name, subname } => write!(
                f,
                "Invalid subcommand `{}` for command `{}`.",
                subname, name
            ),
            Self::MissingSubcommand { name } => write!(f, "Missing subcommand for `{}`.", name),
            Self::MissingArgument { name } => write!(f, "Missing argument for command `{}`.", name),
            Self::TooManyArguments { name } => {
                write!(f, "Too many arguments for command `{}`.", name)
            }
            Self::WrongArgumentKind { name } => {
                write!(f, "Invalid argument type for command `{}`.", name)
            }
            Self::MalformedArgument => write!(f, "Malformed argument."),
            Self::MalformedInteger => write!(f, "Malformed integer argument."),
            Self::MalformedLabel => write!(f, "Malformed label argument."),
            Self::IntegerTooLarge => write!(f, "Integer argument too large."),
        }
    }
}

impl TryFrom<&str> for Command {
    type Error = Error;

    fn try_from(line: &str) -> std::result::Result<Self, Self::Error> {
        let mut iter = CommandIter::from(line);

        // TODO(fix): Check bounds-checking for integer arguments?

        let name = iter.get_command_name()?;
        let command = match name {
            CommandName::Help => Self::Help,
            CommandName::Continue => Self::Continue,
            CommandName::Finish => Self::Finish,
            CommandName::Exit => Self::Exit,
            CommandName::Quit => Self::Quit,
            CommandName::Registers => Self::Registers,
            CommandName::Reset => Self::Reset,

            CommandName::Step => {
                let count = iter.next_positive_integer_or_default(name)?;
                Self::Step { count }
            }
            CommandName::Next => Self::Next,

            CommandName::Get => {
                let location = iter.next_location(name)?;
                Self::Get { location }
            }
            CommandName::Set => {
                let location = iter.next_location(name)?;
                let value = iter.next_integer(name)?;
                Self::Set { location, value }
            }

            CommandName::BreakList => Self::BreakList,
            CommandName::BreakAdd => {
                let location = iter.next_memory_location_or_default(name)?;
                Self::BreakAdd { location }
            }
            CommandName::BreakRemove => {
                let location = iter.next_memory_location_or_default(name)?;
                Self::BreakRemove { location }
            }

            CommandName::Source => {
                let count = iter.next_positive_integer_or_default(name)?;
                let location = iter.next_memory_location_or_default(name)?;
                Self::Source { count, location }
            }

            CommandName::Eval => {
                let instruction = iter.collect_rest();
                Self::Eval { instruction }
            }
        };

        // All commands except `eval`
        iter.expect_end_of_command(name)?;

        Ok(command)
    }
}
