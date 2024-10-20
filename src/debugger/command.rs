use std::fmt;

use super::parse::CommandIter;
use crate::symbol::Register;

#[allow(dead_code)]
#[derive(Debug)]
pub enum Command {
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
    Eval(EvalInstruction),
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

#[derive(Debug)]
pub enum EvalInstruction {}

// TODO(refactor): Rename these variants
#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidCommandName,
    InvalidSubcommand,
    MissingSubcommand,
    MissingArgument,
    InvalidArgumentKind,
    TooManyArguments,
    InvalidArgumentToken,
    InvalidInteger,
    InvalidLabel,
    IntegerTooLarge,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCommandName => write!(f, "Not a command."),
            Self::InvalidSubcommand => write!(f, "Invalid subcommand."),
            Self::MissingSubcommand => write!(f, "Missing subcommand."),
            Self::MissingArgument => write!(f, "Missing argument for command."),
            Self::InvalidArgumentKind => write!(f, "Invalid argument type for command."),
            Self::TooManyArguments => write!(f, "Too many arguments for command."),
            Self::InvalidArgumentToken => write!(f, "Malformed argument."),
            Self::InvalidInteger => write!(f, "Malformed integer argument."),
            Self::InvalidLabel => write!(f, "Malformed label argument."),
            Self::IntegerTooLarge => write!(f, "Integer argument too large for command."),
        }
    }
}

impl TryFrom<&str> for Command {
    type Error = Error;

    fn try_from(line: &str) -> std::result::Result<Self, Self::Error> {
        let mut iter = CommandIter::from(line);

        // TODO(fix): Check bounds for integer arguments
        // TODO(feat): Add more aliases (such as undocumented typo aliases)

        let name = match iter.next_command_name() {
            Some(name) => name,
            None => {
                // Command source should always return a string containing non-whitespace
                // characters, so initial command name should always exist.
                // Only panic in debug mode.
                #[cfg(debug_assertions)]
                {
                    panic!("assertion failed: missing command name.");
                }
                #[cfg(not(debug_assertions))]
                {
                    ""
                }
            }
        };

        let command = match name.to_lowercase().as_str() {
            "continue" | "c" => Self::Continue,
            "finish" | "f" => Self::Finish,
            "exit" | "e" => Self::Exit,
            "quit" | "q" => Self::Quit,
            "registers" | "r" => Self::Registers,
            "reset" => Self::Reset,

            "step" | "t" => {
                let count = iter.next_positive_integer_or_default()?;
                Self::Step { count }
            }
            "next" | "n" => Self::Next,
            "get" | "g" => {
                let location = iter.next_location()?;
                Self::Get { location }
            }
            "set" | "s" => {
                let location = iter.next_location()?;
                let value = iter.next_integer()?;
                Self::Set { location, value }
            }
            "source" => {
                let count = iter.next_positive_integer_or_default()?;
                let location = iter.next_memory_location_or_default()?;
                Self::Source { count, location }
            }

            "break" | "b" => {
                let Some(subname) = iter.next_command_name() else {
                    return Err(Error::MissingSubcommand);
                };
                match subname.to_lowercase().as_str() {
                    "list" | "l" => Self::BreakList,
                    "add" | "a" => {
                        let location = iter.next_memory_location_or_default()?;
                        Self::BreakAdd { location }
                    }
                    "remove" | "r" => {
                        let location = iter.next_memory_location_or_default()?;
                        Self::BreakRemove { location }
                    }
                    _ => return Err(Error::InvalidSubcommand),
                }
            }
            "breaklist" | "bl" => Self::BreakList,
            "breakadd" | "ba" => {
                let location = iter.next_memory_location_or_default()?;
                Self::BreakAdd { location }
            }
            "breakremove" | "br" => {
                let location = iter.next_memory_location_or_default()?;
                Self::BreakRemove { location }
            }

            "eval" => {
                eprintln!("unimplemented: eval command");
                return Err(Error::InvalidCommandName);
            }

            _ => return Err(Error::InvalidCommandName),
        };

        // All commands except `eval`
        iter.expect_end_of_command()?;

        Ok(command)
    }
}
