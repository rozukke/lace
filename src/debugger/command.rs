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

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum CommandName {
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

#[derive(Debug)]
pub enum EvalInstruction {}

// TODO(refactor): Rename these variants
// TODO(opt): Most `String` fields could be `&str`
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

fn next_command_name(iter: &mut CommandIter) -> Result<CommandName, Error> {
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

    let name = match name.to_lowercase().as_str() {
        "continue" | "c" => CommandName::Continue,
        "finish" | "f" => CommandName::Finish,
        "exit" | "e" => CommandName::Exit,
        "quit" | "q" => CommandName::Quit,
        "registers" | "r" => CommandName::Registers,
        "reset" => CommandName::Reset,
        "step" | "t" => CommandName::Step,
        "next" | "n" => CommandName::Next,
        "get" | "g" => CommandName::Get,
        "set" | "s" => CommandName::Set,
        "source" => CommandName::Source,
        "breaklist" | "bl" => CommandName::BreakList,
        "breakadd" | "ba" => CommandName::BreakAdd,
        "breakremove" | "br" => CommandName::BreakRemove,
        "break" | "b" => {
            let name = name.to_string();
            let Some(subname) = iter.next_command_name() else {
                return Err(Error::MissingSubcommand { name });
            };
            match subname.to_lowercase().as_str() {
                "list" | "l" => CommandName::BreakList,
                "add" | "a" => CommandName::BreakAdd,
                "remove" | "r" => CommandName::BreakRemove,
                _ => {
                    return Err(Error::InvalidSubcommand {
                        name,
                        subname: subname.to_string(),
                    });
                }
            }
        }
        "eval" => {
            eprintln!("unimplemented: eval command");
            return Err(Error::InvalidCommandName {
                name: "eval".to_string(),
            });
        }
        _ => {
            return Err(Error::InvalidCommandName {
                name: name.to_string(),
            })
        }
    };

    Ok(name)
}

impl TryFrom<&str> for Command {
    type Error = Error;

    fn try_from(line: &str) -> std::result::Result<Self, Self::Error> {
        let mut iter = CommandIter::from(line);

        // TODO(fix): Check bounds for integer arguments
        // TODO(feat): Add more aliases (such as undocumented typo aliases)

        let name = next_command_name(&mut iter)?;

        let command = match name {
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
                eprintln!("unimplemented: eval command");
                return Err(Error::InvalidCommandName {
                    name: "eval".to_string(),
                });
            }
        };

        // All commands except `eval`
        iter.expect_end_of_command(name)?;

        Ok(command)
    }
}
