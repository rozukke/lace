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

impl TryFrom<&str> for Command {
    type Error = Error;

    fn try_from(line: &str) -> std::result::Result<Self, Self::Error> {
        let mut iter = CommandIter::from(line);

        // TODO(fix): Check bounds for integer arguments
        // TODO(feat): Add more aliases (such as undocumented typo aliases)
        let name = iter.next_command_name()?;
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
                let subname = iter.next_command_name()?;
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
                    _ => return Err(Error::InvalidCommandName),
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
