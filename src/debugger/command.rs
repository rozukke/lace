use std::fmt;

use super::{error, parse::ArgIter};
use crate::symbol::Register;

#[derive(Debug)]
pub enum Command<'a> {
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
    Eval { instruction: &'a str },
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
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Location {
    Register(Register),
    Memory(MemoryLocation),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum MemoryLocation {
    PCOffset(i16),
    Address(u16),
    Label(Label),
}

/// Label with word offset.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Label {
    pub name: &'static str,
    pub address: u16,
    pub offset: i16,
}

impl<'a> TryFrom<&'a str> for Command<'a> {
    type Error = error::Command;

    /// Assumes line is non-empty.
    fn try_from(line: &'a str) -> std::result::Result<Self, Self::Error> {
        let mut iter = ArgIter::from(line);

        let command_name = iter.get_command_name()?;
        Command::parse_arguments(command_name, &mut iter).map_err(|error| {
            error::Command::InvalidArgument {
                command_name,
                error,
            }
        })
    }
}

impl<'a> Command<'a> {
    fn parse_arguments(name: CommandName, iter: &mut ArgIter<'a>) -> Result<Self, error::Argument> {
        let mut expected_args = 0;

        let command = match name {
            // Allow trailing arguments
            CommandName::Help => return Ok(Self::Help),

            CommandName::Continue => Self::Continue,
            CommandName::Finish => Self::Finish,
            CommandName::Exit => Self::Exit,
            CommandName::Quit => Self::Quit,
            CommandName::Registers => Self::Registers,
            CommandName::Reset => Self::Reset,

            CommandName::Step => {
                expected_args = 1;
                let count = iter.next_positive_integer_or_default("count")?;
                Self::Step { count }
            }
            CommandName::Next => Self::Next,

            CommandName::Get => {
                expected_args = 1;
                let location = iter.next_location("location", expected_args)?;
                Self::Get { location }
            }
            CommandName::Set => {
                expected_args = 2;
                let location = iter.next_location("location", expected_args)?;
                let value = iter.next_integer("value", expected_args)?;
                Self::Set { location, value }
            }

            CommandName::Jump => {
                expected_args = 1;
                let location = iter.next_memory_location("location", expected_args)?;
                Self::Jump { location }
            }

            CommandName::BreakList => Self::BreakList,
            CommandName::BreakAdd => {
                expected_args = 1;
                let location = iter.next_memory_location_or_default("location")?;
                Self::BreakAdd { location }
            }
            CommandName::BreakRemove => {
                expected_args = 1;
                let location = iter.next_memory_location_or_default("location")?;
                Self::BreakRemove { location }
            }

            CommandName::Source => {
                expected_args = 1;
                let location = iter.next_memory_location_or_default("location")?;
                Self::Source { location }
            }

            CommandName::Eval => {
                let instruction = iter.collect_rest();
                if instruction.is_empty() {
                    return Err(error::Argument::MissingArgumentList {
                        argument_name: "instruction",
                    });
                }
                // Don't return `Err` for invalid argument count, as this shouldn't happen
                debug_assert!(
                    iter.expect_end(0, 0).is_ok(),
                    "no more arguments should exist",
                );
                return Ok(Self::Eval { instruction });
            }
        };

        iter.expect_end(expected_args, iter.arg_count() + 1)?;

        Ok(command)
    }
}
