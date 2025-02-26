pub mod error;
mod parse;
mod reader;

use std::fmt;

use self::parse::Arguments;
use self::reader::Read as _;
use crate::symbol::Register;

pub use self::reader::CommandReader;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Command<'a> {
    Help,
    Progress { count: u16 },
    Next,
    Continue,
    Finish,
    Quit,
    Exit,
    BreakList,
    BreakAdd { location: MemoryLocation<'a> },
    BreakRemove { location: MemoryLocation<'a> },
    Get { location: Location<'a> },
    Set { location: Location<'a>, value: u16 },
    Jump { location: MemoryLocation<'a> },
    Registers,
    Reset,
    Assembly { location: MemoryLocation<'a> },
    Eval { instruction: &'a str },
    Echo { string: &'a str },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum CommandName {
    Help,
    Next,
    Step,
    Finish,
    Continue,
    Registers,
    Get,
    Set,
    Jump,
    BreakList,
    BreakAdd,
    BreakRemove,
    Source,
    Eval,
    Echo,
    Reset,
    Quit,
    Exit,
}

impl fmt::Display for CommandName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Help => write!(f, "help"),
            Self::Next => write!(f, "step"),
            Self::Step => write!(f, "step-into"),
            Self::Finish => write!(f, "step-out"),
            Self::Continue => write!(f, "continue"),
            Self::Registers => write!(f, "registers"),
            Self::Get => write!(f, "print"),
            Self::Set => write!(f, "move"),
            Self::Jump => write!(f, "goto"),
            Self::BreakList => write!(f, "break list"),
            Self::BreakAdd => write!(f, "break add"),
            Self::BreakRemove => write!(f, "break remove"),
            Self::Source => write!(f, "assembly"),
            Self::Eval => write!(f, "eval"),
            Self::Echo => write!(f, "echo"),
            Self::Reset => write!(f, "reset"),
            Self::Quit => write!(f, "quit"),
            Self::Exit => write!(f, "exit"),
        }
    }
}

/// Register or memory location.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Location<'a> {
    Register(Register),
    Memory(MemoryLocation<'a>),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum MemoryLocation<'a> {
    PCOffset(i16),
    Address(u16),
    Label(Label<'a>),
}

/// Label with word offset.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Label<'a> {
    pub name: &'a str,
    pub offset: i16,
}

#[cfg(test)]
impl<'a> Label<'a> {
    pub fn new(name: &'a str, offset: i16) -> Self {
        Self { name, offset }
    }
}

impl<'a> Command<'a> {
    pub fn read_from<F>(source: &mut CommandReader, handle_error: F) -> Option<Self>
    where
        F: Fn(error::Command),
    {
        loop {
            let line = source.read()?.trim();

            // Necessary, since `Command::try_from` assumes non-empty line
            if line.is_empty() {
                continue;
            }

            // Remove silly lifetime restriction
            // SAFETY: Any reference which is returned from this function WILL be valid
            // SAFETY: The buffer which owns this string is not freed until all debugger business
            // has ended
            // SAFETY: The buffer also will not be overwritten until command has entirely
            // completed its execution. The fact that the buffer holds a line of multiple commands
            // does not change this fact
            let line = unsafe { &*(line as *const str) };

            match Command::try_from(line) {
                Ok(command) => return Some(command),
                Err(error) => {
                    handle_error(error);
                    continue;
                }
            }
        }
    }

    /// Assumes line is non-empty.
    //
    // Do not `impl TryFrom`. This method should be private
    fn try_from(line: &'a str) -> std::result::Result<Self, error::Command> {
        let mut iter = Arguments::from(line);

        let command_name = iter.get_command_name()?;
        Command::parse_arguments(command_name, &mut iter).map_err(|error| {
            error::Command::InvalidArgument {
                command_name,
                error,
            }
        })
    }

    fn parse_arguments(
        name: CommandName,
        iter: &mut Arguments<'a>,
    ) -> Result<Self, error::Argument> {
        let mut expected_args = 0;

        let command = match name {
            // Allow trailing arguments
            CommandName::Help => return Ok(Self::Help),

            CommandName::Next => Self::Next,
            CommandName::Continue => Self::Continue,
            CommandName::Finish => Self::Finish,
            CommandName::Registers => Self::Registers,
            CommandName::Reset => Self::Reset,
            CommandName::Quit => Self::Quit,
            CommandName::Exit => Self::Exit,

            CommandName::Step => {
                expected_args = 1;
                let count = iter.next_positive_integer_or_default("count")?;
                Self::Progress { count }
            }

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

            CommandName::Source => {
                expected_args = 1;
                let location = iter.next_memory_location_or_default("location")?;
                Self::Assembly { location }
            }

            CommandName::BreakList => Self::BreakList,
            CommandName::BreakAdd => {
                expected_args = 1;
                let location = iter.next_memory_location("location", expected_args)?;
                Self::BreakAdd { location }
            }
            CommandName::BreakRemove => {
                expected_args = 1;
                let location = iter.next_memory_location("location", expected_args)?;
                Self::BreakRemove { location }
            }

            CommandName::Eval => {
                let instruction = iter.get_rest();
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

            CommandName::Echo => {
                let string = iter.get_rest();
                if string.is_empty() {
                    return Err(error::Argument::MissingArgumentList {
                        argument_name: "instruction",
                    });
                }
                debug_assert!(
                    iter.expect_end(0, 0).is_ok(),
                    "no more arguments should exist",
                );
                return Ok(Self::Echo { string });
            }
        };

        iter.expect_end(expected_args, iter.arg_count() + 1)?;

        Ok(command)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_from() {
        fn expect_command(input: &str, expected: Result<Command, ()>) {
            println!("{:?}", input);
            let result = Command::try_from(input).map_err(|_| ());
            assert_eq!(result, expected);
        }

        expect_command("ts", Err(()));
        expect_command("break", Err(()));
        expect_command("break ts", Err(()));
        expect_command("progress r0", Err(()));
        expect_command("get r0 r0", Err(()));
        expect_command("set r0", Err(()));
        expect_command("p x19248", Err(()));
        expect_command("p Q@)#", Err(()));

        expect_command("help", Ok(Command::Help));
        expect_command("  help   me!  ", Ok(Command::Help));
        expect_command("progress", Ok(Command::Progress { count: 1 }));
        expect_command("p 0", Ok(Command::Progress { count: 1 }));
        expect_command("progress   #012", Ok(Command::Progress { count: 12 }));
        expect_command(
            "set   #012 0x123",
            Ok(Command::Set {
                location: Location::Memory(MemoryLocation::Address(12)),
                value: 0x123,
            }),
        );
        expect_command(
            "get r6",
            Ok(Command::Get {
                location: Location::Register(Register::R6),
            }),
        );
        expect_command("registers", Ok(Command::Registers));
        expect_command(
            "assembly  HW+4",
            Ok(Command::Assembly {
                location: MemoryLocation::Label(Label {
                    name: "HW",
                    offset: 4,
                }),
            }),
        );
        expect_command(
            "a",
            Ok(Command::Assembly {
                location: MemoryLocation::PCOffset(0),
            }),
        );
        expect_command(
            " eval  something  instruction  ",
            Ok(Command::Eval {
                instruction: "something  instruction",
            }),
        );
    }

    #[test]
    #[should_panic]
    fn empty_command_panics() {
        let _ = Command::try_from("");
    }
}
