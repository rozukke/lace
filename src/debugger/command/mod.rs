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
    StepOver,
    StepInto { count: u16 },
    StepOut,
    Continue,
    Registers,
    Print { location: Location<'a> },
    Move { location: Location<'a>, value: u16 },
    Goto { location: MemoryLocation<'a> },
    Assembly { location: MemoryLocation<'a> },
    Eval { instruction: &'a str },
    Echo { string: &'a str },
    Reset,
    Quit,
    Exit,
    BreakList,
    BreakAdd { location: MemoryLocation<'a> },
    BreakRemove { location: MemoryLocation<'a> },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum CommandName {
    Help,
    StepOver,
    StepInto,
    StepOut,
    Continue,
    Registers,
    Print,
    Move,
    Goto,
    Assembly,
    Eval,
    Echo,
    Reset,
    Quit,
    Exit,
    BreakList,
    BreakAdd,
    BreakRemove,
}

impl fmt::Display for CommandName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Help => write!(f, "help"),
            Self::StepOver => write!(f, "step"),
            Self::StepInto => write!(f, "step into"),
            Self::StepOut => write!(f, "step out"),
            Self::Continue => write!(f, "continue"),
            Self::Registers => write!(f, "registers"),
            Self::Print => write!(f, "print"),
            Self::Move => write!(f, "move"),
            Self::Goto => write!(f, "goto"),
            Self::Assembly => write!(f, "assembly"),
            Self::Eval => write!(f, "eval"),
            Self::Echo => write!(f, "echo"),
            Self::Reset => write!(f, "reset"),
            Self::Quit => write!(f, "quit"),
            Self::Exit => write!(f, "exit"),
            Self::BreakList => write!(f, "break list"),
            Self::BreakAdd => write!(f, "break add"),
            Self::BreakRemove => write!(f, "break remove"),
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

            CommandName::StepOver => Self::StepOver,
            CommandName::Continue => Self::Continue,
            CommandName::StepOut => Self::StepOut,
            CommandName::Registers => Self::Registers,
            CommandName::Reset => Self::Reset,
            CommandName::Quit => Self::Quit,
            CommandName::Exit => Self::Exit,

            CommandName::StepInto => {
                expected_args = 1;
                let count = iter.next_positive_integer_or_default("count")?;
                Self::StepInto { count }
            }

            CommandName::Print => {
                expected_args = 1;
                let location = iter.next_location("location", expected_args)?;
                Self::Print { location }
            }
            CommandName::Move => {
                expected_args = 2;
                let location = iter.next_location("location", expected_args)?;
                let value = iter.next_integer("value", expected_args)?;
                Self::Move { location, value }
            }

            CommandName::Goto => {
                expected_args = 1;
                let location = iter.next_memory_location("location", expected_args)?;
                Self::Goto { location }
            }

            CommandName::Assembly => {
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
        expect_command("stepinto r0", Err(()));
        expect_command("print r0 r0", Err(()));
        expect_command("move r0", Err(()));
        expect_command("p x19248", Err(()));
        expect_command("p Q@)#", Err(()));

        expect_command("help", Ok(Command::Help));
        expect_command("  help   me!  ", Ok(Command::Help));
        expect_command("stepinto", Ok(Command::StepInto { count: 1 }));
        expect_command("si 0", Ok(Command::StepInto { count: 1 }));
        expect_command("stepinto   #012", Ok(Command::StepInto { count: 12 }));
        expect_command(
            "move   #012 0x123",
            Ok(Command::Move {
                location: Location::Memory(MemoryLocation::Address(12)),
                value: 0x123,
            }),
        );
        expect_command(
            "print r6",
            Ok(Command::Print {
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
