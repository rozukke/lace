use std::error::Error;
use std::fmt;

use super::parse::NaiveType;
use super::CommandName;

// `String` fields could possibly be converted to `&str`
// ...but the user must be punished for entering bad commands

/// Error parsing a command.
#[derive(Debug, PartialEq)]
pub enum Command {
    Invalid {
        command_name: String,
        suggested: Option<CommandName>,
    },
    MissingSubcommand {
        command_name: &'static str,
    },
    InvalidSubcommand {
        command_name: &'static str,
        subcommand_name: String,
        suggested: Option<CommandName>,
    },
    InvalidArgument {
        command_name: CommandName,
        error: Argument,
    },
}

/// Error parsing command arguments.
#[derive(Debug, PartialEq)]
pub enum Argument {
    Missing {
        argument_name: &'static str,
        expected_count: u8,
        actual_count: u8,
    },
    /// For `eval` and `echo`.
    MissingList { argument_name: &'static str },
    TooManyArguments {
        expected_count: u8,
        actual_count: u8,
    },
    InvalidValue {
        argument_name: &'static str,
        string: String,
        error: Value,
    },
}

/// Error parsing an argument value.
#[derive(Debug, PartialEq)]
pub enum Value {
    MismatchedType {
        expected_type: &'static str,
        actual_type: NaiveType,
    },
    Malformed {},
    MalformedInteger {},
    MalformedLabel {},
    MalformedRegister {},
    IntegerTooLarge {
        max: u16,
    },
}

impl Error for Command {}
impl Error for Argument {}
impl Error for Value {}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Invalid {
                command_name,
                suggested,
            } => {
                write!(f, "Not a command: `{}`.", command_name)?;
                if let Some(suggested) = suggested {
                    write!(f, "\n    Did you mean `{}`?", suggested)?;
                }
                if let Some(mnemonic) = is_mnemonic(command_name) {
                    write!(
                        f,
                        "\n    To simulate an instruction, run `eval {} (...)`.",
                        mnemonic
                    )?;
                }
            }
            Self::InvalidSubcommand {
                command_name,
                subcommand_name,
                suggested,
            } => {
                write!(
                    f,
                    "Invalid subcommand: `{} {}`.",
                    command_name, subcommand_name
                )?;
                if let Some(suggested) = suggested {
                    write!(f, "\n    Did you mean `{}`?", suggested)?;
                }
            }
            Self::MissingSubcommand { command_name } => {
                write!(f, "Missing subcommand: `{} (...)`.", command_name)?;
            }
            Self::InvalidArgument {
                command_name,
                error,
            } => {
                write!(f, "In command `{}`:", command_name)?;
                write!(f, "\n    ")?;
                write!(f, "{}", error)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Argument::MissingList { argument_name } => {
                write!(f, "Missing text argument `{}`.", argument_name)?;
            }
            Argument::Missing {
                argument_name,
                expected_count,
                actual_count,
            } => {
                write!(f, "Missing argument `{}`.", argument_name)?;
                write!(f, "\n        ")?;
                write!(
                    f,
                    "Expected {} argument{}, found {}.",
                    expected_count,
                    if *expected_count == 1 { "" } else { "s" },
                    actual_count,
                )?;
            }
            Argument::TooManyArguments {
                expected_count,
                actual_count,
            } => {
                write!(f, "Too many arguments.")?;
                write!(f, "\n        ")?;
                write!(
                    f,
                    "Expected {} argument{}, found {}.",
                    expected_count,
                    if *expected_count == 1 { "" } else { "s" },
                    actual_count,
                )?;
            }
            Argument::InvalidValue {
                argument_name,
                string: value,
                error,
            } => {
                write!(f, "For argument `{}`: `{}`.", argument_name, value)?;
                write!(f, "\n        ")?;
                write!(f, "{}", error)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::MismatchedType {
                expected_type,
                actual_type,
            } => {
                write!(f, "Incorrect value type.")?;
                write!(f, "\n        ")?;
                write!(f, "Expected {}.", expected_type)?;
                write!(f, "\n        ")?;
                write!(f, "Found {}.", actual_type)?;
            }
            Value::Malformed {} => {
                write!(f, "Invalid value.")?;
            }
            Value::MalformedInteger {} => {
                write!(f, "Malformed integer.")?;
            }
            Value::MalformedLabel {} => {
                write!(f, "Malformed label.")?;
            }
            Value::MalformedRegister {} => {
                write!(f, "Malformed register.")?;
            }
            Value::IntegerTooLarge { max } => {
                write!(f, "Integer too large.")?;
                write!(f, "\n        ")?;
                write!(f, "Maximum value: 0x{:04x}.", max)?;
            }
        }
        Ok(())
    }
}

impl Argument {
    /// Create [`Argument::InvalidValue`] from [`Value`].
    pub fn invalid_value<'a>(
        argument_name: &'static str,
        argument: &'a str,
    ) -> impl Fn(Value) -> Self + 'a {
        move |error| Self::InvalidValue {
            argument_name,
            string: argument.to_string(),
            error,
        }
    }
}

const MNEMONICS: &[&str] = &[
    "add", "and", "br", "brnzp", "brnz", "brzp", "brnp", "brn", "brz", "brp", "jmp", "jsr", "jsrr",
    "ld", "ldi", "ldr", "lea", "not", "ret", "rti", "st", "sti", "str", "pop", "push", "call",
    "rets", "trap", "getc", "out", "puts", "in", "putsp", "halt", "putn", "reg",
];

fn is_mnemonic(command_name: &str) -> Option<&'static str> {
    for mnemonic in MNEMONICS {
        if mnemonic.eq_ignore_ascii_case(command_name) {
            return Some(mnemonic);
        }
    }
    None
}
