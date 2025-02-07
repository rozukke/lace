use std::{error::Error, fmt};

use super::command::CommandName;

/// Error parsing a command.
#[derive(Debug, PartialEq)]
pub enum Command {
    #[allow(clippy::enum_variant_names)]
    InvalidCommand {
        command_name: String,
    },
    MissingSubcommand {
        command_name: &'static str,
    },
    InvalidSubcommand {
        command_name: &'static str,
        subcommand_name: String,
    },
    InvalidArgument {
        command_name: CommandName,
        error: Argument,
    },
}

/// Error parsing command arguments.
#[derive(Debug, PartialEq)]
pub enum Argument {
    /// For `eval`.
    MissingArgumentList { argument_name: &'static str },
    #[allow(clippy::enum_variant_names)]
    MissingArgument {
        argument_name: &'static str,
        expected_count: u8,
        actual_count: u8,
    },
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
        actual_type: &'static str,
    },
    #[allow(clippy::enum_variant_names)]
    MalformedValue {},
    // TODO(feat): Add subvariants
    MalformedInteger {},
    MalformedLabel {},
    MalformedRegister {},
    IntegerTooLarge {
        max: u16,
    },
    LabelNotFound {
        similar: Option<&'static String>,
    },
}

impl Error for Command {}
impl Error for Argument {}
impl Error for Value {}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCommand { command_name } => {
                write!(f, "Not a command: `{}`", command_name)?;
            }
            Self::InvalidSubcommand {
                command_name,
                subcommand_name,
            } => {
                write!(
                    f,
                    "Invalid subcommand: `{} {}`",
                    command_name, subcommand_name
                )?;
            }
            Self::MissingSubcommand { command_name } => {
                write!(f, "Missing subcommand: `{} ?`", command_name)?;
            }
            Self::InvalidArgument {
                command_name,
                error,
            } => {
                writeln!(f, "In command `{}`:", command_name)?;
                write!(f, "    ")?;
                write!(f, "{}", error)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Argument::MissingArgumentList { argument_name } => {
                write!(f, "Missing text argument `{}`.", argument_name)?;
            }
            Argument::MissingArgument {
                argument_name,
                expected_count,
                actual_count,
            } => {
                writeln!(f, "Missing argument `{}`.", argument_name)?;
                write!(f, "        ")?;
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
                writeln!(f, "Too many arguments.")?;
                write!(f, "        ")?;
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
                writeln!(f, "For argument `{}`: `{}`.", argument_name, value)?;
                write!(f, "        ")?;
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
                writeln!(f, "Incorrect value type")?;
                write!(f, "        ")?;
                write!(f, "Expected {}, found {}.", expected_type, actual_type)?;
            }
            Value::MalformedValue {} => {
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
                writeln!(f, "Integer too large.")?;
                write!(f, "        ")?;
                write!(f, "Maximum value: 0x{:04x}.", max)?;
            }
            Value::LabelNotFound { similar } => {
                write!(f, "Label not found")?;
                if let Some(similar) = similar {
                    write!(f, ". Did you mean `{}`?", similar)?;
                }
            }
        }
        Ok(())
    }
}
