use std::{error::Error, fmt};

use super::command::CommandName;

/// Error parsing a command.
#[derive(Debug, PartialEq)]
pub enum CommandError {
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
        error: ArgumentError,
    },
}

/// Error parsing command arguments.
#[derive(Debug, PartialEq)]
pub enum ArgumentError {
    /// For `eval`.
    MissingArgumentList { argument_name: &'static str },
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
        error: ValueError,
    },
}

/// Error parsing an argument value.
#[derive(Debug, PartialEq)]
pub enum ValueError {
    MismatchedType {
        expected_type: &'static str,
        actual_type: &'static str,
    },
    MalformedValue {},
    MalformedInteger {},
    MalformedLabel {},
    // TODO(feat): Add field for largest possible size
    IntegerTooLarge {},
}

impl Error for CommandError {}
impl Error for ArgumentError {}
impl Error for ValueError {}

impl fmt::Display for CommandError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCommand { command_name } => {
                write!(f, "Not a command: `{}`", command_name)
            }
            Self::InvalidSubcommand {
                command_name,
                subcommand_name,
            } => write!(
                f,
                "Invalid subcommand `{}` for command `{}`",
                subcommand_name, command_name
            ),
            Self::MissingSubcommand { command_name } => {
                write!(f, "Missing subcommand for `{}`", command_name)
            }
            Self::InvalidArgument {
                command_name,
                error,
            } => {
                write!(f, "In command `{}`: {}", command_name, error)
            }
        }
    }
}

impl fmt::Display for ArgumentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArgumentError::MissingArgumentList { argument_name } => {
                write!(f, "Missing argument list `{}`", argument_name)
            }
            ArgumentError::MissingArgument {
                argument_name,
                expected_count,
                actual_count,
            } => {
                write!(
                    f,
                    "Missing argument `{}` (expected {}, found {})",
                    argument_name, expected_count, actual_count
                )
            }
            ArgumentError::TooManyArguments {
                expected_count,
                actual_count,
            } => {
                write!(
                    f,
                    "Too many arguments (expected {}, found {})",
                    expected_count, actual_count
                )
            }
            ArgumentError::InvalidValue {
                argument_name,
                error,
            } => {
                write!(f, "For argument `{}`: {}", argument_name, error)
            }
        }
    }
}

impl fmt::Display for ValueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueError::MismatchedType {
                expected_type,
                actual_type,
            } => {
                write!(
                    f,
                    "Incorrect value type (expected {}, found {})",
                    expected_type, actual_type
                )
            }
            ValueError::MalformedValue {} => {
                write!(f, "Invalid value")
            }
            ValueError::MalformedInteger {} => {
                write!(f, "Malformed integer")
            }
            ValueError::MalformedLabel {} => {
                write!(f, "Malformed label")
            }
            ValueError::IntegerTooLarge {} => {
                write!(f, "Integer too large")
            }
        }
    }
}
