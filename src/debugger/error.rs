use std::fmt;

use super::command::CommandName;

/// Error parsing a command.
// TODO(opt): Most `String` fields could be `&str` (with difficulty, no doubt)
#[derive(Debug, PartialEq)]
pub enum CommandError {
    InvalidCommand {
        name: String,
    },
    MissingSubcommand {
        name: &'static str,
    },
    InvalidSubcommand {
        name: &'static str,
        subname: String,
    },
    InvalidArgument {
        name: CommandName,
        error: ArgumentError,
    },
}

// TODO(rename): Type names and variants
#[derive(Debug, PartialEq)]
pub enum ArgumentError {
    MissingArgument {
        argument: &'static str,
        expected: u8,
    },
    /// For `eval`.
    MissingArgumentList {
        argument: &'static str,
    },
    TooManyArguments {
        expected: u8,
        actual: u8,
    },
    InvalidValue {
        argument: &'static str,
        error: ValueError,
    },
}

#[derive(Debug, PartialEq)]
pub enum ValueError {
    WrongArgumentType {},
    MalformedArgument {},
    MalformedInteger {},
    MalformedLabel {},
    IntegerTooLarge {},
}

impl std::error::Error for CommandError {}

impl fmt::Display for CommandError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCommand { name } => write!(f, "Not a command: `{}`.", name),
            Self::InvalidSubcommand { name, subname } => write!(
                f,
                "Invalid subcommand `{}` for command `{}`.",
                subname, name
            ),
            Self::MissingSubcommand { name } => {
                write!(f, "Missing subcommand for `{}`.", name)
            }

            Self::InvalidArgument { name, error } => {
                match error {
                    ArgumentError::MissingArgument { argument, expected } => {
                        write!(f, "Missing argument `{}` (expected {})", argument, expected)?;
                    }
                    ArgumentError::MissingArgumentList { argument } => {
                        write!(f, "Missing argument list `{}`", argument)?;
                    }
                    ArgumentError::TooManyArguments { expected, actual } => {
                        write!(
                            f,
                            "Too many arguments (expected {}, found {})",
                            expected, actual
                        )?;
                    }

                    ArgumentError::InvalidValue { argument, error } => {
                        match error {
                            ValueError::WrongArgumentType {} => {
                                write!(f, "Invalid type")?;
                            }
                            ValueError::MalformedArgument {} => {
                                write!(f, "Malformed argument")?;
                            }
                            ValueError::MalformedInteger {} => {
                                write!(f, "Malformed integer argument")?;
                            }
                            ValueError::MalformedLabel {} => {
                                write!(f, "Malformed label argument")?;
                            }
                            ValueError::IntegerTooLarge {} => {
                                write!(f, "Integer argument too large")?;
                            }
                        }

                        write!(f, " for argument `{}`", argument)?;
                    }
                }

                write!(f, " for command `{}`", name)
            }
        }
    }
}
