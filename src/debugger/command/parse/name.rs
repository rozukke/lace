use super::{error, Arguments, CommandName};

// TODO(feat): Add more aliases (such as undocumented typo aliases)
#[rustfmt::skip]
const COMMANDS: CommandNameList = &[
    (CommandName::Help,        &["help", "--help", "h", "-h"]),
    (CommandName::Continue,    &["continue", "cont", "c"]), // or 'proceed'
    (CommandName::Finish,      &["finish", "fin", "f"]),
    (CommandName::Exit,        &["exit"]),
    (CommandName::Quit,        &["quit", "q"]),
    (CommandName::Registers,   &["registers", "reg", "r"]),
    (CommandName::Reset,       &["reset"]),
    (CommandName::Step,        &["progress", "p"]), // or 'advance'
    (CommandName::Next,        &["next", "n"]),
    (CommandName::Get,         &["get", "g"]),
    (CommandName::Set,         &["set", "s"]),
    (CommandName::Jump,        &["jump", "j"]),
    (CommandName::Source,      &["assembly", "asm", "a"]), // or 'source'
    (CommandName::Eval,        &["eval", "e"]),
    (CommandName::BreakList,   &["breaklist", "bl"]),
    (CommandName::BreakAdd,    &["breakadd", "ba"]),
    (CommandName::BreakRemove, &["breakremove", "br"]),
    // "break" is treated specially
];
const BREAK_COMMAND: CandidateList = &["break", "b"];
#[rustfmt::skip]
const BREAK_SUBCOMMANDS: CommandNameList = &[
    (CommandName::BreakList,   &["list", "l"]),
    (CommandName::BreakAdd,    &["add", "a"]),
    (CommandName::BreakRemove, &["remove", "r"]),
];

impl Arguments<'_> {
    /// Parse next [`CommandName`].
    ///
    /// Considers multi-word command names (i.e. subcommands) as one name. Eg. "break add".
    ///
    /// Assumes line is non-empty.
    pub fn get_command_name(&mut self) -> Result<CommandName, error::Command> {
        // TODO(fix): assert no other arguments were parsed/consumed yet

        let command_name = self.next_str_name();
        // Command source should always return a string containing non-whitespace
        // characters, so initial command name should always exist.
        debug_assert!(command_name.is_some(), "missing command name");
        let command_name = command_name.unwrap_or("");

        if let Some(command) = find_name_match(command_name, COMMANDS) {
            return Ok(command);
        };

        // "break" is currently the only command with subcommands, so it is treated specially
        // This could be written a bit nicer. But it doesn't seem necessary.
        if name_matches(command_name, BREAK_COMMAND) {
            // Normalize name and get as `'static`
            // Only used for errors
            let command_name = BREAK_COMMAND[0]; // Array must be non-empty if this branch is being ran

            let Some(subcommand_name) = self.next_str_name() else {
                return Err(error::Command::MissingSubcommand { command_name });
            };
            let Some(command) = find_name_match(subcommand_name, BREAK_SUBCOMMANDS) else {
                return Err(error::Command::InvalidSubcommand {
                    command_name,
                    subcommand_name: subcommand_name.to_string(),
                });
            };
            return Ok(command);
        }

        Err(error::Command::InvalidCommand {
            command_name: command_name.to_string(),
        })
    }
}

/// A [`CommandName`] with a list of name candidates.
type CommandNameList<'a> = &'a [(CommandName, CandidateList<'a>)];
/// List of single-word aliases for a command or subcommand.
type CandidateList<'a> = &'a [&'a str];

/// Returns the first [`CommandName`], which has a corresponding candidate which matches `name`
/// (case insensitive).
///
/// Returns `None` if no match was found.
fn find_name_match(name: &str, commands: CommandNameList) -> Option<CommandName> {
    for (command, candidates) in commands {
        if name_matches(name, candidates) {
            return Some(*command);
        }
    }
    None
}

/// Returns `true` if `name` matchs any item of `candidates` (case insensitive).
fn name_matches(name: &str, candidates: CandidateList) -> bool {
    for candidate in candidates {
        if name.eq_ignore_ascii_case(candidate) {
            return true;
        }
    }
    false
}
