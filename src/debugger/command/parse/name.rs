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
    (CommandName::Echo,        &["echo"]),
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
        assert!(
            self.cursor == 0,
            "tried to parse command name from middle of buffer",
        );

        let command_name = self.next_token_str();
        // Command source should always return a string containing non-whitespace characters
        let command_name = command_name.expect("missing command name");

        if let Some(command) = find_name_match(command_name, COMMANDS) {
            return Ok(command);
        };

        // "break" is currently the only command with subcommands, so it is treated specially
        // This could be written a bit nicer. But it doesn't seem necessary.
        if name_matches(command_name, BREAK_COMMAND) {
            // Normalize name and get as `'static`
            // Only used for errors
            let command_name = BREAK_COMMAND[0]; // Array must be non-empty if this branch is being ran

            let Some(subcommand_name) = self.next_token_str() else {
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

        // User clearly wants return to bash
        if command_name == "sudo" {
            println!("Goodbye");
            std::process::exit(0);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_command_name() {
        fn expect_command_name(
            input: &str,
            expected_rest: &str,
            expected_result: Result<CommandName, ()>,
        ) {
            println!("{:?}", input);
            let mut arguments = Arguments::from(input);
            let result = arguments.get_command_name().map_err(|_| ());
            assert_eq!(result, expected_result);
            if expected_result.is_ok() {
                assert_eq!(arguments.get_rest(), expected_rest);
            }
        }

        expect_command_name(" ts  ", "", Err(()));
        expect_command_name("break  ", "", Err(()));
        expect_command_name("break ts", "", Err(()));

        expect_command_name("help", "", Ok(CommandName::Help));
        expect_command_name("h", "", Ok(CommandName::Help));
        expect_command_name(" help me! ", "me!", Ok(CommandName::Help));
        expect_command_name(
            "break   list somethign",
            "somethign",
            Ok(CommandName::BreakList),
        );
        expect_command_name("b l", "", Ok(CommandName::BreakList));
        expect_command_name("    bl ts", "ts", Ok(CommandName::BreakList));
    }

    #[test]
    #[should_panic]
    fn another_command_name_panics() {
        let mut arguments = Arguments::from("help help");
        let _ = arguments.get_command_name();
        let _ = arguments.get_command_name();
    }
}
