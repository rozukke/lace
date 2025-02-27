use super::{error, Arguments, CommandName};

macro_rules! name_list {
    [
        $(
            $name:ident
            [ $( $candidate:literal ),* $(,)? ]
            [ $( $misspellings:literal ),* $(,)? ]
        )*
    ] => {
        &[
            $( CommandNameEntry {
                name: CommandName::$name,
                candidates: &[ $($candidate,)* ],
                misspellings: &[ $($misspellings,)* ],
            }, )*
        ]
    };
}
const COMMANDS: &'static [CommandNameEntry] = name_list![
    Help
        ["h", "help", "--help", "-h", "HELP", "man", "info", "wtf"]
        []
    Next
        ["s", "step"]
        ["next"]
    Step
        ["si", "stepinto", "stepin", "step-into", "step-in", "stepi", "step-i"]
        []
    Finish
        ["so", "stepout", "step-out", "stepo", "step-o"]
        ["finish", "fin"]
    Continue
        ["c", "continue", "cont", "con"]
        []
    Get
        ["p", "print"]
        ["get"]
    Set
        ["m", "move", "mov", "mv"]
        ["set"]
    Registers
        ["r", "registers", "register", "reg", "regs"]
        []
    Jump
        ["g", "goto", "go"]
        ["jump", "jsr", "jsrr", "call"]
    Source
        ["a", "assembly", "asm"]
        ["source", "src"]
    Eval
        ["e", "eval", "evil"]
        ["run", "exec", "execute", "sim", "simulate"]
    Reset
        ["z", "reset"]
        []
    Echo
        ["echo"]
        []
    Quit
        ["q", "quit"]
        []
    Exit
        ["x", "exit", ":q", ":wq", "^C"]
        ["halt", "end", "stop"]
    BreakList
        ["bl", "breaklist", "break-list", "break-ls"]
        []
    BreakAdd
        ["ba", "breakadd", "break-add"]
        []
    BreakRemove
        ["br", "breakremove", "break-remove", "break-rm"]
        []
];
const BREAK_COMMAND: CandidateList = &["break", "b"];
const BREAK_SUBCOMMANDS: &'static [CommandNameEntry] = name_list![
    BreakList
        ["l", "list", "ls"]
        []
    BreakAdd
        ["a", "add"]
        []
    BreakRemove
        ["r", "remove", "rm"]
        []
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

        // "break" is currently the only command with subcommands, so it is treated specially
        // This could be written a bit nicer. But it doesn't seem necessary.
        if name_matches(command_name, BREAK_COMMAND) {
            // Normalize name and get as `'static`
            // Only used for errors
            let command_name = BREAK_COMMAND[0]; // Array must be non-empty if this branch is being ran

            let Some(subcommand_name) = self.next_token_str() else {
                return Err(error::Command::MissingSubcommand { command_name });
            };
            match find_name_match(subcommand_name, BREAK_SUBCOMMANDS) {
                Ok(command) => return Ok(command),
                Err(_) => {
                    return Err(error::Command::InvalidSubcommand {
                        command_name,
                        subcommand_name: subcommand_name.to_string(),
                    });
                }
            }
        }

        match find_name_match(command_name, COMMANDS) {
            Ok(command) => return Ok(command),

            Err(suggested) => {
                // User clearly wants return to bash
                if command_name == "sudo" {
                    println!("Goodbye");
                    std::process::exit(0);
                }

                return Err(error::Command::InvalidCommand {
                    command_name: command_name.to_string(),
                    suggested,
                });
            }
        }
    }
}

/// A [`CommandName`] with a list of name candidates and misspellings which should trigger a
/// suggestion.
struct CommandNameEntry {
    name: CommandName,
    candidates: CandidateList,
    misspellings: CandidateList,
}

/// List of single-word aliases for a command or subcommand.
type CandidateList = &'static [&'static str];

/// Returns the first [`CommandName`], which has a corresponding candidate which matches the
/// `provided` command name (case insensitive).
///
/// Returns `Err(_)` if no match was found, with an optional 'suggested' command name.
fn find_name_match(
    provided: &str,
    entries: &'static [CommandNameEntry],
) -> Result<CommandName, Option<CommandName>> {
    for CommandNameEntry {
        name, candidates, ..
    } in entries
    {
        if name_matches(provided, candidates) {
            return Ok(*name);
        }
    }
    for CommandNameEntry {
        name, misspellings, ..
    } in entries
    {
        if name_matches(provided, misspellings) {
            return Err(Some(*name));
        }
    }
    Err(None)
}

/// Returns `true` if the `provided` command name matchs any item of `candidates` (case
/// insensitive).
fn name_matches(provided: &str, candidates: CandidateList) -> bool {
    for candidate in candidates {
        if provided.eq_ignore_ascii_case(candidate) {
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
