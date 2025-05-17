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
const COMMANDS: &[CommandNameEntry] = name_list![
    Help
        ["h", "help", "--help", "-h", ":h", "man", "info", "wtf"]
        []
    Continue
        ["c", "continue", "cont"]
        ["con", "proceed"]
    Print
        ["p", "print"]
        ["get", "show", "display", "put", "puts", "out"]
    Move
        ["m", "move"]
        ["set", "mov", "mv", "assign"]
    Registers
        ["r", "registers", "reg"]
        ["dump", "register", "regs"]
    Goto
        ["g", "goto"]
        ["jump", "call", "go", "go-to", "jsr", "jsrr", "br", "brn", "brz", "brp", "brnz", "brnp", "brzp", "brnzp"]
    Assembly
        ["a", "assembly", "asm"]
        ["source", "src", "ass", "inspect"]
    Eval
        ["e", "eval", "evil", "evaluate"]
        ["run", "exec", "execute", "sim", "simulate", "instruction", "instr"]
    Reset
        ["z", "reset"]
        ["restart", "refresh", "reboot"]
    Echo // Not included in help
        ["echo"]
        []
    Quit
        ["q", "quit"]
        []
    Exit
        ["x", "exit", ":q", ":wq", "^C"]
        ["halt", "end", "stop"]

    StepOver
        []
        ["next", "step-over", "stepover"]
    StepInto
        ["si", "stepinto"]
        ["into", "in", "stepin", "step-into", "step-in", "stepi", "step-i", "sin"]
    StepOut
        ["so", "stepout"]
        ["finish", "fin", "out", "step-out", "stepo", "step-o", "sout"]

    BreakList
        ["bl", "breaklist"]
        ["break-list", "break-ls", "blist", "bls", "bp", "breakpoint", "breakpointlist", "breakpoint-list"]
    BreakAdd
        ["ba", "breakadd"]
        ["break-add", "badd", "breakpointadd", "breakpoint-add"]
    BreakRemove
        ["br", "breakremove"]
        ["break-remove", "break-rm", "bremove", "brm", "breakpointremove", "breakpoint-remove"]
];
const COMMAND_STEP: CandidateList = &["step", "s"];
const SUBCOMMANDS_STEP: &[CommandNameEntry] = name_list![
    StepOver
        []
        ["next"]
    StepInto
        ["i", "into"]
        ["in"]
    StepOut
        ["o", "out"]
        ["finish", "fin"]
];
const COMMAND_BREAK: CandidateList = &["b", "break"];
const SUBCOMMANDS_BREAK: &[CommandNameEntry] = name_list![
    BreakList
        ["l", "list"]
        ["print", "show", "display", "dump", "ls"]
    BreakAdd
        ["a", "add"]
        ["set", "move"]
    BreakRemove
        ["r", "remove"]
        ["delete", "rm"]
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

        // Subcommands for `step`
        if let Some(command) = self.name_matches_with_subcommand(
            command_name,
            COMMAND_STEP,
            SUBCOMMANDS_STEP,
            Some(CommandName::StepOver),
        )? {
            return Ok(command);
        }
        // Subcommands for `break`
        if let Some(command) =
            self.name_matches_with_subcommand(command_name, COMMAND_BREAK, SUBCOMMANDS_BREAK, None)?
        {
            return Ok(command);
        }

        match find_name_match(command_name, COMMANDS) {
            Ok(command) => Ok(command),

            Err(suggested) => {
                // User clearly wants return to bash
                if command_name == "sudo" {
                    println!("Goodbye");
                    std::process::exit(0);
                }

                Err(error::Command::InvalidCommand {
                    command_name: command_name.to_string(),
                    suggested,
                })
            }
        }
    }

    // TODO(doc)
    fn name_matches_with_subcommand(
        &mut self,
        command_name: &str,
        commands: CandidateList,
        subcommands: &'static [CommandNameEntry],
        default: Option<CommandName>,
    ) -> Result<Option<CommandName>, error::Command> {
        // This could be written a bit nicer. But it doesn't seem necessary.
        if !name_matches(command_name, commands) {
            return Ok(None);
        }

        // Normalize name and get as `'static`
        // Only used for errors
        let command_name = commands[0]; // Array must be non-empty if this branch is being ran

        let Some(subcommand_name) = self.next_token_str() else {
            match default {
                Some(command) => return Ok(Some(command)),
                None => return Err(error::Command::MissingSubcommand { command_name }),
            }
        };

        match find_name_match(subcommand_name, subcommands) {
            Ok(command) => Ok(Some(command)),
            Err(suggested) => Err(error::Command::InvalidSubcommand {
                command_name,
                subcommand_name: subcommand_name.to_string(),
                suggested,
            }),
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
