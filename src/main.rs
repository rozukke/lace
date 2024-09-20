use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use std::time::Duration;

use clap::{Parser, Subcommand};
use colored::Colorize;
use hotwatch::notify::Event;
use hotwatch::{
    blocking::{Flow, Hotwatch},
    EventKind,
};
use miette::{IntoDiagnostic, Result};

use lace::reset_state;
use lace::{Air, RunState, StaticSource};

/// Lace is a complete & convenient assembler toolchain for the LC3 assembly language.
#[derive(Parser)]
#[command(version)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    /// Quickly provide a `.asm` file to run
    path: Option<PathBuf>,
}

#[derive(Subcommand)]
enum Command {
    /// Run text `.asm` or binary `.lc3` file directly and output to terminal
    Run {
        /// .asm file to run
        name: PathBuf,
    },
    /// Create binary `.lc3` file to run later or view compiled data
    Compile {
        /// `.asm` file to compile
        name: PathBuf,
        /// Destination to output .lc3 file
        dest: Option<PathBuf>,
    },
    /// Check a `.asm` file without running or outputting binary
    Check {
        /// File to check
        name: PathBuf,
    },
    /// Remove compilation artifacts for specified source
    Clean {
        /// `.asm` file to try remove artifacts for
        name: PathBuf,
    },
    /// Place a watch on a `.asm` file to receive constant assembler updates
    Watch {
        /// `.asm` file to watch
        name: PathBuf,
    },
    /// Format `.asm` file to adhere to recommended style
    Fmt {
        /// `.asm` file to format
        name: PathBuf,
    },
}

fn main() -> miette::Result<()> {
    use MsgColor::*;
    let args = Args::parse();

    if let Some(command) = args.command {
        match command {
            Command::Run { name } => {
                run(&name)?;
                Ok(())
            }
            Command::Compile { name, dest } => {
                file_message(Green, "Assembing", &name);
                let contents = StaticSource::new(fs::read_to_string(&name).into_diagnostic()?);
                let air = assemble(&contents)?;

                let out_file_name =
                    dest.unwrap_or(name.with_extension("lc3").file_stem().unwrap().into());
                let mut file = File::create(&out_file_name).unwrap();

                // Deal with .orig
                if let Some(orig) = air.orig() {
                    let _ = file.write(&orig.to_be_bytes());
                } else {
                    let _ = file.write(&0x3000u16.to_be_bytes());
                }

                // Write lines
                for stmt in air {
                    let _ = file.write(&stmt.emit()?.to_be_bytes());
                }

                message(Green, "Finished", "emit binary");
                file_message(Green, "Saved", &out_file_name);
                Ok(())
            }
            Command::Check { name } => {
                file_message(Green, "Checking", &name);
                let contents = StaticSource::new(fs::read_to_string(&name).into_diagnostic()?);
                let _ = assemble(&contents)?;
                message(Green, "Success", "no errors found!");
                Ok(())
            }
            Command::Clean { name: _ } => todo!("There are no debug files implemented to clean!"),
            Command::Watch { name } => {
                // Clear screen and move cursor to top left
                print!("\x1B[2J\x1B[2;1H");
                file_message(Green, "Watching", &name);
                message(Cyan, "Help", "press CTRL+C to exit");

                let mut watcher = Hotwatch::new_with_custom_delay(Duration::from_millis(500))
                    .into_diagnostic()?;

                watcher
                    .watch(name.clone(), move |event: Event| match event.kind {
                        EventKind::Modify(_) => {
                            // Clear screen
                            print!("\x1B[2J\x1B[2;1H");
                            file_message(Green, "Watching", &name);
                            message(Green, "Re-checking", "file change detected");
                            message(Cyan, "Help", "press CTRL+C to exit");

                            let mut contents = StaticSource::new(
                                fs::read_to_string(&name).into_diagnostic().unwrap(),
                            );
                            let _ = match assemble(&contents) {
                                Ok(_) => {
                                    message(Green, "Success", "no errors found!");
                                }
                                Err(e) => {
                                    println!("\n{:?}", e);
                                }
                            };

                            reset_state();
                            // To avoid leaking memory
                            contents.reclaim();
                            Flow::Continue
                        }
                        EventKind::Remove(_) => {
                            if name.exists() {
                                Flow::Continue
                            } else {
                                message(Red, "Error", "watched file was deleted. Exiting...");
                                std::process::exit(1);
                            }
                        }
                        _ => Flow::Continue,
                    })
                    .into_diagnostic()?;
                watcher.run();
                Ok(())
            }
            Command::Fmt { name: _ } => todo!("Formatting is not currently implemented"),
        }
    } else {
        if let Some(path) = args.path {
            run(&path)?;
            Ok(())
        } else {
            println!("\n~ lace v{VERSION} - Copyright (c) 2024 Artemis Rosman ~");
            println!("{}", LOGO.truecolor(255, 183, 197).bold());
            println!("{SHORT_INFO}");
            std::process::exit(0);
        }
    }
}

enum MsgColor {
    Green,
    Cyan,
    Red,
}

fn file_message(color: MsgColor, left: &str, right: &PathBuf) {
    let right = format!("target {}", right.to_str().unwrap());
    message(color, left, &right);
}

fn message<S>(color: MsgColor, left: S, right: S)
where
    S: Colorize + std::fmt::Display,
{
    let left = match color {
        MsgColor::Green => left.green(),
        MsgColor::Cyan => left.cyan(),
        MsgColor::Red => left.red(),
    };
    println!("{left:>12} {right}");
}

fn run(name: &PathBuf) -> Result<()> {
    file_message(MsgColor::Green, "Assembling", &name);
    let contents = StaticSource::new(fs::read_to_string(&name).into_diagnostic()?);
    let air = assemble(&contents)?;

    message(MsgColor::Green, "Running", "emitted binary");
    let mut program = RunState::try_from(air)?;
    program.run();

    file_message(MsgColor::Green, "Completed", &name);
    Ok(())
}

/// Return assembly intermediate representation of source file for further processing
fn assemble(contents: &StaticSource) -> Result<Air> {
    let parser = lace::AsmParser::new(contents.src())?;
    let mut air = parser.parse()?;
    air.backpatch()?;
    Ok(air)
}

const LOGO: &str = r#"
      ..                                  
x .d88"                                   
 5888R                                    
 '888R         u           .        .u    
  888R      us888u.   .udR88N    ud8888.  
  888R   .@88 "8888" <888'888k :888'8888. 
  888R   9888  9888  9888 'Y"  d888 '88%" 
  888R   9888  9888  9888      8888.+"    
  888R   9888  9888  9888      8888L      
 .888B . 9888  9888  ?8888u../ '8888c. .+ 
 ^*888%  "888*""888"  "8888P'   "88888%   
   "%     ^Y"   ^Y'     "P'       "YP'"#;

const SHORT_INFO: &str = r"
Welcome to lace (from LAIS - LC3 Assembler & Interpreter System),
an all-in-one toolchain for working with LC3 assembly code.
Please use `-h` or `--help` to access the usage instructions and documentation.
";

const VERSION: &str = env!("CARGO_PKG_VERSION");
