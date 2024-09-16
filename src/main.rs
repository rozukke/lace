#![allow(unused)] // Remove later

use std::fs::{self, File};
use std::io::Write;
use std::ops::RangeBounds;
use std::path::PathBuf;
use std::process::exit;
use std::time::Duration;

use clap::builder::styling::Style;
use clap::{Parser, Subcommand};
use colored::Colorize;
use hotwatch::notify::Event;
use hotwatch::{
    blocking::{Flow, Hotwatch},
    EventKind,
};
use miette::{GraphicalTheme, IntoDiagnostic, MietteHandlerOpts, Result};

use lace::{reset_state, AsmParser, RunState, StaticSource};

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
    let args = Args::parse();

    if let Some(command) = args.command {
        match command {
            Command::Run { name } => {
                let contents = StaticSource::new(fs::read_to_string(&name).into_diagnostic()?);
                println!(
                    "{:>12} target {}",
                    "Assembling".green().bold(),
                    name.to_str().unwrap()
                );
                // Process asm
                let parser = lace::AsmParser::new(contents.src())?;
                let mut air = parser.parse()?;
                air.backpatch()?;
                // Run file
                println!("{:>12} binary", "Running".green().bold());
                let mut program = RunState::try_from(air)?;
                program.run();
                println!(
                    "{:>12} target {}",
                    "Completed".green().bold(),
                    name.to_str().unwrap()
                );
                Ok(())
            }
            Command::Compile { name, dest } => {
                let contents = StaticSource::new(fs::read_to_string(&name).into_diagnostic()?);
                println!(
                    "{:>12} target {}",
                    "Assembling".green().bold(),
                    name.to_str().unwrap()
                );
                // Process asm
                let parser = lace::AsmParser::new(contents.src())?;
                let mut air = parser.parse()?;
                air.backpatch()?;
                // Write to file
                let out_file_name = dest.unwrap_or(
                    format!("{}.lc3", name.file_stem().unwrap().to_str().unwrap()).into(),
                );
                let mut file = File::create(&out_file_name).unwrap();
                // Deal with .orig
                if let Some(orig) = air.orig() {
                    file.write(&orig.to_be_bytes());
                } else {
                    file.write(&0x3000u16.to_be_bytes());
                }
                // Write lines
                for i in 0..air.len() {
                    file.write(&air.get(i).emit()?.to_be_bytes());
                }
                println!("{:>12} binary", "Finished".green().bold(),);
                println!(
                    "{:>12} {}",
                    "Saved to".green().bold(),
                    out_file_name.to_str().unwrap()
                );
                Ok(())
            }
            Command::Check { name } => {
                println!(
                    "{:>12} target {}",
                    "Checking".green().bold(),
                    name.to_str().unwrap()
                );
                check(&name);
                println!("{:>12} with 0 errors", "Finished".green().bold(),);
                Ok(())
            }
            Command::Clean { name } => todo!(),
            Command::Watch { name } => {
                // Clear screen and move cursor to top left
                print!("\x1B[2J\x1B[2;1H");
                println!(
                    "{:>12} target {}",
                    "Watching".green().bold(),
                    name.clone().to_str().unwrap()
                );
                println!("{:>12} press CTRL+C to exit", "TIP:".cyan());
                let mut watcher = Hotwatch::new_with_custom_delay(Duration::from_millis(500))
                    .into_diagnostic()?;
                watcher
                    .watch(name.clone(), move |event: Event| match event.kind {
                        EventKind::Modify(_) => {
                            // Clear screen
                            print!("\x1B[2J\x1B[2;1H");
                            println!(
                                "{:>12} target {}",
                                "Watching".green().bold(),
                                name.clone().to_str().unwrap()
                            );
                            println!("{:>12} re-checking...", "File changed".green());
                            println!("{:>12} press CTRL+C to exit", "Help".cyan());
                            match check(&name) {
                                Ok(_) => {
                                    println!("{:>12} no errors found", "Success!".green());
                                }
                                Err(e) => {
                                    println!("\n{:?}", e);
                                }
                            };
                            reset_state();
                            Flow::Continue
                        }
                        EventKind::Remove(_) => {
                            println!("{}", "Watched file was deleted. Exiting...".red());
                            std::process::exit(1);
                        }
                        _ => Flow::Continue,
                    })
                    .into_diagnostic()?;
                watcher.run();
                Ok(())
            }
            Command::Fmt { name } => todo!(),
        }
    } else {
        if let Some(path) = args.path {
            todo!("Should allow for running files with no subcommand")
        } else {
            println!("\n~ lace v{VERSION} - Copyright (c) 2024 Artemis Rosman ~");
            println!("{}", LOGO.truecolor(255, 183, 197).bold());
            println!("{SHORT_INFO}");
            std::process::exit(0);
        }
    }
}

fn check(name: &PathBuf) -> Result<()> {
    let contents = StaticSource::new(fs::read_to_string(&name).into_diagnostic()?);
    let parser = lace::AsmParser::new(contents.src())?;
    let mut air = parser.parse()?;
    air.backpatch()?;
    for stmt in air {
        let _ = stmt.emit()?;
    }
    Ok(())
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

const SHORT_HELP: &str = r"
Unable to recognise command. Please use `-h` or `--help` to view usage instructions.
";

const VERSION: &str = env!("CARGO_PKG_VERSION");
