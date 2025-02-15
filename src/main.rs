use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::thread::sleep;
use std::time::Duration;

use clap::{Parser, Subcommand};
use colored::Colorize;
use hotwatch::notify::Event;
use hotwatch::{
    blocking::{Flow, Hotwatch},
    EventKind,
};
use miette::{bail, IntoDiagnostic, Result};

use lace::{reset_state, DebuggerOptions};
use lace::{Air, RunEnvironment, StaticSource};

/// Lace is a complete & convenient assembler toolchain for the LC3 assembly language.
#[derive(Parser)]
#[command(version)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    /// Quickly provide a `.asm` file to run
    path: Option<PathBuf>,
    // TODO: Include `--minimal` option here, mirroring `run` subcommand
}

#[derive(Subcommand)]
enum Command {
    /// Run text `.asm` or binary `.lc3` file directly and output to terminal
    Run {
        /// `.asm` or `.lc3` file to run
        name: PathBuf,
        /// Produce minimal output, suited for blackbox tests
        #[arg(short, long)]
        minimal: bool,
    },
    /// Run text `.asm` file directly and with debugger
    Debug {
        /// `.asm` file to run
        name: PathBuf,
        /// Read debugger commands from argument
        #[arg(short, long)]
        command: Option<String>,
        /// Produce minimal output, suited for blackbox tests
        #[arg(short, long)]
        minimal: bool,
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
    lace::env::init();

    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new() //
                .context_lines(lace::DIAGNOSTIC_CONTEXT_LINES)
                .build(),
        )
    }))?;

    if let Some(command) = args.command {
        match command {
            Command::Run { name, minimal } => {
                run(&name, None, minimal)?;
                Ok(())
            }
            Command::Debug {
                name,
                command,
                minimal,
            } => {
                run(&name, Some(DebuggerOptions { command }), minimal)?;
                Ok(())
            }
            Command::Compile { name, dest } => {
                file_message(Green, "Assembling", &name);
                let contents = StaticSource::new(fs::read_to_string(&name).into_diagnostic()?);
                let air = assemble(&contents)?;

                let out_file_name =
                    dest.unwrap_or(name.with_extension("lc3").file_name().unwrap().into());
                let mut file = File::create(&out_file_name).unwrap();

                // Deal with .orig
                if let Some(orig) = air.orig() {
                    let _ = file.write(&orig.to_be_bytes());
                } else {
                    let _ = file.write(&0x3000u16.to_be_bytes());
                }

                // Write lines
                for stmt in &air {
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
                if !name.exists() {
                    bail!("File does not exist. Exiting...")
                }
                // Vim breaks if watching a single file
                let folder_path = match name.parent() {
                    Some(pth) if pth.is_dir() => pth.to_path_buf(),
                    _ => Path::new(".").to_path_buf(),
                };

                // Clear screen and move cursor to top left
                print!("\x1B[2J\x1B[2;1H");
                file_message(Green, "Watching", &name);
                message(Cyan, "Help", "press CTRL+C to exit");

                let mut watcher = Hotwatch::new_with_custom_delay(Duration::from_millis(500))
                    .into_diagnostic()?;

                watcher
                    .watch(folder_path, move |event: Event| match event.kind {
                        // Watch remove for vim changes
                        EventKind::Modify(_) | EventKind::Remove(_) => {
                            // Clear screen
                            print!("\x1B[2J\x1B[2;1H");
                            file_message(Green, "Watching", &name);
                            message(Green, "Re-checking", "file change detected");
                            message(Cyan, "Help", "press CTRL+C to exit");

                            // Now we are developing software (makes reruns more obvious)
                            sleep(Duration::from_millis(50));

                            let mut contents = StaticSource::new(match fs::read_to_string(&name) {
                                Ok(cts) => cts,
                                Err(e) => {
                                    eprintln!("{e}. Exiting...");
                                    std::process::exit(1)
                                }
                            });
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
            run(&path, None, false)?;
            Ok(())
        } else {
            println!("\n~ lace v{VERSION} - Copyright (c) 2024 Artemis Rosman ~");
            println!("{}", LOGO.truecolor(255, 183, 197).bold());
            println!("{SHORT_INFO}");
            std::process::exit(0);
        }
    }
}

#[allow(unused)]
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

fn run(name: &PathBuf, debugger_opts: Option<DebuggerOptions>, minimal: bool) -> Result<()> {
    file_message(MsgColor::Green, "Assembling", &name);
    let mut program = if let Some(ext) = name.extension() {
        match ext.to_str().unwrap() {
            "lc3" | "obj" => {
                if debugger_opts.is_some() {
                    bail!("Cannot use debugger on non-assembly file");
                }

                // Read to byte buffer
                let mut file = File::open(&name).into_diagnostic()?;
                let f_size = file.metadata().unwrap().len();
                let mut buffer = Vec::with_capacity(f_size as usize);
                file.read_to_end(&mut buffer).into_diagnostic()?;

                if buffer.len() % 2 != 0 {
                    bail!("File is not aligned to 16 bits")
                }

                let u16_buf: Vec<u16> = buffer
                    .chunks_exact(2)
                    .map(|word| u16::from_be_bytes([word[0], word[1]]))
                    .collect();
                RunEnvironment::from_raw(&u16_buf)?
            }
            "asm" => {
                let contents = StaticSource::new(fs::read_to_string(&name).into_diagnostic()?);
                let air = assemble(&contents)?;
                RunEnvironment::try_from(air, debugger_opts)?
            }
            _ => {
                bail!("File has unknown extension. Exiting...")
            }
        }
    } else {
        bail!("File has no extension. Exiting...");
    };

    program.set_minimal(minimal);

    message(MsgColor::Green, "Running", "emitted binary");
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
