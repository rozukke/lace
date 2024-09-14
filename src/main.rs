#![allow(unused)] // Remove later

use std::fs::{self, File};
use std::io::Write;
use std::ops::RangeBounds;
use std::path::PathBuf;

use clap::builder::styling::Style;
use clap::{Parser, Subcommand};
use colored::Colorize;
use miette::{GraphicalTheme, IntoDiagnostic, MietteHandlerOpts, Result};

use lace::AsmParser;

/// Lace is a complete & convenient assembler toolchain for the LC3 assembly language.
#[derive(Parser)]
#[command(version)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    /// Run commands in debug mode
    #[arg(long)]
    debug: bool,
}

#[derive(Subcommand)]
enum Command {
    /// Run text `.asm` or binary `.lc3` file directly and output to terminal
    Run {
        /// Map the LC3 operating system ROM to memory to handle traps and interrupts
        #[arg(short, long)]
        os: bool,
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
            Command::Run { os, name } => todo!(),
            Command::Compile { name, dest } => {
                // Available until end of program
                let contents: &'static str =
                    Box::leak(Box::new(fs::read_to_string(&name).into_diagnostic()?));
                // Process asm
                let parser = lace::AsmParser::new(&contents)?;
                let mut air = parser.parse()?;
                air.backpatch()?;
                // Write to file
                let mut file = File::create(dest.unwrap_or(
                    format!("{}.lc3", name.file_stem().unwrap().to_str().unwrap()).into(),
                ))
                .unwrap();
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
                Ok(())
            }
            Command::Clean { name } => todo!(),
            Command::Watch { name } => todo!(),
            Command::Fmt { name } => todo!(),
        }
    } else {
        println!("\n~ lace v{VERSION} - Copyright (c) 2024 Artemis Rosman ~");
        println!("{}", LOGO.truecolor(255, 183, 197).bold());
        println!("{SHORT_INFO}");
        std::process::exit(0);
    }
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
Welcome to lace (from LAIS - LC3 Assembler & Interpreter System), an all-in-one toolchain
for working with LC3 assembly code. Please use `-h` or `--help` to access
the usage instructions and documentation.
";

const SHORT_HELP: &str = r"
Unable to recognise command. Please use `-h` or `--help` to view usage instructions.
";

const VERSION: &str = env!("CARGO_PKG_VERSION");
