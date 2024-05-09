#![allow(unused)] // Remove later

use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use clap::{Parser, Subcommand};
use colored::Colorize;

mod command;
mod state;

/// Lace is a complete compiler and interpreter toolchain for the LC3 assembly language.
#[derive(Parser)]
#[command(version)]
struct Args {
    #[command(subcommand)]
    command: Option<Subcommands>,

    /// Run commands in debug mode
    #[arg(long)]
    debug: bool,
}

#[derive(Subcommand)]
enum Subcommands {
    /// Run .asm or .obj file using interpreter and output to terminal
    Run {
        /// Map the LC3 operating system ROM to memory to handle traps and interrupts
        #[arg(short, long)]
        os: bool,
        /// .asm file to run
        name: String,
    },
    /// Create binary .obj files to run later or view compiled data
    Compile {
        /// .asm file to compile
        name: String,
        /// destination to output .obj file
        dest: Option<String>,
    },
    /// Remove compilation artifacts for specified source
    Clean {
        /// .asm file to try remove artifacts for
        name: String,
    },
    /// Place a watch on a .asm file to receive constant compiler updates
    Watch {
        /// .asm file to watch
        name: String,
    },
    /// Format .asm file to adhere to recommended style
    Fmt {
        /// .asm file to format
        name: String,
    },
}

fn main() {
    let args = Args::parse();

    if args.command.is_none() {
        println!("\n~ lace v{VERSION} - Copyright (c) 2024 Artemis Rosman ~");
        println!("{}", LOGO.truecolor(255, 183, 197).bold());
        println!("{SHORT_INFO}");
        std::process::exit(0);
    }

    let debug = args.debug;
    let command = args.command.unwrap_or_else(|| {
        println!("{SHORT_HELP}");
        std::process::exit(1);
    });

    match command {
        Subcommands::Run { os, name } => todo!(),
        Subcommands::Compile { name, dest } => {
            // Parse file into a buffer and symbol table
            let file = File::open(&name).unwrap_or_else(|err| {
                eprintln!(
                    "Failed to open file with path {}: {}",
                    name.bold(),
                    err.to_string().red()
                );
                std::process::exit(1)
            });

            // Process lines and check for wrong file type
            let lines = BufReader::new(file)
                .lines()
                .enumerate()
                .map(|(i, line)| {
                    line.unwrap_or_else(|err| {
                        eprintln!("Failed to read line {}: {}", i, err.to_string().red());
                        eprintln!(" --> {}:{}", name, i);
                        eprintln!(
                            "Check that you are providing a valid {} file.",
                            ".asm".bold()
                        );
                        std::process::exit(1)
                    })
                })
                .collect::<Vec<String>>();

            // Start parsing lines into symbol table and IR
            todo!()
        }
        Subcommands::Clean { name } => todo!(),
        Subcommands::Watch { name } => todo!(),
        Subcommands::Fmt { name } => todo!(),
    }
}

const LOGO: &str = r#"      ..                                  
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
Welcome to lace (a.k.a. LAIS - LC3 Assembler & Interpreter System), an all-in-one toolchain
for working with Little Computer 3 assembly code. Please use `-h` or `--help` to access
the usage instructions and documentation.
";

const SHORT_HELP: &'static str = r"
Unable to recognise command. Please use `-h` or `--help` to view usage instructions.
";

const VERSION: &str = env!("CARGO_PKG_VERSION");
