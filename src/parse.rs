use crate::token::Token;
use colored::Colorize;
use std::{
    fs::File,
    io::{BufRead, BufReader, Read},
    path::PathBuf,
};

pub struct FileAndPath {
    file: File,
    path: PathBuf,
}

impl FileAndPath {
    pub fn open(path: &str) -> FileAndPath {
        let path = PathBuf::from(&path);
        let file = File::open(&path).unwrap_or_else(|err| {
            eprintln!(
                "Failed to open file with path {}: {}",
                path.display(),
                err.to_string().red()
            );
            std::process::exit(1)
        });

        FileAndPath { file, path }
    }

    pub fn tokenize_asm(&self) -> Vec<Vec<Token>> {
        // Process lines and check for wrong file type
        let contents = BufReader::new(&self.file)
            .lines()
            .enumerate()
            .map(|(i, line)| {
                line.unwrap_or_else(|err| {
                    eprintln!("Failed to read line {}: {}", i, err.to_string().red());
                    eprintln!(" --> {}:{}", self.path.display(), i);
                    eprintln!(
                        "Check that you are providing a valid {} file.",
                        ".asm".bold()
                    );
                    std::process::exit(1)
                })
            })
            .collect::<Vec<String>>();

        // Turn lines into a vector that contains a list of tokens for each line
        contents
            .iter()
            .enumerate()
            .map(|(i, line)| {
                // Get line without comment & whitespace
                println!("{}", line);
                let sc_idx = if let Some(idx) = line.find(';') {
                    idx
                } else {
                    line.len()
                };
                let clean_str = &line[..sc_idx];

                // Split on commas and spaces -> vec
                let arr_list: Vec<Token> = clean_str
                    .split(|c| c == ' ' || c == ',')
                    .filter(|word| !word.is_empty())
                    .map(|word| Token {
                        val: word.into(),
                        line: (i + 1) as u16,
                        col: unsafe { word.as_ptr().offset_from(line.as_ptr()) + 1 } as u16,
                    })
                    .collect::<Vec<Token>>();
                arr_list
            })
            // Filter after iteration to preserve correct line numbers in tokens
            .filter(|line| !line.is_empty())
            .collect()
    }
}
