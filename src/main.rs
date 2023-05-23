mod lexer;
mod token;

use std::path::PathBuf;
use std::fs::File;
use std::io::BufReader;
use std::io::Read;

use clap::Parser;
use anyhow::Result;

use crate::lexer::Lexer;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    file_path: PathBuf,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let file = File::open(cli.file_path)?;
    let mut buf_reader = BufReader::new(file);
    let mut file_contents = String::new();
    buf_reader.read_to_string(&mut file_contents)?;

    let mut lexer: Lexer = Lexer::new(&file_contents);
    while let Some(token) = lexer.next_token() {
        println!("{:?}", token);
    }

    Ok(())
}

