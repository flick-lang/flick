mod ast;
mod compiler;
mod lexer;
mod parser;
mod token;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use crate::compiler::Compiler;
use anyhow::Result;
use clap::Parser as ClapParser;

use crate::lexer::Lexer;
use crate::parser::Parser;

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    file_path: PathBuf,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let mut file = File::open(cli.file_path)?;
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)?;
    let file_chars: Vec<_> = file_contents.chars().collect();

    let lexer = Lexer::new(&file_chars);
    let tokens: Vec<_> = lexer.collect();

    let mut parser = Parser::new(&tokens);
    let statements = parser.parse();
    let compiler = Compiler::new(&statements);
    println!("{:?}", compiler.compile());

    Ok(())
}
