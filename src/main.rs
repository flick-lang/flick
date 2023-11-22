mod compiler;
mod lexer;
mod parser;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::process::Command;

use anyhow::Result;
use clap::Parser as ClapParser;

use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Input path for source code
    source_path: PathBuf,

    /// Whether to emit LLVM ir
    #[arg(short, long)]
    emit_ir: bool,

    /// Output path for the executable
    #[arg(short, long)]
    output_path: Option<PathBuf>,

    /// Output path for the object file
    #[arg(long)]
    object_output_path: Option<PathBuf>,
}

impl Cli {
    fn get_executable_output_path(&self) -> PathBuf {
        match &self.output_path {
            Some(path) => path.clone(),
            None => {
                let mut path = self.source_path.clone();
                path.set_extension("");
                path
            }
        }
    }

    fn get_object_output_path(&self) -> PathBuf {
        match &self.object_output_path {
            Some(path) => path.clone(),
            None => {
                let mut path = self.source_path.clone();
                path.set_extension("o");
                path
            }
        }
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // TODO: Find a way to not clone it here
    let mut file = File::open(&cli.source_path)?;
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)?;
    let file_chars: Vec<_> = file_contents.chars().collect();

    let lexer = Lexer::new(&file_chars);
    let tokens: Vec<_> = lexer.collect();

    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program();

    let mut compiler = Compiler::new();

    compiler.compile(&program);

    if cli.emit_ir {
        println!("\nIR before optimization:");
        compiler.print_ir();
    }

    compiler.optimize();

    if cli.emit_ir {
        println!("\nIR after optimization:");
        compiler.print_ir();
    }

    let object_output_path = cli.get_object_output_path();
    compiler.to_file(&object_output_path);

    let executable_output_path = cli.get_executable_output_path();
    Command::new("clang")
        .arg(&object_output_path)
        .arg("-o")
        .arg(&executable_output_path)
        .output()?;

    if cli.object_output_path.is_none() {
        std::fs::remove_file(&object_output_path)?;
    }

    // Command::new(&executable_output_path).output()?;

    // todo parse 🇸🇪 into a return or smth lol idk
    // todo write syntax highlighting extension
    Ok(())
}
