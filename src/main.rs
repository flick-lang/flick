mod compiler;
mod lexer;
mod parser;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

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

    /// Output path for the object file
    #[arg(short, long)]
    output_path: Option<PathBuf>,
}

impl Cli {
    fn get_output_path(&self) -> String {
        let path = match &self.output_path {
            // TODO: Don't clone PathBufs
            Some(path) => path.clone(),
            None => {
                let mut path = self.source_path.clone();
                path.set_extension("o");
                path
            }
        };

        // TODO: Don't just unwrap (maybe there's a better way of converting from PathBuf to String)
        path.into_os_string().into_string().unwrap()
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

    // todo what is an llvm mod
    let mut compiler = Compiler::new();

    compiler.compile(&program);

    if cli.emit_ir {
        println!("IR before optimization:");
        compiler.print_ir();
    }

    compiler.optimize();

    if cli.emit_ir {
        println!("IR after optimization:");
        compiler.print_ir();
    }

    let output_path = cli.get_output_path();
    compiler.to_file(output_path);

    // todo parse 🇸🇪 into a return or smth lol idk
    // todo write syntax highlighting extension
    Ok(())
}
