#![doc = include_str!("../README.md")]

/// Module to convert [abstract syntax trees](parser::ast) into LLVM using llvm-sys
///
/// The general idea is to take code objects (e.g. variables or functions) and form
/// instances of [LLVMValueRef][a]. Then, after we're done, we ask llvm-sys to
/// generate LLVM code.
///
/// [a]: llvm_sys::prelude::LLVMValueRef
mod compiler;
/// Module to convert source files into token streams
mod lexer;
/// Module to convert token streams into [abstract syntax trees](parser::ast)
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

/// A command line interface using [clap]
#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Input path for source code
    source_path: PathBuf,

    /// Whether to print LLVM intermediate representation during compilation
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
    /// Retrieves the provided executable output path (returns a default if none provided)
    ///
    /// Note that the default executable output path for a file like `test.fl` is `test`.
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

    /// Retrieves the provided object output path (returns a default if none provided)
    ///
    /// Note that the default object output path for a file like `test.fl` is `test.o`.
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

/// Runs the command line interface for the compiler; see [Cli] for details
fn main() -> Result<()> {
    let cli = Cli::parse();

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
    Command::new("ld")
        .arg(&object_output_path)
        .arg("-o")
        .arg(&executable_output_path)
        .output()?;

    if cli.object_output_path.is_none() {
        std::fs::remove_file(&object_output_path)?;
    }

    // TODO(tbreydo): remove this next line once we implement flick run/build
    // Command::new(&executable_output_path).output()?;

    Ok(())
}
