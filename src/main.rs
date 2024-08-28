use std::fs::File;
use std::io::Read;
use std::path::{PathBuf, Path};
use std::process::Command;

use anyhow::Result;
use clap::Parser as ClapParser;

use flick::{error::FlickError, Compiler, Lexer, Parser, Typer};

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

    /// Path to linker (default: 'gcc')
    #[arg(long)]
    linker_path: Option<PathBuf>,

    /// Whether to just compile without running the linker to generate an executable
    #[arg(long)]
    no_link: bool,
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

    /// Retrieves the provided linker path (returns a default if none provided)
    ///
    /// Note that the default linker path is `gcc`.
    fn get_linker_path(&self) -> PathBuf {
        match &self.linker_path {
            Some(path) => path.clone(),
            None => PathBuf::from("gcc"),
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

    let tokens = match Lexer::lex(&file_chars) {
        Ok(tokens) => tokens,
        Err(err) => {
            print_error(&cli.source_path, &file_chars, err);
            return Ok(());
        }
    };

    let program = Parser::parse_program(&tokens);

    let mut typer = Typer::new();
    let typed_program = typer.type_program(&program);

    let mut compiler = Compiler::new();
    compiler.compile(&typed_program);

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

    if cli.no_link {
        return Ok(());
    }

    let executable_output_path = cli.get_executable_output_path();
    let linker_path = cli.get_linker_path();
    Command::new(linker_path)
        .arg(&object_output_path)
        .arg("-o")
        .arg(&executable_output_path)
        .output()?;

    if cli.object_output_path.is_none() {
        std::fs::remove_file(&object_output_path)?;
    }

    Ok(())
}


fn print_error(source_path: impl AsRef<Path>, file_chars: &[char], error: FlickError) {
    // TODO: Write to string then print string to stderr instead of printing to stderr directly

    let line_start_index = file_chars
        .iter()
        .take(error.index)
        .take_while(|c: &&char| **c != '\n')
        .count() + 1;
    let line_num = file_chars[..line_start_index].iter().filter(|&&c| c == '\n').count() + 1;
    let col_num = error.index - line_start_index + 1;
    let line = &file_chars[line_start_index..]
        .iter()
        .take_while(|&&c| c != '\n')
        .collect::<String>();

    eprintln!("{}: {}", "error", error);

    let max_line_num_width = (line_num+1).to_string().len();
    // if line_num > 1 {
    //     write_source_code_line(&file_chars[line_start_index - line.len()..line_start_index], line_num - 1, max_line_num_width);
    // }
    write_source_code_line(line, line_num, max_line_num_width);
    // let num_lines = file_chars.iter().filter(|&&c| c == '\n').count() + 1;
    // if line_num < num_lines {
    //     write_source_code_line(&file_chars[line_start_index + line.len()..], line_num + 1, max_line_num_width);
    // }
    eprintln!("in {}:{}:{}", source_path.as_ref().display(), line_num, col_num);
}

fn write_source_code_line(line: &str, line_num: usize, max_line_num_width: usize) {
    eprintln!(" {:0>width$} │ {}", line_num, line, width = max_line_num_width);
}
