use std::fs::File;
use std::io::Read;
use std::path::{PathBuf, Path};
use std::process::Command;

use anyhow::{Context, Result};
use clap::error::ErrorKind;
use clap::{CommandFactory, Parser as ClapParser, ValueEnum};

use flick::{error::FlickError, Compiler, Lexer, Parser, Typer};

/// A command line interface using [clap]
#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Input path for source code
    source_path: PathBuf,

    /// What to produce
    #[arg(long, value_enum, default_value_t = Emit::Link)]
    emit: Emit,

    /// Output path (default: the source path with the extension for `--emit`, e.g. `test`, `test.o`, `test.s`, `test.ll` or `test.bc`)
    #[arg(short, long)]
    output_path: Option<PathBuf>,

    /// Path to linker (default: 'cc')
    #[arg(long)]
    linker_path: Option<PathBuf>,

    /// Link a static executable, which needs no shared libraries at runtime (passes `-static` to the linker)
    #[arg(long)]
    r#static: bool,
}

/// The kinds of output the compiler can produce, named after rustc's `--emit` kinds
#[derive(Clone, Copy, PartialEq, Eq, ValueEnum)]
enum Emit {
    /// An executable, linked by running the linker on an object file
    Link,
    /// An object file
    Obj,
    /// Target assembly
    Asm,
    /// Textual LLVM IR, after optimization
    LlvmIr,
    /// Binary LLVM IR (bitcode), after optimization
    LlvmBc,
}

impl Emit {
    /// The file extension for this kind of output
    fn extension(self) -> &'static str {
        match self {
            Emit::Link => "",
            Emit::Obj => "o",
            Emit::Asm => "s",
            Emit::LlvmIr => "ll",
            Emit::LlvmBc => "bc",
        }
    }
}

impl Cli {
    /// Retrieves the provided output path (returns a default if none provided)
    ///
    /// Note that the default output path for a file like `test.fl` is `test`, `test.o`, `test.s`, `test.ll` or `test.bc`, depending on
    /// `--emit`.
    fn get_output_path(&self) -> PathBuf {
        match &self.output_path {
            Some(path) => path.clone(),
            None => self.source_path.with_extension(self.emit.extension()),
        }
    }

    /// Retrieves the provided linker path (returns a default if none provided)
    ///
    /// Note that the default linker path is `cc`.
    fn get_linker_path(&self) -> PathBuf {
        match &self.linker_path {
            Some(path) => path.clone(),
            None => PathBuf::from("cc"),
        }
    }
}

/// Runs the command line interface for the compiler; see [Cli] for details
fn main() -> Result<()> {
    let cli = Cli::parse();
    if cli.r#static && cli.emit != Emit::Link {
        Cli::command()
            .error(ErrorKind::ArgumentConflict, "--static only applies to `--emit link`")
            .exit();
    }

    let mut file = File::open(&cli.source_path)?;
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)?;
    let file_chars: Vec<_> = file_contents.chars().collect();

    let tokens = match Lexer::lex(&file_chars) {
        Ok(tokens) => tokens,
        Err(err) => {
            print_error(&cli.source_path, &file_chars, err);
            std::process::exit(1);
        }
    };

    let program = Parser::parse_program(&tokens);

    let mut typer = Typer::new();
    let typed_program = typer.type_program(&program);

    let mut compiler = Compiler::new();
    compiler.compile(&typed_program);
    compiler.optimize();

    let output_path = cli.get_output_path();
    match cli.emit {
        Emit::Link => link(&cli, &compiler, &output_path)?,
        Emit::Obj => compiler.to_file(&output_path),
        Emit::Asm => compiler.asm_to_file(&output_path),
        Emit::LlvmIr => compiler.ir_to_file(&output_path),
        Emit::LlvmBc => compiler.bitcode_to_file(&output_path),
    }

    Ok(())
}

/// Writes an object file and runs the linker on it to produce the executable at `output_path`
fn link(cli: &Cli, compiler: &Compiler, output_path: &Path) -> Result<()> {
    // Only an intermediate for the linker, so it can't clobber a `test.o` made
    // with `--emit obj`. A private temp dir rather than a fixed name in the
    // shared one, which other users could pre-create as a symlink.
    let object_dir = tempfile::tempdir().context("failed to create a temporary directory")?;
    let object_path = object_dir.path().join("flick.o");
    compiler.to_file(&object_path);

    let linker_path = cli.get_linker_path();
    let status = Command::new(&linker_path)
        .arg(&object_path)
        .arg("-o")
        .arg(output_path)
        .args(cli.r#static.then_some("-static"))
        .status()
        .with_context(|| format!("failed to run linker '{}'", linker_path.display()))?;
    if !status.success() {
        anyhow::bail!("linker '{}' failed ({status})", linker_path.display());
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
