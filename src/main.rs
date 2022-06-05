use std::path::PathBuf;
use rustyline::error::ReadlineError;
use rustyline::{Config, EditMode, Editor};
use flick::SourceFile;
use directories::ProjectDirs;
use clap::{ArgEnum, Parser};
use std::fs;
use rustyline::config::Configurer;
use flick::lexer::Tokens;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// What mode to run the program in
    #[clap(short, long, arg_enum, default_value_t = Mode::Tokenize)]
    mode: Mode,

    /// File to run
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,

    /// Use vi to edit commands (if running in the REPL)
    #[clap(short, long)]
    vi_mode: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ArgEnum)]
enum Mode {
    Tokenize,
    Compile,
    Run
}

fn handle_run(cli: Cli) {
    let file = cli.file.unwrap();
    let sf = SourceFile::from_file(file).expect("error reading file");
    match cli.mode {
        Mode::Tokenize => print_tokens(sf.tokens()),
        mode => unimplemented!("{:?}", mode),
    }
}

fn handle_repl(cli: Cli) {
    let mut config_builder = Config::builder()
        .max_history_size(100_000)
        .auto_add_history(true);
    if cli.vi_mode {
        config_builder.set_edit_mode(EditMode::Vi);
    }
    let mut rl = Editor::<()>::with_config(config_builder.build());

    let history_path = match ProjectDirs::from("com", "flick-lang", "flick") {
        Some(proj_dir) => {
            let dir = proj_dir.cache_dir();
            match fs::create_dir_all(dir) {
                Ok(_) => Some(dir.join("flick-history.txt")),
                Err(_) => None,
            }
        }
        None => None,
    };
    let history_path = history_path.unwrap_or_else(|| PathBuf::from("~/.flick-history"));

    let _ = rl.load_history(&history_path);

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let sf = SourceFile::new("REPL", line);
                match cli.mode {
                    Mode::Tokenize => print_tokens(sf.tokens()),
                    mode => unimplemented!("{:?}", mode),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("^D");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
        println!();
    }

    rl.save_history(&history_path).unwrap();
}

fn print_tokens(tokens: Tokens) {
    let (tokens, errors): (Vec<_>, Vec<_>) = tokens.partition(|x| x.is_ok());
    if !errors.is_empty() {
        let err_str = errors.into_iter()
            .map(|err| err.unwrap_err().to_string())
            .collect::<Vec<_>>()
            .join("\n\n");

        eprintln!("{}", err_str);
    } else {
        for token in tokens {
            println!("{:?}", token.unwrap());
        }
    }
}

fn main() {
    let cli = Cli::parse();

    if cli.file.is_some() {
        handle_run(cli)
    } else {
        handle_repl(cli)
    }
}
