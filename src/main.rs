#[warn(
    missing_debug_implementations,
    missing_copy_implementations,
    unused_import_braces,
    unused_lifetimes
)]
use crate::program::Program;
use owo_colors::{OwoColorize, Stream::Stderr};
use std::error::Error;

mod lexer;
mod program;

fn main() {
    let program = Program::from_file("examples/bad.fl");
    for token in program.tokens() {
        match token {
            Ok(t) => println!("{:?}", t), // should panic at ≈
            Err(e) => handle_error(e),
        }
    }
}

fn handle_error(err: impl Error) {
    // TODO: Print line and column numbers of error
    eprintln!(
        "{} {}",
        "Error: ".if_supports_color(Stderr, |text| text.red()),
        err
    );

    // NOTE: These APIs aren't stable yet, but they should be used once they are
    // let boxed_err: Box<dyn Error> = err.into();
    // for e in boxed_err.chain() {
    //     eprintln!("caused by: {}", e);
    // }

    // // The backtrace is not always generated
    // // Run with 'RUST_BACKTRACE=1'
    // if let Some(backtrace) = err.backtrace() {
    //     eprintln!("backtrace: {:?}", backtrace);
    // }

    std::process::exit(1);
}
