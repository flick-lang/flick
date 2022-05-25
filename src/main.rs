#[warn(
    missing_debug_implementations,
    missing_copy_implementations,
    unused_import_braces,
    unused_lifetimes
)]
use crate::program::Program;

mod program;
mod token;

fn main() {
    let program = Program::from_file("examples/comments.fl");
    for token in program.tokens() {
        println!("{:?}", token);
    }
}
