#[warn(missing_debug_implementations, missing_copy_implementations, unused_import_braces, unused_lifetimes)]

use crate::program::Program;

mod token;
mod program;

fn main() {
    let program = Program::from_file("examples/floats.fl");
    for token in program.tokens() {
        println!("{:?}", token);
    }
}
