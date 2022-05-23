#[warn(missing_debug_implementations, missing_copy_implementations, unused_import_braces, unused_lifetimes)]

use crate::program::Program;

mod token;
mod program;

fn main() {
    let program = Program::new("call(3)\nprint(5)\nx = \"before_quote\\\"end_q\\u00e9uote\"\ny = \"hex newline \\x0a\"");
    for token in program.tokens() {
        println!("{:?}", token);
    }
}
