use crate::program::Program;

mod token;
mod program;

fn main() {
    let program = Program::new("call(3)\nprint(5)");
    for token in program.tokens() {
        println!("{:?}", token);
    }
}
