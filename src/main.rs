use flick::Program;

fn main() {
    let program = Program::from_file("examples/bad_strs.fl").expect("error reading file");
    for token in program.tokens() {
        match token {
            Ok(t) => println!("{:?}", t),
            Err(e) => eprintln!("{}", e),
        }
    }
}
