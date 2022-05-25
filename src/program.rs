use crate::lexer::Tokens;
use std::fs::File;
use std::io::Read;
use std::path::Path;

pub struct Program {
    source: String,
}

impl Program {
    pub fn new(source: &str) -> Self {
        Self {
            source: String::from(source),
        }
    }

    pub fn from_file(path: impl AsRef<Path>) -> Self {
        let mut source = String::new();
        File::open(path)
            .unwrap()
            .read_to_string(&mut source)
            .unwrap(); // todo error check
        Self { source }
    }

    pub fn tokens(&self) -> Tokens {
        Tokens {
            unparsed: &self.source,
        }
    }
}
