use std::fs::File;
use std::io;
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::lexer::Tokens;

pub struct SourceFile {
    file_path: PathBuf,
    source: String,
}

impl SourceFile {
    pub fn new(file_path: impl AsRef<Path>, source: &str) -> Self {
        Self {
            source: source.to_string(),
            file_path: file_path.as_ref().into(),
        }
    }

    pub fn from_file(file_path: impl AsRef<Path>) -> io::Result<Self> {
        let mut source = String::new();
        File::open(file_path.as_ref())?.read_to_string(&mut source)?;
        Ok(Self {
            source,
            file_path: file_path.as_ref().into(),
        })
    }

    pub fn tokens(&self) -> Tokens {
        Tokens::new(&self.file_path, &self.source)
    }
}
