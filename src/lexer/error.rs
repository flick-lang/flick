use colored::Colorize;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq)]
pub struct Error {
    kind: ErrorKind,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self { kind }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", "error".red(), self.kind)
    }
}

impl std::error::Error for Error {}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    FloatParsing(String),
    UnknownChar(char),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FloatParsing(float) => write!(f, "invalid float literal '{}'", float),
            Self::UnknownChar(c) => write!(f, "unknown char '{}' (U+{:x})", c, *c as u32),
        }
    }
}

impl std::error::Error for ErrorKind {}
