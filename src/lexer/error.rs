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
    IncompleteEscape,
    UnknownChar(char),
    UnknownEscape(char),
    UnterminatedStrLiteral,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FloatParsing(float) => write!(f, "invalid float literal '{}'", float),
            Self::IncompleteEscape => write!(f, "incomplete escape"),
            Self::UnknownChar(c) => write!(f, "unknown char '{}' (U+{:0>4X})", c, *c as u32),
            Self::UnknownEscape(c) => write!(f, "unknown escape '{}'", c),
            Self::UnterminatedStrLiteral => write!(f, "unterminated string literal"),
        }
    }
}

impl std::error::Error for ErrorKind {}
