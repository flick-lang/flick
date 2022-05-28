use colored::Colorize;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq)]
pub struct Error {
    pub(crate) kind: ErrorKind,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self { kind }
    }
}
impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", "error".red(), self.kind)
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    BadUnicodeEscape(String),
    InvalidCharInEscape(char),
    InvalidFloat(String),
    TruncatedEscapeSequence,
    UnknownEscape(char),
    UnknownStartOfToken(char),
    UnterminatedStr,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BadUnicodeEscape(s) => {
                write!(f, "bad unicode escape sequence '{}'", s)
            }
            Self::InvalidCharInEscape(c) => {
                write!(f, "invalid character in escape sequence: '{}'", c)
            }
            Self::InvalidFloat(float) => write!(f, "invalid float literal '{}'", float),
            Self::TruncatedEscapeSequence => write!(f, "escape sequence is too short"),
            Self::UnknownEscape(c) => write!(f, "unknown escape '{}'", c),
            Self::UnknownStartOfToken(c) => {
                write!(f, "unknown start of token (U+{:0>4X})", *c as u32)
            }
            Self::UnterminatedStr => write!(f, "unterminated string literal"),
        }
    }
}

impl std::error::Error for ErrorKind {}
