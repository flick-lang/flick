use std::fmt;
use std::error::Error;

use crate::lexing::error::LexingError;

pub type Result<T> = std::result::Result<T, FlickError>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FlickError {
    pub index: usize,
    pub kind: ErrorKind,
}

impl fmt::Display for FlickError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Error for FlickError {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorKind {
    LexingError(LexingError),
    // ParsingError(ParsingError),
    // TypingError(TypingError),
    // CompilationError(CompilationError),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LexingError(err) => err.fmt(f),
        }
    }
}
