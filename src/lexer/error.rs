use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownChar(char),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownChar(c) => write!(f, "unknown char '{}'", c),
        }
    }
}

impl std::error::Error for Error {}
