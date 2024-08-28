use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LexingError {
    UnexpectedCharacter(char),
}

impl fmt::Display for LexingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedCharacter(c) => write!(f, "unexpected character: '{}'", c),
        }
    }
}
