use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LexingError {
    UnexpectedCharacter(char),
}

impl fmt::Display for LexingError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}
