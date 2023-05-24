#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Comment(String),

    Usize(usize),
    Identifier(String),

    // Keywords
    Var,
    While,

    // Types
    Int,

    // Brackets
    LSquirly,
    RSquirly,
    LParen,
    RParen,

    // Punctuation
    Newline,
    Colon,
    Assign,

    // Comparators
    EqualTo,
    LessThan,
    GreaterThan,
    LessOrEqualTo,
    GreaterOrEqualTo,

    // Operators
    Plus,
    Minus,
    Asterisk,
    Slash,

    // Assigners
    PlusEq,
    MinusEq,
    TimesEq,
    DivideEq,
}
