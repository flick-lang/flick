#[derive(Debug)]
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
    EQ,
    LEQ,
    GEQ,
    LT,
    GT,

    // Operators
    Asterisk,
    Slash,
    Minus,
    Plus,

    // Assigners
    TimesEq,
    DivideEq,
    MinusEq,
    PlusEq,
}
