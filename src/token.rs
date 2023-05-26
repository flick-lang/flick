#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Token {
    Comment(String),

    Int(isize),
    Identifier(String),

    // Keywords
    Var,
    While,

    // Types
    VarType(VarType),

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
    NotEqualTo,
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum VarType {
    Int
}
