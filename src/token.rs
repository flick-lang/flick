// todo: impl  Display or Debug or whatever

use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Docstring(String),
    Comment(String),

    IntLiteral(i64),
    Identifier(String),

    // Keywords
    Fn,
    While,

    // Types
    VarType(VarType),

    // Brackets
    LSquirly,
    RSquirly,
    LParen,
    RParen,
    LSquare,
    RSquare,

    // Punctuation
    Newline,
    Colon,
    Comma,

    OperatorSymbol(OperatorSymbol),
}

impl Token {
    pub fn get_char_count(&self) -> usize {
        self.to_string().len()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Docstring(docstring) => write!(f, "{}", docstring),
            Self::Comment(comment) => write!(f, "{}", comment),
            Self::IntLiteral(int) => write!(f, "{}", int),
            Self::Identifier(id) => write!(f, "{}", id),
            Self::Fn => write!(f, "fn"),
            Self::While => write!(f, "while"),
            Self::VarType(var_type) => write!(f, "{}", var_type),
            Self::LSquirly => write!(f, "{{"),
            Self::RSquirly => write!(f, "}}"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LSquare => write!(f, "["),
            Self::RSquare => write!(f, "]"),
            Self::Newline => writeln!(f),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::OperatorSymbol(op_symbol) => write!(f, "{}", op_symbol),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum OperatorSymbol {
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
    Assign,
}

impl fmt::Display for OperatorSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotEqualTo => write!(f, "!="),
            Self::EqualTo => write!(f, "=="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessOrEqualTo => write!(f, "<="),
            Self::GreaterOrEqualTo => write!(f, ">="),

            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),

            Self::PlusEq => write!(f, "+="),
            Self::MinusEq => write!(f, "-="),
            Self::TimesEq => write!(f, "*="),
            Self::DivideEq => write!(f, "/="),
            Self::Assign => write!(f, "="),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum VarType {
    Int,
}

impl fmt::Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
        }
    }
}
