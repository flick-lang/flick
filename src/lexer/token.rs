use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Docstring(String),
    Comment(String),

    I64Literal(i64),
    Identifier(String),

    // Keywords
    Fn,
    While,
    Ret,

    // Types
    Type(Type),

    // Brackets
    LSquirly,
    RSquirly,
    LParen,
    RParen,
    // LSquare,
    // RSquare,

    // Punctuation
    Newline,
    Semicolon,
    Colon,
    Comma,

    OperatorSymbol(OperatorSymbol),
    AssignmentSymbol(AssignmentSymbol),
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

            Self::I64Literal(int) => write!(f, "{}", int),
            Self::Identifier(id) => write!(f, "{}", id),

            Self::Fn => write!(f, "fn"),
            Self::While => write!(f, "while"),
            Self::Ret => write!(f, "ret"),

            Self::Type(var_type) => write!(f, "{}", var_type),

            Self::LSquirly => write!(f, "{{"),
            Self::RSquirly => write!(f, "}}"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            // Self::LSquare => write!(f, "["),
            // Self::RSquare => write!(f, "]"),
            Self::Newline => writeln!(f),
            Self::Semicolon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),

            Self::OperatorSymbol(op_symbol) => write!(f, "{}", op_symbol),
            Self::AssignmentSymbol(assignment_symbol) => write!(f, "{}", assignment_symbol),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Type {
    I64,
    Void,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::I64 => write!(f, "int"),
            Self::Void => write!(f, "void"),
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
    LessThanOrEqualTo,
    GreaterThanOrEqualTo,

    // Operators
    Plus,
    Minus,
    Asterisk,
    Slash,
}

impl fmt::Display for OperatorSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotEqualTo => write!(f, "!="),
            Self::EqualTo => write!(f, "=="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThanOrEqualTo => write!(f, "<="),
            Self::GreaterThanOrEqualTo => write!(f, ">="),

            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AssignmentSymbol {
    PlusEq,
    MinusEq,
    TimesEq,
    DivideEq,
    Eq,
}

impl fmt::Display for AssignmentSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::PlusEq => write!(f, "+="),
            Self::MinusEq => write!(f, "-="),
            Self::TimesEq => write!(f, "*="),
            Self::DivideEq => write!(f, "/="),
            Self::Eq => write!(f, "="),
        }
    }
}
