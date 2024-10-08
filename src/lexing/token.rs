use crate::types::Type;
use std::fmt;

/// An enum to represent any given non-whitespace token in a source code
///
/// For example, `foo(42)` consists of four tokens:
/// 1. `Token::Identifier("foo".to_string())`
/// 1. `Token::LParen`
/// 1. `Token::IntLiteral("42".to_string())`
/// 1. `Token::RParen`
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Docstring(String),
    Comment(String),

    IntLiteral(String),
    Identifier(String),

    // Keywords
    Extern,
    Pub,
    Fn,
    Ret,
    While,
    If,
    Else,
    True,
    False,

    /// The built-in Flick types, like `void`
    Type(Type),

    // Brackets
    LSquirly,
    RSquirly,
    LParen,
    RParen,

    // Punctuation
    Newline,
    Comma,

    /// One of `+`, `-`, `*`, and `/`
    OperatorSymbol(OperatorSymbol),
    /// One of `>`, `<`, `<=`, `>=`, `==`, and `!=`
    ComparatorSymbol(ComparatorSymbol),
    /// One of `+=`, `-=`, `*=`, `/=`, and `=`
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

            Self::IntLiteral(int) => write!(f, "{}", int),
            Self::Identifier(id) => write!(f, "{}", id),

            Self::Pub => write!(f, "pub"),
            Self::Fn => write!(f, "fn"),
            Self::Extern => write!(f, "extern"),
            Self::While => write!(f, "while"),
            Self::Ret => write!(f, "ret"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),

            Self::Type(var_type) => write!(f, "{}", var_type),

            Self::LSquirly => write!(f, "{{"),
            Self::RSquirly => write!(f, "}}"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Newline => writeln!(f),
            Self::Comma => write!(f, ","),

            Self::OperatorSymbol(operator_symbol) => write!(f, "{}", operator_symbol),
            Self::ComparatorSymbol(comparator_symbol) => write!(f, "{}", comparator_symbol),
            Self::AssignmentSymbol(assignment_symbol) => write!(f, "{}", assignment_symbol),
        }
    }
}

/// An enum to store one of `+`, `-`, `*`, and `/`
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum OperatorSymbol {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Modulo,
}

impl fmt::Display for OperatorSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
        }
    }
}

/// An enum to store one of `>`, `<`, `<=`, `>=`, `==`, and `!=`
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ComparatorSymbol {
    NotEqualTo,
    EqualTo,
    LessThan,
    GreaterThan,
    LessOrEqualTo,
    GreaterOrEqualTo,
}

impl fmt::Display for ComparatorSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotEqualTo => write!(f, "!="),
            Self::EqualTo => write!(f, "=="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessOrEqualTo => write!(f, "<="),
            Self::GreaterOrEqualTo => write!(f, ">="),
        }
    }
}

/// An enum to store one of `+=`, `-=`, `*=`, `/=`, and `=`
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AssignmentSymbol {
    PlusEq,
    MinusEq,
    TimesEq,
    DivideEq,
    Eq,
}

impl fmt::Display for AssignmentSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PlusEq => write!(f, "+="),
            Self::MinusEq => write!(f, "-="),
            Self::TimesEq => write!(f, "*="),
            Self::DivideEq => write!(f, "/="),
            Self::Eq => write!(f, "="),
        }
    }
}
