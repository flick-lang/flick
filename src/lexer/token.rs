use std::fmt;

/// An enum to represent any given non-whitespace token in a source code
///
/// For example, `foo(42)` consists of four tokens:
/// 1. `Token::Identifier("print")`
/// 1. `Token::LParen`
/// 1. `Token::I64Literal(42)`
/// 1. `Token::RParen`
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Docstring(String),
    Comment(String),

    I64Literal(i64),
    Identifier(String),

    // Keywords
    Pub,
    Fn,
    While,
    Ret,

    /// The built-in Flick types, like `void`
    Type(Type),

    // Brackets
    LSquirly,
    RSquirly,
    LParen,
    RParen,

    // Punctuation
    Newline,
    Semicolon,
    Colon,
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

            Self::I64Literal(int) => write!(f, "{}", int),
            Self::Identifier(id) => write!(f, "{}", id),

            Self::Pub => write!(f, "pub"),
            Self::Fn => write!(f, "fn"),
            Self::While => write!(f, "while"),
            Self::Ret => write!(f, "ret"),

            Self::Type(var_type) => write!(f, "{}", var_type),

            Self::LSquirly => write!(f, "{{"),
            Self::RSquirly => write!(f, "}}"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Newline => writeln!(f),
            Self::Semicolon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),

            Self::OperatorSymbol(operator_symbol) => write!(f, "{}", operator_symbol),
            Self::ComparatorSymbol(comparator_symbol) => write!(f, "{}", comparator_symbol),
            Self::AssignmentSymbol(assignment_symbol) => write!(f, "{}", assignment_symbol),
        }
    }
}

/// An enum to store the built-in Flick types, like `void`
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

/// An enum to store one of `+`, `-`, `*`, and `/`
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum OperatorSymbol {
    Plus,
    Minus,
    Asterisk,
    Slash,
}

impl fmt::Display for OperatorSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
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
    LessThanOrEqualTo,
    GreaterThanOrEqualTo,
}

impl fmt::Display for ComparatorSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotEqualTo => write!(f, "!="),
            Self::EqualTo => write!(f, "=="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThanOrEqualTo => write!(f, "<="),
            Self::GreaterThanOrEqualTo => write!(f, ">="),
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
