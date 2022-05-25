#[derive(Debug, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Literal(Literal),
    Punctuation(Punctuation),
    Comment(Comment)
}

#[derive(Debug, PartialEq)]
pub enum Comment {
    Regular(String),
    Docstring(String)
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Arr,
    Bool,
    Float,
    Int,
    Map,
    Set,
    Str,

    And,
    False,
    Not,
    Or,
    True,

    Fn,
    For,
    If,
    While,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Float(f64),
    Int(usize),
    Str(String),
}

#[derive(Debug, PartialEq)]
pub enum Punctuation {
    Ampersand,
    Asterisk,
    At,
    Backslash,
    Caret,
    Colon,
    Comma,
    Dash,
    Dollar,
    Dot,
    DoubleQuote,
    Equals,
    Exclamation,
    Hashtag,
    Newline,
    Percent,
    Pipe,
    Plus,
    Question,
    SingleQuote,
    Slash,
    Tilde,

    OpenBracket(Bracket),
    CloseBracket(Bracket),
}

#[derive(Debug, PartialEq)]
pub enum Bracket {
    Angle,
    Curly,
    Round,
    Square,
}