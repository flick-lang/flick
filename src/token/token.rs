#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Keyword(Keyword),
    Identifier(&'a str),
    Literal(Literal<'a>),
    Punctuation(Punctuation),
    Unknown(char),
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
pub enum Literal<'a> {
    Float(f64),
    Int(usize),
    Str(&'a str),
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