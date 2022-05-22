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
    Int,
    Float,
    Str,
    Bool,
    Map,
    Arr,
    Set,

    Or,
    And,
    Not,
    True,
    False,

    If,
    Fn,
    While,
    For,
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
    Dash,
    Dollar,
    Dot,
    DoubleQuote,
    Equal,
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