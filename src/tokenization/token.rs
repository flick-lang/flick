pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Literal(Literal),
    Punctuation(Punctuation),
}

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

pub enum Literal {
    Float(f64),
    Int(isize),
    Str(String),
}

pub enum Punctuation {
    Ampersand,
    Asterisk,
    At,
    Caret,
    Dash,
    Dollar,
    Dot,
    DoubleQuote,
    Exclamation,
    Hashtag,
    Percent,
    Pipe,
    Plus,
    Question,
    SingleQuote,
    Tilde,

    OpenBracket(Bracket),
    CloseBracket(Bracket),

    Unknown(char),
}

pub enum Bracket {
    Round,
    Square,
    Curly,
    Angle,
}