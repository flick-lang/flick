pub enum Token {
    EOF,

    // commands
    Func,
    Import,

    // primary
    Identifier(String),
    Number(f64)
}