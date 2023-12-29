#[allow(clippy::module_inception)]

/// Module that defines the [Lexer](lexer::Lexer) struct for converting code to tokens.
mod lexer;

/// Module that defines the [Token](token::Token) enum.
pub mod token;

pub use lexer::Lexer;
