mod error;
mod token;
mod tokens;

pub use token::Bracket;
pub use token::Comment;
pub use token::Keyword;
pub use token::Literal;
pub use token::Punctuation;
pub use token::Token;

pub use tokens::Tokens;

pub use error::Error;
pub type Result<T> = std::result::Result<T, Error>;
