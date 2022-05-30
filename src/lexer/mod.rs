pub use error::Error;
pub use error::ErrorKind;
pub use token::Bracket;
pub use token::Comment;
pub use token::Keyword;
pub use token::Literal;
pub use token::Punctuation;
pub use token::Token;
pub use tokens::Tokens;

mod error;
mod location;
mod source_iterator;
mod token;
mod tokens;

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
