use error::Error;
use error::ErrorKind;
use token::Bracket;
use token::Comment;
use token::Keyword;
use token::Literal;
use token::Punctuation;
use token::Token;

pub use tokens::Tokens;

mod error;
mod location;
mod source_iterator;
mod token;
mod tokens;

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
