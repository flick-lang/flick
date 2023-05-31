pub mod ast;
#[allow(clippy::module_inception)]
mod parser;

pub use parser::Parser;
