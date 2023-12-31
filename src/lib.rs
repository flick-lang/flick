#![doc = include_str!("../README.md")]

/// Module to convert [abstract syntax trees](parser::ast) into LLVM using llvm-sys
///
/// The general idea is to take code objects (e.g. variables or functions) and form
/// instances of [LLVMValueRef][a]. Then, after we're done, we ask llvm-sys to
/// generate LLVM code.
///
/// [a]: llvm_sys::prelude::LLVMValueRef
mod compiler;
/// Module to convert source files into token streams
mod lexer;
/// Module to convert token streams into [abstract syntax trees](parser::ast)
mod parser;

pub use compiler::Compiler;
pub use lexer::Lexer;
pub use parser::ast;
pub use parser::Parser;
