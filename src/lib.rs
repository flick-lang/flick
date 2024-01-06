#![doc = include_str!("../README.md")]

/// Module to convert [abstract syntax trees](ast) into LLVM using llvm-sys
///
/// The general idea is to take code objects (e.g. variables or functions) and form
/// instances of [LLVMValueRef][a]. Then, after we're done, we ask llvm-sys to
/// generate LLVM code.
///
/// [a]: llvm_sys::prelude::LLVMValueRef
mod compilation;
/// Module to convert source files into token streams
mod lexing;
/// Module to convert token streams into [abstract syntax trees](ast)
mod parsing;

pub use compilation::compiler::Compiler;
pub use compilation::scope_manager;
pub use lexing::lexer::Lexer;
pub use lexing::token;
pub use parsing::ast;
pub use parsing::parser::Parser;
