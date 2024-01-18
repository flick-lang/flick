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
/// This module is used by [Compiler](compiler::Compiler), because it manages namespaces/scopes.
///
/// See [ScopeManager] for more details.
mod scope_manager;
/// Module to store all the Flick types
mod types;
/// Module to add types to [abstract syntax trees](ast)
mod typing;

// TODO (Max): Should we remove pub use and just make users use absolute path (I kinda like the idea of that if we somehow make the paths nicer)
pub use compilation::compiler::Compiler;
pub use lexing::lexer::Lexer;
pub use lexing::token;
pub use parsing::ast;
pub use parsing::parser::Parser;
pub use scope_manager::ScopeManager;
pub use types::Type;
pub use typing::typed_ast;
pub use typing::typer::Typer;
