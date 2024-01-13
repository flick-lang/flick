/// This module defines the [Compiler] struct, which converts [abstract syntax trees][a]
/// into LLVM code.
///
/// [a]: crate::parser::ast;
pub mod compiler;

/// This module is used by [Compiler](compiler::Compiler), because it manages namespaces/scopes.
///
/// See [scope_manager::ScopeManager] for more details.
pub mod scope_manager;
