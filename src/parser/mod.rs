/// Module that defines the components of Flick's abstract syntax tree (AST).
///
/// The top-level node in Flick's AST is [ast::Program], which stores a collection of function
/// definitions ([ast::FuncDef]). Each function's body is a collection of [statements](ast::Statement),
/// which further break down into [expressions](ast::Expr), which break down further into
/// components.
///
/// For example, `9 * (2 + 3);` corresponds to the following expression:
///
/// ```
/// # use flick::ast::*;
/// # let _ =
/// Statement::Expr(Expr::Binary(
///     Binary {
///         left: Box::new(Expr::I64Literal(9)),
///         operator: BinaryOperator::Multiply,
///         right: Box::new(Expr::Binary(
///             Binary {
///                 left: Box::new(Expr::I64Literal(2)),
///                 operator: BinaryOperator::Add,
///                 right: Box::new(Expr::I64Literal(3)),
///             }
///         )),
///     }
/// ));
/// ```
pub mod ast;
#[allow(clippy::module_inception)]
/// Module that defines the [Parser] struct for converting tokens to an abstract syntax tree.
mod parser;

pub use parser::Parser;
