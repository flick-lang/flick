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
/// Expr::Binary(
///     Binary {
///         left: Box::new(Expr::IntLiteral("9".to_string())),
///         operator: BinaryOperator::Multiply,
///         right: Box::new(Expr::Binary(
///             Binary {
///                 left: Box::new(Expr::IntLiteral("2".to_string())),
///                 operator: BinaryOperator::Add,
///                 right: Box::new(Expr::IntLiteral("3".to_string())),
///             }
///         )),
///     }
/// );
/// ```
pub mod ast;
/// Module that defines the [Parser] struct for converting tokens to an abstract syntax tree.
pub mod parser;
