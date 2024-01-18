/// Module that adds types to Flick's [typed abstract syntax tree (typed AST)](typed_ast).
//
// The top-level node in Flick's AST is [ast::Program], which stores a collection of function
// definitions ([ast::FuncDef]). Each function's body is a collection of [statements](ast::Statement),
// which further break down into [expressions](ast::Expr), which break down further into
// components.
//
// For example, `9 * (2 + 3);` corresponds to the following expression:
//
// ```
// # use flick::typed_ast::*;
// # let _ =
// Expr::Binary(
//     Binary {
//         left: Box::new(Expr::I64Literal(9)),
//         operator: BinaryOperator::Multiply,
//         right: Box::new(Expr::Binary(
//             Binary {
//                 left: Box::new(Expr::I64Literal(2)),
//                 operator: BinaryOperator::Add,
//                 right: Box::new(Expr::I64Literal(3)),
//             }
//         )),
//     }
// );
// ```
pub mod typed_ast;
/// Module that defines the [Typer] struct for adding types to an abstract syntax tree.
pub mod typer;
