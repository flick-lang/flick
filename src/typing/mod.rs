/// Module that adds types to Flick's [abstract syntax tree (AST)](crate::ast).
/// Look at [typer::Typer] to see how abstract syntax trees are typed.
///
/// The top-level node in Flick's typed AST is [typed_ast::TypedProgram], which stores a collection
/// of function definitions ([typed_ast::TypedFuncDef]). Each function's body is a collection of
/// [statements](typed_ast::TypedStatement), which further break down into
/// [expressions](typed_ast::TypedExpr), which break down further into components.
///
///
/// For example, the typed version of `9 * (2 + 3)` corresponds to the following expression:
///
/// ```
/// use flick::ast::BinaryOperator;
/// use flick::typed_ast::*;
/// use flick::types::{Type, IntType};
/// let _ = TypedExpr::Binary(TypedBinary {
///     left: Box::new(TypedExpr::IntLiteral(TypedIntLiteral { int_value: "9".to_string(), int_type: IntType { signed: false, width: 64 }})),
///     operator: BinaryOperator::Multiply,
///     right: Box::new(TypedExpr::Binary(TypedBinary {
///         left: Box::new(TypedExpr::IntLiteral(TypedIntLiteral { int_value: "2".to_string(), int_type: IntType { signed: false, width: 64 }})),
///         operator: BinaryOperator::Add,
///         right: Box::new(TypedExpr::IntLiteral(TypedIntLiteral { int_value: "3".to_string(), int_type: IntType { signed: false, width: 64 }})),
///         result_type: Type::Int(IntType { signed: false, width: 64 }),
///     })),
///     result_type: Type::Int(IntType { signed: false, width: 64 }),
/// });
/// ```
pub mod typed_ast;
/// Module that defines the [typer::Typer] struct for adding types to an abstract syntax tree.
pub mod typer;
