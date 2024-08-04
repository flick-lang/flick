/// Module that adds types to Flick's [abstract syntax tree (AST)](crate::ast).
///
/// The top-level node in Flick's typed AST is [typed_ast::TypedProgram], which stores a collection of function
/// definitions ([typed_ast::TypedFuncDef]). Each function's body is a collection of [statements](typed_ast::TypedStatement),
/// which further break down into [expressions](typed_ast::TypedExpr), which break down further into
/// components.
///
/// For example, `9 * (2 + 3);` corresponds to the following expression:
///
/// ```ignore
/// use flick::ast::BinaryOperator;
/// use flick::typed_ast::*;
/// let _ = TypedExpr::Binary(TypedBinary {
///     left: Box::new(TypedExpr::IntLiteral(TypedIntLiteral { int_value: "9".to_string(), int_type: Type::Int(IntType { width: 32 })})),
///     operator: BinaryOperator::Multiply,
///     right: Box::new(TypedExpr::Binary(TypedBinary {
///         left: Box::new(TypedExpr::IntLiteral(TypedIntLiteral { int_value: "2".to_string(), int_type: Type::Int(IntType { width: 32 })})),
///         operator: BinaryOperator::Add,
///         right: Box::new(TypedExpr::IntLiteral(TypedIntLiteral { int_value: "3".to_string(), int_type: Type::Int(IntType { width: 32 })})),
///     })),
/// });
/// ```
pub mod typed_ast;
/// Module that defines the [Typer] struct for adding types to an abstract syntax tree.
pub mod typer;
