use crate::ast::{BinaryOperator, ComparisonOperator, FuncProto};
use crate::types::{FuncType, IntType};
use crate::Type;

/// A typed version of [Program](crate::ast::Program)
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedProgram {
    pub global_statements: Vec<TypedGlobalStatement>,
}

/// A typed version of [GlobalStatement](crate::ast::GlobalStatement)
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypedGlobalStatement {
    Extern(FuncProto),
    FuncDef(TypedFuncDef)
}

/// A typed version of [crate::ast::FuncDef]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedFuncDef {
    pub proto: FuncProto,
    pub body: Vec<TypedStatement>,
}

/// A function parameter (its name and its data type).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedFuncParam {
    pub param_type: Type,
    pub param_name: String,
}

/// A typed version of [Statement](crate::ast::Statement).
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypedStatement {
    VarDeclaration(TypedVarDeclaration),
    WhileLoop(TypedWhileLoop),
    Assignment(TypedAssignment),
    Return(Option<TypedExpr>),
    Call(TypedCall),
    If(TypedIf),
}

/// A typed version of [VarDeclaration](crate::ast::VarDeclaration)
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedVarDeclaration {
    pub var_name: String,
    pub var_type: Type,
    pub var_value: TypedExpr,
}

/// A typed version of [WhileLoop](crate::ast::WhileLoop).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedWhileLoop {
    pub condition: TypedExpr,
    pub body: Vec<TypedStatement>,
}

/// A typed version of [If](crate::ast::If).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedIf {
    pub condition: TypedExpr,
    pub then_body: Vec<TypedStatement>,
    pub else_body: Option<Vec<TypedStatement>>,
}

/// A typed version of [Expr](crate::ast::Expr)
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypedExpr {
    Identifier(TypedIdentifier),
    IntLiteral(TypedIntLiteral),
    Binary(TypedBinary),
    Comparison(TypedComparison),
    Call(TypedCall),
}

/// A typed version of [Assignment](crate::ast::Assignment).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedAssignment {
    pub name: String,
    pub value: Box<TypedExpr>,
}

/// A typed version of [Binary](crate::ast::Binary).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedBinary {
    pub left: Box<TypedExpr>,
    pub operator: BinaryOperator,
    pub right: Box<TypedExpr>,
    pub result_type: Type,
}

/// A typed version of [IntLiteral](crate::ast::Expr::IntLiteral)
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedIntLiteral {
    pub negative: bool,
    pub int_value: String,
    pub int_type: IntType,
}

/// A comparison expression (the operator and the left/right-hand sides).
///
/// For example, `a < foo(1)` breaks down into:
/// - left: `a`
/// - operator: `<`
/// - right: `foo(1)`
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedComparison {
    pub left: Box<TypedExpr>,
    pub operator: ComparisonOperator,
    pub right: Box<TypedExpr>,
    pub result_type: Type,
}

/// A call expression (the name of the function to call and the arguments to pass).
///
/// For example, `foo(a, 12 - b, "test")` is a call expression with 3 args.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedCall {
    pub function_name: String,
    pub function_type: FuncType,
    pub args: Vec<TypedExpr>,
}

/// An identifier, like `x` or `cur_count`, along with its type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedIdentifier {
    pub name: String,
    pub id_type: Type,
}
