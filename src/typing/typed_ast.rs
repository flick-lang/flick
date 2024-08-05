use crate::ast::{BinaryOperator, ComparisonOperator, FuncProto};
use crate::types::{FuncType, IntType};
use crate::Type;

/// A program; a collection of function definitions. See also: [TypedFuncDef].
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedProgram {
    pub func_defs: Vec<TypedFuncDef>,
}

/// A function definition (metadata, prototype, and body).
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

/// A statement (the equivalent of 'a line of code').
///
/// Statements do not evaluate to any particular value, but they have side effects. For
/// example, consider the following code:
///
/// ```text
/// i64 i = 0  // this is a statement
/// if should_change_i() (
///     i = 1  // this is a statement
/// }
/// ```
///
/// Each line is a statement. Note, `i = 1` is also technically an expression that
/// evaluates to `1`. However, since we are not using the value of `i = 1` when we write
/// `i = 1`, our code becomes a statement.
///
/// See also: [TypedExpr].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypedStatement {
    VarDeclaration(TypedVarDeclaration),
    WhileLoop(TypedWhileLoop),
    Assignment(TypedAssignment),
    Return(Option<TypedExpr>),
}

/// A variable declaration and, optionally, variable definition as well.
///
/// This struct stores the name and type of the declared variable, as well as its
/// value (if the variable declaration comes with an assignment, like `int a = 7;` instead of
/// `int a;`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedVarDeclaration {
    pub var_name: String,
    pub var_type: Type,
    pub var_value: TypedExpr,
}

/// A while loop (its 'while condition' and its body).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedWhileLoop {
    pub condition: TypedExpr,
    pub body: Vec<TypedStatement>,
}

/// An expression, which is any piece of code that has a value.
///
/// For example, `current_length` or `1 + 2` or `foo("bye")` are expressions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypedExpr {
    Identifier(TypedIdentifier),
    IntLiteral(TypedIntLiteral),
    Binary(TypedBinary),
    Comparison(TypedComparison),
    Call(TypedCall),
}

/// An assignment expression (the variable name and the new value).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedAssignment {
    pub name: String,
    pub value: Box<TypedExpr>,
}

/// A binary expression (the operator and the left/right-hand sides).
///
/// For example, `a + foo(1)` breaks down into:
/// - left: `a`
/// - operator: `+`
/// - right: `foo(1)`
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedBinary {
    pub left: Box<TypedExpr>,
    pub operator: BinaryOperator,
    pub right: Box<TypedExpr>,
}

/// An integer literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedIntLiteral {
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
