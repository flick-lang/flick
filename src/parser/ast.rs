use crate::lexer::token::ComparatorSymbol::*;
use crate::lexer::token::OperatorSymbol::*;
use crate::lexer::token::{ComparatorSymbol, OperatorSymbol, Type};

/// A program; a collection of function definitions. See also: [FuncDef].
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub func_defs: Vec<FuncDef>,
}

/// A function definition (metadata, prototype, and body).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncDef {
    pub is_public: bool,
    pub proto: FuncProto,
    pub body: Vec<Statement>,
}

/// A function prototype (name, parameters, and return type).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncProto {
    pub name: String,
    pub params: Vec<FuncParam>,
    pub return_type: Type,
}

/// A function parameter (its name and its data type).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncParam {
    pub param_type: Type,
    pub param_name: String,
}

/// A statement (the equivalent of 'a line of code').
///
/// Statements do not evaluate to any particular value, but they have side effects. For
/// example, `i += 1;` is a statement. Note, `i += 1` is technically an expression, which
/// evaluates to `i + 1`. However, since we are not using the value of `i += 1` when we write
/// `i += 1;`, our code becomes a statement.
///
/// See also: [Expr].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    VarDeclarations(Vec<VarDeclaration>),
    WhileLoop(WhileLoop),
    Expr(Expr),
    Return(Option<Expr>),
}

/// A variable declaration and, optionally, variable definition as well.
///
/// This struct stores the name and type of the declared variable, as well as its
/// value (if the variable declaration comes with an assignment, like `int a = 7;` instead of
/// `int a;`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDeclaration {
    pub var_name: String,
    pub var_type: Type,
    pub var_value: Option<Expr>,
}

/// A while loop (its 'while condition' and its body).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct WhileLoop {
    pub condition: Expr,
    pub body: Vec<Statement>,
}

/// An expression, which is any piece of code that has a value.
///
/// For example, `current_length` or `1 + 2` or `foo("bye")` are expressions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Identifier(String),
    I64Literal(i64),
    Assign(Assign),
    Binary(Binary),
    Call(Call),
}

/// An assignment expression (the variable name and the new value).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assign {
    pub name: String,
    pub value: Box<Expr>,
}

/// A binary expression (the operator and the left/right-hand sides).
///
/// For example, `a + foo(1)` breaks down into:
/// - left: `a`
/// - operator: `+`
/// - right: `foo(1)`
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub right: Box<Expr>,
}

/// A call expression (the name of the function to call and the arguments to pass).
///
/// For example, `foo(a, 12 - b, "test")` is a call expression with 3 args.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    pub function_name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,

    NotEqualTo,
    EqualTo,
    LessThan,
    GreaterThan,
    LessOrEqualTo,
    GreaterOrEqualTo,
    // LogicalAnd,
    // LogicalOr,
}

impl From<OperatorSymbol> for BinaryOperator {
    fn from(operator: OperatorSymbol) -> Self {
        match operator {
            Plus => Self::Add,
            Minus => Self::Subtract,
            Asterisk => Self::Multiply,
            Slash => Self::Divide,
        }
    }
}

impl From<ComparatorSymbol> for BinaryOperator {
    fn from(comparator: ComparatorSymbol) -> Self {
        match comparator {
            NotEqualTo => Self::NotEqualTo,
            EqualTo => Self::EqualTo,
            LessThan => Self::LessThan,
            GreaterThan => Self::GreaterThan,
            LessThanOrEqualTo => Self::LessOrEqualTo,
            GreaterThanOrEqualTo => Self::GreaterOrEqualTo,
        }
    }
}
