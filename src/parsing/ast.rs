use crate::lexing::token::ComparatorSymbol::*;
use crate::lexing::token::OperatorSymbol::*;
use crate::lexing::token::{ComparatorSymbol, OperatorSymbol};
use crate::Type;
use std::fmt;

/// A program; a collection of function definitions. See also: [FuncDef].
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub func_defs: Vec<FuncDef>,
}

/// A function definition (metadata, prototype, and body).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncDef {
    pub proto: FuncProto,
    pub body: Vec<Statement>,
}

/// A function prototype (name, parameters, and return type).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncProto {
    pub is_public: bool,
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
/// See also: [Expr].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    VarDeclaration(VarDeclaration),
    WhileLoop(WhileLoop),
    Assignment(Assignment),
    Return(Option<Expr>),
    Call(Call),
    If(If),
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
    pub var_value: Expr,
}

/// A if statement (its 'if condition' and its body).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub condition: Expr,
    pub body: Vec<Statement>,
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
    IntLiteral(String),
    Binary(Binary),
    Comparison(Comparison),
    Call(Call),
}

/// An assignment statement (the variable name and the new value).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assignment {
    pub name: String,
    pub value: Box<Expr>,
}

/// A binary expression (the operator and the left/right-hand sides).
///
/// For example, `a + foo(1)` breaks down into:
/// - left: `a`
/// - operator: `+`
/// - right: `foo(1)`
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub right: Box<Expr>,
}

/// An operator for the [Binary] expression.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
        }
    }
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

/// A comparison expression (the operator and the left/right-hand sides).
///
/// For example, `a < foo(1)` breaks down into:
/// - left: `a`
/// - operator: `<`
/// - right: `foo(1)`
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Comparison {
    pub left: Box<Expr>,
    pub operator: ComparisonOperator,
    pub right: Box<Expr>,
}

/// An operator for the [Comparison] expression.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ComparisonOperator {
    NotEqualTo,
    EqualTo,
    LessThan,
    GreaterThan,
    LessOrEqualTo,
    GreaterOrEqualTo,
}

impl fmt::Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotEqualTo => write!(f, "!="),
            Self::EqualTo => write!(f, "=="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessOrEqualTo => write!(f, "<="),
            Self::GreaterOrEqualTo => write!(f, ">="),
        }
    }
}

impl From<ComparatorSymbol> for ComparisonOperator {
    fn from(comparator: ComparatorSymbol) -> Self {
        match comparator {
            NotEqualTo => Self::NotEqualTo,
            EqualTo => Self::EqualTo,
            LessThan => Self::LessThan,
            GreaterThan => Self::GreaterThan,
            LessOrEqualTo => Self::LessOrEqualTo,
            GreaterOrEqualTo => Self::GreaterOrEqualTo,
        }
    }
}

/// A call expression (the name of the function to call and the arguments to pass).
///
/// For example, `foo(a, 12 - b, "test")` is a call expression with 3 args.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    pub function_name: String,
    pub args: Vec<Expr>,
}
