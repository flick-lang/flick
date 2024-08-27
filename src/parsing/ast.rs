use crate::lexing::token::ComparatorSymbol::*;
use crate::lexing::token::OperatorSymbol::*;
use crate::lexing::token::{ComparatorSymbol, OperatorSymbol};
use crate::Type;
use std::fmt;

/// A program consisting of at least one [GlobalStatement].
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub global_statements: Vec<GlobalStatement>,
}

/// A global statement is something that can be written in the "global" scope, as opposed
/// to inside of a function body. So, for example, function definitions and external function
/// declarations are "global" statements.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum GlobalStatement {
    Extern(FuncProto),
    FuncDef(FuncDef),
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
    pub func_visibility: FuncVisibility,
    pub name: String,
    pub params: Vec<FuncParam>,
    pub return_type: Box<Type>,
}

impl fmt::Display for FuncProto {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|p| format!("{} {}", p.param_type, p.param_name))
            .collect::<Vec<String>>()
            .join(", ");
        write!(
            f,
            "{} {}({}) {}",
            self.func_visibility, self.name, params, self.return_type
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FuncVisibility {
    Public,
    Private,
    Extern,
}

impl fmt::Display for FuncVisibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Public => write!(f, "pub fn"),
            Self::Private => write!(f, "fn"),
            Self::Extern => write!(f, "extern fn"),
        }
    }
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

/// A variable declaration.
///
/// This struct stores the name and type of the declared variable, as well as its
/// initial value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDeclaration {
    pub var_name: String,
    pub var_type: Type,
    pub var_value: Expr,
}

/// An if statement.
///
/// Note, `then_body` corresponds to the statements to be executed if the condition is true,
/// and `else_body` (optional) corresponds to the "else" block of the if statement.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub condition: Expr,
    pub then_body: Vec<Statement>,
    pub else_body: Option<Vec<Statement>>,
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
    IntLiteral(IntLiteral),
    BoolLiteral(bool),
    Binary(Binary),
    Comparison(Comparison),
    Call(Call),
    Unary(Unary),
}

/// An assignment statement (the variable name and the new value).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assignment {
    pub name: String,
    pub value: Box<Expr>,
}

/// An integer literal, which represents a constant integer value in the source code.
///
/// For example, `42` or `-42` are integer literals.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntLiteral {
    pub negative: bool,
    pub value: String,
}

impl fmt::Display for IntLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.negative {
            write!(f, "-")?;
        }
        write!(f, "{}", self.value)
    }
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
    Remainder,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Remainder => write!(f, "%"),
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
            Modulo => Self::Remainder,
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

/// A unary expression, which consists of an operator (e.g. "cast to u32") and a value.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Unary {
    pub operator: UnaryOperator,
    pub expr: Box<Expr>,
}

/// A unary operator, like "cast to u32" or "not".
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    /// A cast converts its operand into the specified destination [Type].
    Cast(Type),
}
