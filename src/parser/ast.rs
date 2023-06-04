use crate::lexer::token::ComparatorSymbol::*;
use crate::lexer::token::OperatorSymbol::*;
use crate::lexer::token::{ComparatorSymbol, OperatorSymbol, Type};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub func_defs: Vec<FuncDef>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncDef {
    pub is_public: bool,
    pub proto: FuncProto,
    pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncProto {
    pub name: String,
    pub params: Vec<FuncParam>,
    pub return_type: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncParam {
    pub param_type: Type,
    pub param_name: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    VarDeclarations(Vec<VarDeclaration>),
    WhileLoop(WhileLoop),
    Expr(Expr),
    Return(Option<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDeclaration {
    pub var_name: String,
    pub var_type: Type,
    pub var_value: Option<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct WhileLoop {
    pub condition: Expr,
    pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Identifier(String),
    I64Literal(i64),
    Assign(Assign),
    Binary(Binary),
    Call(Call),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assign {
    pub name: String,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub right: Box<Expr>,
}

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
