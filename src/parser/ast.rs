use crate::lexer::token::OperatorSymbol::*;
use crate::lexer::token::{OperatorSymbol, Type};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub func_defs: Vec<FuncDef>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<FuncParam>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncParam {
    pub param_type: Type,
    pub param_name: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    VarDeclaration(VarDeclaration),
    WhileLoop(WhileLoop),
    ExprStatement(Expr),
    ReturnStatement(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDeclaration {
    pub var_name: String,
    pub var_type: Type,
    pub var_value: Expr,
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
    BinExpr(BinExpr),
    CallExpr(CallExpr),
    IndexExpr(IndexExpr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallExpr {
    pub function_name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IndexExpr {
    pub container: Box<Expr>,
    pub index: (),
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

    PlusEq,
    TimesEq,
    MinusEq,
    DivideEq,
    Assign,
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

            NotEqualTo => Self::NotEqualTo,
            EqualTo => Self::EqualTo,
            LessThan => Self::LessThan,
            GreaterThan => Self::GreaterThan,
            LessThanOrEqualTo => Self::LessOrEqualTo,
            GreaterThanOrEqualTo => Self::GreaterOrEqualTo,

            PlusEq => Self::PlusEq,
            TimesEq => Self::TimesEq,
            MinusEq => Self::MinusEq,
            DivideEq => Self::DivideEq,
            Assign => Self::Assign,
        }
    }
}
