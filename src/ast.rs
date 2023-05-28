use crate::token::OperatorSymbol::*;
use crate::token::{OperatorSymbol, Type};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    VarDeclaration(VarDeclaration),
    WhileLoop(WhileLoop),
    Expr(Expr),
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
    Int(i64),
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
    pub function_name: Box<Expr>,
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
            LessOrEqualTo => Self::LessOrEqualTo,
            GreaterOrEqualTo => Self::GreaterOrEqualTo,

            PlusEq => Self::PlusEq,
            TimesEq => Self::TimesEq,
            MinusEq => Self::MinusEq,
            DivideEq => Self::DivideEq,
            Assign => Self::Assign,
        }
    }
}
