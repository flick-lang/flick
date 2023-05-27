use crate::token::OperatorSymbol::*;
use crate::token::{OperatorSymbol, VarType};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    VarDeclaration {
        var_name: Expr,
        var_type: VarType,
        var_value: Expr,
    },
    WhileLoop {
        condition: Expr,
        body: Vec<Statement>,
    },
    Expr(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Identifier(String),
    Int(isize),
    BinExpr {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    CallExpr {
        function_name: Box<Expr>,
        args: Vec<Expr>,
    },
    IndexExpr {
        container: Box<Expr>,
        index: (),
    },
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
