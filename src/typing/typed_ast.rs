use crate::ast::{BinaryOperator, ComparisonOperator, FuncProto, UnaryOperator};
use crate::types::IntType;
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


/// Returns true if any of the statements in the slice always return.
pub fn some_statement_always_returns(statements: &[TypedStatement]) -> bool {
    statements.iter().any(|stmt| stmt.always_returns())
}


impl TypedStatement {
    /// Returns true if this statement always returns, no matter the control flow.
    pub fn always_returns(&self) -> bool {
        match self {
            Self::VarDeclaration(_) | Self::Assignment(_) | Self::Call(_) => false,
            Self::Return(_) => true,

            Self::WhileLoop(while_loop) => some_statement_always_returns(&while_loop.body),

            Self::If(if_stmt) => {
                let then_always_returns = some_statement_always_returns(&if_stmt.then_body);
                let else_always_returns = match if_stmt.else_body.as_ref() {
                    Some(else_body) => some_statement_always_returns(else_body),
                    None => true,
                };
        
                then_always_returns && else_always_returns
            }

        }
    }
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
    BoolLiteral(bool),
    Binary(TypedBinary),
    Comparison(TypedComparison),
    Call(TypedCall),
    Unary(TypedUnary),
}

impl TypedExpr {
    /// Returns the type of this expression upon evaluation
    pub fn get_result_type(&self) -> Type {
        match self {
            Self::Identifier(id) => id.id_type.clone(),
            Self::IntLiteral(int) => Type::Int(int.int_type),
            Self::BoolLiteral(_) => Type::Bool,
            Self::Binary(binary) => binary.result_type.clone(),
            Self::Comparison(_) => Type::Bool,
            Self::Call(call) => *call.function_proto.return_type.clone(),
            Self::Unary(unary) => unary.result_type.clone(),
        }
    }
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
    pub operand_type: Type,
}

/// A call expression (the name of the function to call and the arguments to pass).
///
/// For example, `foo(a, 12 - b, "test")` is a call expression with 3 args.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedCall {
    pub function_name: String,
    pub function_proto: FuncProto,
    pub args: Vec<TypedExpr>,
}

/// An identifier, like `x` or `cur_count`, along with its type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedIdentifier {
    pub name: String,
    pub id_type: Type,
}


/// A typed version of [Unary](crate::ast::Unary).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypedUnary {
    pub operator: UnaryOperator,
    pub operand: Box<TypedExpr>,
    pub result_type: Type,
}
