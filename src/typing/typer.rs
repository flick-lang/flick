use crate::ast::{
    Assignment, Binary, Call, Comparison, Expr, FuncDef, Program, Statement, VarDeclaration,
    WhileLoop,
};
use crate::scope_manager::ScopeManager;
use crate::typed_ast::{
    TypedAssignment, TypedBinary, TypedCall, TypedComparison, TypedExpr, TypedFuncDef,
    TypedIntLiteral, TypedProgram, TypedStatement, TypedVarDeclaration, TypedWhileLoop,
};
use crate::types::{FuncType, IntType};
use crate::Type;

pub struct Typer {
    scope_manager: ScopeManager<Type>,
}

impl Typer {
    pub fn new() -> Self {
        let scope_manager = ScopeManager::new();
        Self { scope_manager }
    }

    pub fn type_program(&mut self, program: &Program) -> TypedProgram {
        let mut func_defs = Vec::with_capacity(program.func_defs.len());
        for func_def in program.func_defs.iter() {
            func_defs.push(self.type_func_def(func_def))
        }
        TypedProgram { func_defs }
    }

    fn type_func_def(&mut self, func_def: &FuncDef) -> TypedFuncDef {
        let func_name = &func_def.proto.name;
        match self.scope_manager.get(func_name) {
            Some(Type::Func(_)) => panic!("Cannot redefine function '{}'", func_name),
            Some(_) => panic!(
                "Cannot define function '{}' because variable with same name already exists",
                func_name
            ),
            None => {}
        }

        let param_types: Vec<_> = func_def
            .proto
            .params
            .iter()
            .map(|p| p.param_type.clone())
            .collect();
        let return_type = Box::new(func_def.proto.return_type.clone());
        let func_type = Type::Func(FuncType {
            param_types,
            return_type,
        });
        self.scope_manager.set(func_name, func_type);

        for param in func_def.proto.params.iter() {
            let param_name = &param.param_name;
            let param_type = param.param_type.clone();
            self.scope_manager.set(param_name, param_type);
        }

        let mut func_body = Vec::with_capacity(func_def.body.len());
        for statement in func_def.body.iter() {
            func_body.push(self.type_statement(statement));
        }

        TypedFuncDef {
            proto: func_def.proto.clone(),
            body: func_body,
        }
    }

    fn type_statement(&mut self, statement: &Statement) -> TypedStatement {
        match statement {
            Statement::VarDeclaration(v) => {
                TypedStatement::VarDeclaration(self.type_var_declaration(v))
            }
            Statement::WhileLoop(w) => TypedStatement::WhileLoop(self.type_while_loop(w)),
            Statement::Assignment(a) => TypedStatement::Assignment(self.type_assignment(a)),
            Statement::Return(r) => TypedStatement::Return(self.type_return(r.as_ref())),
        }
    }

    fn type_var_declaration(&mut self, var_declaration: &VarDeclaration) -> TypedVarDeclaration {
        let var_name = var_declaration.var_name.clone();
        let var_type = var_declaration.var_type.clone();
        let var_value = self.type_expr(&var_declaration.var_value);
        self.scope_manager.set(&var_name, var_type.clone());

        TypedVarDeclaration {
            var_name,
            var_type,
            var_value,
        }
    }

    fn type_while_loop(&mut self, while_loop: &WhileLoop) -> TypedWhileLoop {
        let condition = self.type_expr(&while_loop.condition);
        let body: Vec<_> = while_loop
            .body
            .iter()
            .map(|s| self.type_statement(s))
            .collect();

        TypedWhileLoop { condition, body }
    }

    fn type_assignment(&mut self, assignment: &Assignment) -> TypedAssignment {
        let name = assignment.name.clone();
        let value = self.type_expr(assignment.value.as_ref());
        TypedAssignment {
            name,
            value: Box::new(value),
        }
    }

    fn type_return(&mut self, ret: Option<&Expr>) -> Option<TypedExpr> {
        ret.map(|e| self.type_expr(e))
    }

    fn type_expr(&mut self, expr: &Expr) -> TypedExpr {
        match expr {
            Expr::Identifier(id) => TypedExpr::Identifier(id.clone()),
            Expr::IntLiteral(int) => TypedExpr::IntLiteral(self.type_int_literal(int)),
            Expr::Binary(b) => TypedExpr::Binary(self.type_bin_expr(b)),
            Expr::Comparison(c) => TypedExpr::Comparison(self.type_comparison_expr(c)),
            Expr::Call(c) => TypedExpr::Call(self.type_call_expr(c)),
        }
    }

    fn type_int_literal(&mut self, int_literal: &str) -> TypedIntLiteral {
        TypedIntLiteral {
            int_value: int_literal.to_string(),
            int_type: Type::Int(IntType { width: 64 }),
        }
    }

    fn type_bin_expr(&mut self, bin_expr: &Binary) -> TypedBinary {
        let left = self.type_expr(&bin_expr.left);
        let operator = bin_expr.operator;
        let right = self.type_expr(&bin_expr.right);
        TypedBinary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn type_comparison_expr(&mut self, comparison: &Comparison) -> TypedComparison {
        let left = self.type_expr(&comparison.left);
        let operator = comparison.operator;
        let right = self.type_expr(&comparison.right);
        TypedComparison {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn type_call_expr(&mut self, call_expr: &Call) -> TypedCall {
        let function_name = call_expr.function_name.clone();
        let args: Vec<_> = call_expr.args.iter().map(|a| self.type_expr(a)).collect();
        TypedCall {
            function_name,
            args,
        }
    }
}
