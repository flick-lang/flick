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
            self.define_func(func_def);
        }
        for func_def in program.func_defs.iter() {
            func_defs.push(self.type_func_def(func_def));
        }

        TypedProgram { func_defs }
    }

    fn define_func(&mut self, func_def: &FuncDef) {
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
    }

    fn type_func_def(&mut self, func_def: &FuncDef) -> TypedFuncDef {
        self.scope_manager.enter_scope();

        for param in func_def.proto.params.iter() {
            let param_name = &param.param_name;
            let param_type = param.param_type.clone();
            self.scope_manager.set(param_name, param_type);
        }

        let mut func_body = Vec::with_capacity(func_def.body.len());
        for statement in func_def.body.iter() {
            func_body.push(self.type_statement(statement, &func_def.proto.return_type));
        }

        self.scope_manager.exit_scope();

        TypedFuncDef {
            proto: func_def.proto.clone(),
            body: func_body,
        }
    }

    fn type_statement(
        &mut self,
        statement: &Statement,
        function_return_type: &Type,
    ) -> TypedStatement {
        match statement {
            Statement::VarDeclaration(v) => {
                TypedStatement::VarDeclaration(self.type_var_declaration(v))
            }
            Statement::WhileLoop(w) => {
                TypedStatement::WhileLoop(self.type_while_loop(w, function_return_type))
            }
            Statement::Assignment(a) => TypedStatement::Assignment(self.type_assignment(a)),
            Statement::Return(r) => {
                TypedStatement::Return(self.type_return(r.as_ref(), function_return_type))
            }
        }
    }

    fn type_var_declaration(&mut self, var_declaration: &VarDeclaration) -> TypedVarDeclaration {
        let var_name = var_declaration.var_name.clone();
        let var_type = var_declaration.var_type.clone();
        let var_value = self.type_expr(&var_declaration.var_value, Some(&var_type));
        self.scope_manager.set(&var_name, var_type.clone());

        TypedVarDeclaration {
            var_name,
            var_type,
            var_value,
        }
    }

    fn type_while_loop(
        &mut self,
        while_loop: &WhileLoop,
        function_return_type: &Type,
    ) -> TypedWhileLoop {
        let bool_type = Type::Int(IntType { width: 1 });
        let condition = self.type_expr(&while_loop.condition, Some(&bool_type));

        self.scope_manager.enter_scope();
        let body: Vec<_> = while_loop
            .body
            .iter()
            .map(|s| self.type_statement(s, function_return_type))
            .collect();
        self.scope_manager.exit_scope();

        TypedWhileLoop { condition, body }
    }

    fn type_assignment(&mut self, assignment: &Assignment) -> TypedAssignment {
        let name = assignment.name.clone();
        let var_type = match self.scope_manager.get(&name) {
            Some(t) => t.clone(),
            None => panic!("Variable '{}' has not been declared yet", name),
        };
        let value = self.type_expr(assignment.value.as_ref(), Some(&var_type));

        self.scope_manager.set(&name, var_type);

        TypedAssignment {
            name,
            value: Box::new(value),
        }
    }

    fn type_return(
        &mut self,
        ret: Option<&Expr>,
        function_return_type: &Type,
    ) -> Option<TypedExpr> {
        ret.map(|e| self.type_expr(e, Some(&function_return_type)))
    }

    fn type_expr(&mut self, expr: &Expr, desired_type: Option<&Type>) -> TypedExpr {
        match expr {
            Expr::Identifier(id) => TypedExpr::Identifier(id.clone()),
            Expr::IntLiteral(int) => {
                TypedExpr::IntLiteral(self.type_int_literal(int, desired_type))
            }
            Expr::Binary(b) => TypedExpr::Binary(self.type_bin_expr(b, desired_type)),
            Expr::Comparison(c) => {
                TypedExpr::Comparison(self.type_comparison_expr(c, desired_type))
            }
            Expr::Call(c) => TypedExpr::Call(self.type_call_expr(c, desired_type)),
        }
    }

    fn type_int_literal(
        &mut self,
        int_literal: &str,
        desired_type: Option<&Type>,
    ) -> TypedIntLiteral {
        let default_type = Type::Int(IntType { width: 64 });

        TypedIntLiteral {
            int_value: int_literal.to_string(),
            int_type: desired_type.unwrap_or(&default_type).clone(),
        }
    }

    fn type_bin_expr(&mut self, bin_expr: &Binary, desired_type: Option<&Type>) -> TypedBinary {
        let left = self.type_expr(&bin_expr.left, desired_type);
        let operator = bin_expr.operator;
        let right = self.type_expr(&bin_expr.right, desired_type);
        TypedBinary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn type_comparison_expr(
        &mut self,
        comparison: &Comparison,
        desired_type: Option<&Type>,
    ) -> TypedComparison {
        if desired_type != Some(&Type::Int(IntType { width: 1 })) {
            panic!("Comparison returns an i1 but expected '{:?}'", desired_type);
        }
        // TODO: Find common type

        let left = self.type_expr(&comparison.left, None);
        let operator = comparison.operator;
        let right = self.type_expr(&comparison.right, None);

        let left_type = self.find_type(&left);
        let right_type = self.find_type(&right);

        TypedComparison {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn type_call_expr(&mut self, call_expr: &Call, desired_type: Option<&Type>) -> TypedCall {
        let function_name = call_expr.function_name.clone();

        let function_type = match self.scope_manager.get(&function_name) {
            Some(Type::Func(f)) => f.clone(),
            Some(_) => panic!("Variable '{}' is not a function", function_name),
            None => panic!(
                "Function '{}' is called but has not been defined",
                function_name
            ),
        };

        if Some(function_type.return_type.as_ref()) != desired_type {
            panic!(
                "Expected function '{}' to return '{}' but it has return type '{:?}'",
                function_name, function_type.return_type, desired_type
            )
        }

        let args: Vec<_> = call_expr
            .args
            .iter()
            .zip(function_type.param_types.iter())
            .map(|(e, t)| self.type_expr(e, Some(t)))
            .collect();

        TypedCall {
            function_name,
            args,
        }
    }

    fn find_type(&mut self, typed_expr: &TypedExpr) -> Type {
        match typed_expr {
            TypedExpr::Identifier(id) => self.scope_manager.get(id).unwrap().clone(),
            TypedExpr::IntLiteral(int) => int.int_type.clone(),
            TypedExpr::Binary(_) => todo!(),
            TypedExpr::Comparison(_) => Type::Int(IntType { width: 1 }),
            TypedExpr::Call(call) => match self.scope_manager.get(&call.function_name) {
                Some(Type::Func(f)) => f.return_type.as_ref().clone(),
                _ => unreachable!(),
            },
        }
    }

    fn find_common_type(&mut self) {}

    fn cast_type(&mut self) {}
}
