use crate::ast::{
    Assignment, Binary, Call, Comparison, Expr, FuncDef, Program, Statement, VarDeclaration,
    WhileLoop,
};
use crate::scope_manager::ScopeManager;
use crate::typed_ast::{
    TypedAssignment, TypedBinary, TypedCall, TypedComparison, TypedExpr, TypedFuncDef,
    TypedIdentifier, TypedIntLiteral, TypedProgram, TypedStatement, TypedVarDeclaration,
    TypedWhileLoop,
};
use crate::types::{FuncType, IntType};
use crate::Type;

pub struct Typer {
    scope_manager: ScopeManager<Type>,
}

// mention that the purpose of this struct is to: (a) catch type mismatches, (b) coerce types,j
// (c) handle stuff like i64 a = b + c where b and c are i32, (d) disallow i8 = b + c if b, c i32
// -> don't mention the things it doesn't do yet (since those are left as todos)
// TODO: Document Typer!
impl Typer {
    pub fn new() -> Self {
        let scope_manager = ScopeManager::new();
        Self { scope_manager }
    }

    /// This function goes through the entire `program` and converts it to a [TypedProgram], panicking if
    /// any type mismatches are uncovered.
    /// 
    /// This function assumes that `program` represents a well-parsed program; for example, one returned
    /// by [Parser::parse_program()](crate::Parser::parse_program).
    pub fn type_program(&mut self, program: &Program) -> TypedProgram {
        let mut func_defs = Vec::with_capacity(program.func_defs.len());

        for func_def in program.func_defs.iter() {
            self.declare_func(func_def);
        }
        for func_def in program.func_defs.iter() {
            func_defs.push(self.type_func_def(func_def));
        }

        TypedProgram { func_defs }
    }

    fn declare_func(&mut self, func_def: &FuncDef) {
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

        // Implicitly return void at end of functions with void return type
        if Type::Void == func_def.proto.return_type {
            match func_body.last() {
                Some(TypedStatement::Return(_)) => {}
                _ => func_body.push(TypedStatement::Return(None)),
            }
        }

        self.scope_manager.exit_scope();

        TypedFuncDef {
            proto: func_def.proto.clone(),
            body: func_body,
        }
    }

    // This method takes `function_return_type` in order to type-check return statements, or
    // statements like while loops that might contain return statements.
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
        ret.map(|e| self.type_expr(e, Some(function_return_type)))
    }

    // desired type is optional because, for example, 17 doesn't have a desired type
    fn type_expr(&mut self, expr: &Expr, desired_type: Option<&Type>) -> TypedExpr {
        match expr {
            Expr::Identifier(name) => {
                TypedExpr::Identifier(self.type_identifier(name, desired_type))
            }
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

    /// Checks that the identifier labeled `name` can be interpreted as the type `desired_type`, and
    /// wraps it as a `TypedIdentifier`.
    ///
    /// Note: currently, this function will fail unless `name` was declared to be `desired_type`.
    /// In the future, this function might support type escalation, like letting an `name` of type
    /// `i32` but be allowed to be typed as `i64`.
    fn type_identifier(&self, name: &str, desired_type: Option<&Type>) -> TypedIdentifier {
        let actual_type = self.scope_manager.get(name);
        let id_type = match (actual_type, desired_type) {
            (None, _) => panic!("Identifier '{}' has not been declared yet.", name),
            (Some(actual), Some(desired)) if actual == desired => actual.clone(),
            (Some(actual), Some(desired)) => panic!(
                "Identifier '{}' of type '{}' cannot be used as type '{}'",
                name, actual, desired
            ),
            (Some(actual), None) => actual.clone(),
        };

        TypedIdentifier {
            name: name.to_string(),
            id_type,
        }
    }

    /// Checks that `desired_type` is a valid type (namely, an integer type) and wraps the
    /// `int_literal` as a `TypedIntLiteral`.
    fn type_int_literal(&self, int_literal: &str, desired_type: Option<&Type>) -> TypedIntLiteral {
        let int_type = match desired_type {
            Some(Type::Int(int_type)) => *int_type,
            Some(t) => {
                panic!(
                    "expected integer type for literal '{}', but the desired type is '{}'",
                    int_literal, t
                )
            }
            None => IntType { width: 64 },
        };

        TypedIntLiteral {
            int_value: int_literal.to_string(),
            int_type,
        }
    }

    fn type_bin_expr(&mut self, bin_expr: &Binary, desired_type: Option<&Type>) -> TypedBinary {
        let left = self.type_expr(&bin_expr.left, desired_type);
        let operator = bin_expr.operator;
        let right = self.type_expr(&bin_expr.right, desired_type);

        let left_type = self.find_type(&left);
        let right_type = self.find_type(&right);
        if left_type != right_type {
            panic!(
                "Operator '{}' needs left-hand-side ({}) and right-hand-side ({}) to be the same type",
                operator, left_type, right_type
            );
        }

        TypedBinary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            result_type: left_type, // since both types must be equal
        }
    }

    fn type_comparison_expr(
        &mut self,
        comparison: &Comparison,
        desired_type: Option<&Type>,
    ) -> TypedComparison {
        if desired_type.is_some() && desired_type != Some(&Type::Int(IntType { width: 1 })) {
            panic!(
                "Comparison returns an i1 but expected '{}'",
                desired_type.unwrap()
            );
        }
        // TODO for future: Find common type (by casting/coalescing), like i64 can fit both i64 and i32

        let left = self.type_expr(&comparison.left, None);
        let operator = comparison.operator;
        let right = self.type_expr(&comparison.right, None);

        let left_type = self.find_type(&left);
        let right_type = self.find_type(&right);

        if left_type != right_type {
            panic!(
                "Comparison '{}' needs left-hand-side ({}) and right-hand-side ({}) to be the same type",
                comparison.operator, left_type, right_type
            );
        }

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

        if desired_type.is_some() && Some(function_type.return_type.as_ref()) != desired_type {
            panic!(
                "Expected function '{}' to return '{}' but it has return type '{}'",
                function_name,
                function_type.return_type,
                desired_type.unwrap()
            )
        }

        let num_params = function_type.param_types.len();
        if num_params != call_expr.args.len() {
            panic!(
                "Expected {} argument(s) to function '{}'; got {} argument(s)",
                function_type.param_types.len(),
                call_expr.function_name,
                call_expr.args.len()
            );
        }

        let args: Vec<_> = call_expr
            .args
            .iter()
            .zip(function_type.param_types.iter())
            .map(|(e, desired)| self.type_expr(e, Some(desired)))
            .collect();

        TypedCall {
            function_name,
            function_type,
            args,
        }
    }

    /// Extracts the type from a typed expression.
    fn find_type(&mut self, typed_expr: &TypedExpr) -> Type {
        match typed_expr {
            TypedExpr::Identifier(id) => id.id_type.clone(),
            TypedExpr::IntLiteral(int) => Type::Int(int.int_type),
            TypedExpr::Binary(binary) => binary.result_type.clone(),
            TypedExpr::Comparison(_) => Type::Int(IntType { width: 1 }),
            TypedExpr::Call(call) => Type::Func(call.function_type.clone()),
        }
    }
}

/// As suggested by Clippy's [new_without_default][a], since [Typer::new()] doesn't
/// take any arguments, Typer should implement Default.
///
/// [a]: https://rust-lang.github.io/rust-clippy/master/index.html#/new_without_default
impl Default for Typer {
    fn default() -> Self {
        Typer::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use super::*;

    #[test]
    #[should_panic]
    fn assignmen_with_mismatched_types() {
        // pub fn main() i32 {
        //     i64 a = 3
        //     i64 b = a
        //     i32 c = b  // should error
        // }

        let program = Program {
            func_defs: vec![FuncDef {
                proto: FuncProto {
                    is_public: true,
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Type::Int(IntType { width: 32 })
                },
                body: vec![
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "a".to_string(),
                        var_value: Expr::IntLiteral("3".to_string()),
                        var_type: Type::Int(IntType { width: 64 })
                    }),
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "b".to_string(),
                        var_value: Expr::IntLiteral("a".to_string()),
                        var_type: Type::Int(IntType { width: 64 })
                    }),
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "c".to_string(),
                        var_value: Expr::Identifier("b".to_string()),   // this should panic, since b (i64) can't be in c (i32)
                        var_type: Type::Int(IntType { width: 32 })
                    })
                ]
            }]
        };

        let mut typer = Typer::new();
        let _ = typer.type_program(&program);
    }

    #[test]
    fn valid_assignments() {
        // pub fn main() i32 {
        //     i32 a = 3
        //     i32 b = a
        //     ret b
        // }

        let program = Program {
            func_defs: vec![FuncDef {
                proto: FuncProto {
                    is_public: true,
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Type::Int(IntType { width: 32 })
                },
                body: vec![
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "a".to_string(),
                        var_value: Expr::IntLiteral("3".to_string()),
                        var_type: Type::Int(IntType { width: 32 })
                    }),
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "b".to_string(),
                        var_value: Expr::IntLiteral("a".to_string()),
                        var_type: Type::Int(IntType { width: 32 })
                    }),
                    Statement::Return(Some(Expr::Identifier("b".to_string())))
                ]
            }]
        };

        let expected_typed_program = TypedProgram {
            func_defs: vec![TypedFuncDef {
                proto: FuncProto {
                    is_public: true,
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Type::Int(IntType { width: 32 })
                },
                body: vec![
                    TypedStatement::VarDeclaration(TypedVarDeclaration {
                        var_name: "a".to_string(),
                        var_value: TypedExpr::IntLiteral(TypedIntLiteral {
                            int_value: "3".to_string(),
                            int_type: IntType { width: 32 }
                        }),
                        var_type: Type::Int(IntType { width: 32 })
                    }),
                    TypedStatement::VarDeclaration(TypedVarDeclaration {
                        var_name: "b".to_string(),
                        var_value: TypedExpr::IntLiteral(TypedIntLiteral {
                            int_value: "a".to_string(),
                            int_type: IntType { width: 32 }
                        }),
                        var_type: Type::Int(IntType { width: 32 })
                    }),
                    TypedStatement::Return(Some(TypedExpr::Identifier(TypedIdentifier {
                        name: "b".to_string(),
                        id_type: Type::Int(IntType { width: 32 })
                    })))
                ]
            }]
        };
        
        let mut typer = Typer::new();
        let actual_typed_program = typer.type_program(&program);
        assert_eq!(expected_typed_program, actual_typed_program);
    }
}
