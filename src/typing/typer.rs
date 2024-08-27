use crate::ast::{
    Assignment, Binary, Call, Comparison, Expr, FuncDef, FuncProto, FuncVisibility,
    GlobalStatement, If, IntLiteral, Program, Statement, Unary, UnaryOperator, VarDeclaration,
    WhileLoop,
};
use crate::scope_manager::ScopeManager;
use crate::typed_ast::{
    TypedAssignment, TypedBinary, TypedCall, TypedComparison, TypedExpr, TypedFuncDef,
    TypedGlobalStatement, TypedIdentifier, TypedIf, TypedIntLiteral, TypedProgram, TypedStatement,
    TypedUnary, TypedVarDeclaration, TypedWhileLoop,
};
use crate::types::IntType;
use crate::Type;

/// This struct handles the conversion from a regular [abstract syntax tree](crate::ast) to a
/// [typed abstract syntax tree](crate::typed_ast). See [Typer::type_program] for details.
pub struct Typer {
    scope_manager: ScopeManager<Type>,
}

impl Typer {
    pub fn new() -> Self {
        let scope_manager = ScopeManager::new();
        Self { scope_manager }
    }

    /// This method goes through the entire `program` and converts it to a [TypedProgram],
    /// panicking if any type mismatches or undefined identifiers are uncovered.
    ///
    /// In the future, this method may also coerce types as necessary, such as when `i32` and
    /// `i32` are summed and placed into an `i64` (currently, programs can only store `i32 + i32`
    /// in another `i32`).
    ///
    /// # Assumptions
    ///
    /// This method assumes that `program` represents a well-parsed program; for example, one
    /// returned by [Parser::parse_program()](crate::Parser::parse_program).
    pub fn type_program(&mut self, program: &Program) -> TypedProgram {
        let mut global_statements = Vec::with_capacity(program.global_statements.len());

        self.scope_manager.enter_scope();
        for global_statement in program.global_statements.iter() {
            match global_statement {
                GlobalStatement::Extern(proto) => self.register_func_proto(proto),
                GlobalStatement::FuncDef(f) => self.register_func_proto(&f.proto),
            }
        }
        for global_statement in program.global_statements.iter() {
            global_statements.push(self.type_global_statement(global_statement))
        }
        self.check_valid_main_func();
        self.scope_manager.exit_scope();

        TypedProgram { global_statements }
    }

    fn check_valid_main_func(&self) {
        let func_proto = match self.scope_manager.get("main") {
            Some(Type::Func(proto)) => proto,
            Some(t) => panic!("Expected 'main' to be a function; found 'main' to be of type {}", t),
            None => panic!("No main function defined")
        };

        if func_proto.func_visibility != FuncVisibility::Public {
            panic!("The 'main' function should be public");
        }

        if !func_proto.params.is_empty() {
            panic!("The 'main' function should not accept any parameters");
        }

        if *func_proto.return_type != Type::Int(IntType { width: 8, signed: false }) {
            panic!("The 'main' function should return a u8");
        }
    }

    /// This method processes the prototype of a function, updating the internal scope manager
    /// and confirming the function isn't being redeclared.
    fn register_func_proto(&mut self, func_proto: &FuncProto) {
        let func_name = &func_proto.name;
        match self.scope_manager.get(func_name) {
            Some(Type::Func(_)) => panic!("Cannot redefine function '{}'", func_name),
            Some(_) => panic!(
                "Cannot define function '{}' because variable with same name already exists",
                func_name
            ),
            None => {}
        }

        let func_type = Type::Func(func_proto.clone());
        self.scope_manager.set(func_name, func_type);
    }

    fn type_global_statement(&mut self, global_statement: &GlobalStatement) -> TypedGlobalStatement {
        match global_statement {
            GlobalStatement::Extern(proto) => TypedGlobalStatement::Extern(proto.clone()),
            GlobalStatement::FuncDef(f) => TypedGlobalStatement::FuncDef(self.type_func_def(f)),
        }
    }

    /// This method processes a function definition by processing each statement within the body,
    /// and by confirming that it always returns the correct type (according to its prototype).
    fn type_func_def(&mut self, func_def: &FuncDef) -> TypedFuncDef {
        self.scope_manager.enter_scope();

        for param in func_def.proto.params.iter() {
            let param_name = &param.param_name;
            let param_type = param.param_type.clone();
            self.scope_manager.set(param_name, param_type);
        }

        let mut func_body = self.type_body(&func_def.body, &func_def.proto.return_type);

        // Implicitly return void at end of functions with void return type
        if Type::Void == *func_def.proto.return_type {
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

    /// This method processes a statement and makes sure that its internals are well-typed.
    ///
    /// # Notes
    ///
    /// This method takes `function_return_type` so that it can type-check return statements, or
    /// statements like while loops that might contain return statements.
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
            // Below, the desired_type of the call is None because the value returned by the call is never used
            Statement::Call(c) => TypedStatement::Call(self.type_call(c, None)),
            Statement::If(i) => TypedStatement::If(self.type_if_statement(i, function_return_type)),
        }
    }

    /// This method checks that the variable introduced by `var_declaration` is being set to a value
    /// of its declared type.
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

    /// This method confirms that a given body (consisting of one or more [Statement][a]) 
    /// is well-typed.
    /// 
    /// [a]: crate::ast::Statement
    fn type_body(&mut self, body: &[Statement], function_return_type: &Type) -> Vec<TypedStatement> {
        self.scope_manager.enter_scope();
        let typed_body: Vec<_> = body
            .iter()
            .map(|s| self.type_statement(s, function_return_type))
            .collect();
        self.scope_manager.exit_scope();
        typed_body
    }

    /// This method checks that an if statement has a *boolean* condition and a collection of body
    /// statements that are well-typed.
    fn type_if_statement(&mut self, if_statement: &If, function_return_type: &Type) -> TypedIf {
        let condition = self.type_expr(&if_statement.condition, Some(&Type::Bool));
        let then_body = self.type_body(&if_statement.then_body, function_return_type);
        let else_body = if_statement.else_body.as_ref().map(|body| self.type_body(body, function_return_type));
        TypedIf { condition, then_body, else_body }
    }

    /// This method checks that a while loop has a *boolean* condition and a collection of body
    /// statements that are well-typed.
    fn type_while_loop(
        &mut self,
        while_loop: &WhileLoop,
        function_return_type: &Type,
    ) -> TypedWhileLoop {
        let condition = self.type_expr(&while_loop.condition, Some(&Type::Bool));
        let body = self.type_body(&while_loop.body, function_return_type);
        TypedWhileLoop { condition, body }
    }

    /// This method checks that an assignment is assigning to a declared variable, and that the new
    /// value matches the variable's declared type.
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

    /// Processes a return statement by confirming that the returned expression matches the return
    /// type of †he function.
    fn type_return(
        &mut self,
        ret: Option<&Expr>,
        function_return_type: &Type,
    ) -> Option<TypedExpr> {
        ret.map(|e| self.type_expr(e, Some(function_return_type)))
    }

    /// Recursively type-checks the provided expression, confirming that it is of type
    /// `desired_type`.
    ///
    /// Note, if the provided `desired_type` is `None`, then the returned `TypedExpr` is allowed to
    /// be of any type.
    fn type_expr(&mut self, expr: &Expr, desired_type: Option<&Type>) -> TypedExpr {
        match expr {
            Expr::Identifier(name) => {
                TypedExpr::Identifier(self.type_identifier(name, desired_type))
            }
            Expr::IntLiteral(int) => {
                TypedExpr::IntLiteral(self.type_int_literal(int, desired_type))
            }
            Expr::BoolLiteral(b) => TypedExpr::BoolLiteral(*b),
            Expr::Binary(b) => TypedExpr::Binary(self.type_binary_expr(b, desired_type)),
            Expr::Comparison(c) => {
                TypedExpr::Comparison(self.type_comparison_expr(c, desired_type))
            }
            Expr::Call(c) => TypedExpr::Call(self.type_call(c, desired_type)),
            Expr::Unary(u) => TypedExpr::Unary(self.type_unary_expr(u, desired_type)),
        }
    }

    /// Checks that the unary expression is of the correct type, and wraps it as a `TypedUnary`.
    /// 
    /// For example, if the unary operator is a cast, then the operand must be castable to the
    /// desired type.
    fn type_unary_expr(&mut self, unary: &Unary, desired_type: Option<&Type>) -> TypedUnary {
        // desired_operand_type will be used as the desired type when typing the operand
        let desired_operand_type = match (&unary.operator, desired_type) {
            (UnaryOperator::Cast(_), None) => None,
            (UnaryOperator::Cast(cast_type), Some(desired)) if cast_type == desired => None,
            (UnaryOperator::Cast(cast_type), Some(desired)) => {
                panic!(
                    "Expected expression of type '{}', but found a cast to type '{}'",
                    desired,
                    cast_type
                )
            }

            // (UnaryOperator::Negate(_), Some(t @ Type::Int(IntType { signed: false, .. }))) => {
            //    panic!("Expected an unsigned expression of type '{}', but found a negation", t)
            // },
            // (UnaryOperator::Negate(_), Some(t @ Type::Int(_))) => t,
            // (UnaryOperator::Negate(_), Some(t )) => panic!("Cannot negate a non-integer type '{}'", t),
        };

        let typed_operand = self.type_expr(&unary.operand, desired_operand_type);
        let operand_type = typed_operand.get_type();

        // Now that we know the type of the operand, we can check if the unary operator is valid
        match &unary.operator {
            UnaryOperator::Cast(cast_type) => Self::check_valid_cast(&cast_type, &operand_type),
        }

        let result_type = match &unary.operator {
            UnaryOperator::Cast(cast_type) => cast_type.clone(),
            // UnaryOperator::Negate(_) => operand_type,
        };

        TypedUnary {
            operator: unary.operator.clone(),
            operand: Box::new(typed_operand),
            result_type,
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
    fn type_int_literal(&self, int_literal: &IntLiteral, desired_type: Option<&Type>) -> TypedIntLiteral {
        let int_type = match desired_type {
            Some(Type::Int(int_type)) => *int_type,
            Some(t) => {
                panic!(
                    "Expected integer type for literal '{}', but the desired type is '{}'",
                    int_literal, t
                )
            }
            None => IntType { signed: false, width: 64 },
        };

        if int_literal.negative && !int_type.signed {
            panic!("Expected unsigned integer type '{}', but got negative int_literal '{}'", int_type, int_literal);
        }

        // TODO: Ensure that the int_literal.value fits within the desired width

        TypedIntLiteral {
            negative: int_literal.negative,
            int_value: int_literal.value.clone(),
            int_type,
        }
    }

    /// Types a binary expression; see [Typer::type_expr] for details.
    fn type_binary_expr(&mut self, binary_expr: &Binary, desired_type: Option<&Type>) -> TypedBinary {
        let left = self.type_expr(&binary_expr.left, desired_type);
        let operator = binary_expr.operator;
        let right = self.type_expr(&binary_expr.right, desired_type);

        let left_type = left.get_type();
        let right_type = right.get_type();
        if left_type != right_type {
            panic!(
                "Operator '{}' needs left-hand-side ({}) and right-hand-side ({}) to be the same type",
                operator, left_type, right_type
            );
        }

        match left_type {
            Type::Int(_) => {}
            _ => panic!("Unsupported lhs and rhs types for binary expr; expected integer but got '{}'", left_type)
        }

        TypedBinary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            result_type: left_type, // since both types must be equal
        }
    }

    /// Types a comparison expression; see [Typer::type_expr] for details.
    fn type_comparison_expr(
        &mut self,
        comparison: &Comparison,
        desired_type: Option<&Type>,
    ) -> TypedComparison {
        if desired_type.is_some() && desired_type != Some(&Type::Bool) {
            panic!(
                "Comparison expressions return an i1 but expected '{}'",
                desired_type.unwrap()
            );
        }
        // TODO for future: Find common type (by casting/coalescing), like i64 can fit both i64 and i32

        let left = self.type_expr(&comparison.left, None);
        let operator = comparison.operator;
        let right = self.type_expr(&comparison.right, None);

        let left_type = left.get_type();
        let right_type = right.get_type();

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
            operand_type: left_type, // since both types must be equal
        }
    }

    /// Types a call expression, making sure that it matches the function's prototype, and that the
    /// return type matches the `desired_type`.[^note]
    ///
    /// [^note]: See also [Typer::type_expr] for details about `desired_type`.
    fn type_call(&mut self, call: &Call, desired_type: Option<&Type>) -> TypedCall {
        let function_name = call.function_name.clone();

        let function_proto = match self.scope_manager.get(&function_name) {
            Some(Type::Func(f)) => f.clone(),
            Some(_) => panic!("Variable '{}' is not a function", function_name),
            None => panic!(
                "Function '{}' is called but has not been defined",
                function_name
            ),
        };

        if desired_type.is_some() && Some(function_proto.return_type.as_ref()) != desired_type {
            panic!(
                "Expected function '{}' to return '{}' but it has return type '{}'",
                function_name,
                desired_type.unwrap(),
                function_proto.return_type
            )
        }

        let num_params = function_proto.params.len();
        if num_params != call.args.len() {
            panic!(
                "Expected {} argument(s) to function '{}'; got {} argument(s)",
                num_params,
                call.function_name,
                call.args.len()
            );
        }

        let args: Vec<_> = call
            .args
            .iter()
            .zip(function_proto.params.iter())
            .map(|(e, p)| self.type_expr(e, Some(&p.param_type)))
            .collect();

        TypedCall {
            function_name,
            function_proto,
            args,
        }
    }

    /// Panics if the cast is invalid, like casting from an unsigned type to a signed type.
    fn check_valid_cast(cast_type: &Type, operand_type: &Type) {
        match (cast_type, operand_type) {
            (Type::Int(IntType { signed: true, .. }), Type::Int(IntType { signed: false, .. })) => {
                panic!(
                    "Cannot cast from unsigned type '{}' to signed type '{}'",
                    operand_type, cast_type
                )
            }
            (Type::Int(IntType { signed: false, .. }), Type::Int(IntType { signed: true, .. })) => {
                panic!(
                    "Cannot cast from signed type '{}' to unsigned type '{}'",
                    operand_type, cast_type
                )
            }
            (Type::Int(_), Type::Int(_)) => (), // valid cast
            _ => panic!(
                "Cannot cast from type '{}' to type '{}'",
                operand_type, cast_type
            ),
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
    use super::*;
    use crate::ast::*;

    #[test]
    #[should_panic(expected = "No main function defined")]
    fn missing_main_function() {
        // pub fn not_main() i32 {
        // }

        let program = Program {
            global_statements: vec![GlobalStatement::FuncDef(FuncDef {
                proto: FuncProto {
                    func_visibility: FuncVisibility::Public,
                    name: "not_main".to_string(),
                    params: vec![],
                    return_type: Box::new(Type::Int(IntType { width: 32, signed: true })),
                },
                body: vec![],
            })],
        };

        let mut typer = Typer::new();
        let _ = typer.type_program(&program);
    }

    #[test]
    #[should_panic(expected = "The 'main' function should return a u8")]
    fn invalid_main_ret_type() {
        // pub fn main() i32 {
        // }

        let program = Program {
            global_statements: vec![GlobalStatement::FuncDef(FuncDef {
                proto: FuncProto {
                    func_visibility: FuncVisibility::Public,
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Box::new(Type::Int(IntType { width: 32, signed: true })),
                },
                body: vec![],
            })],
        };

        let mut typer = Typer::new();
        let _ = typer.type_program(&program);
    }

    #[test]
    #[should_panic(expected = "The 'main' function should not accept any parameters")]
    fn invalid_main_params() {
        // pub fn main(i32 a) i32 {
        // }

        let program = Program {
            global_statements: vec![GlobalStatement::FuncDef(FuncDef {
                proto: FuncProto {
                    func_visibility: FuncVisibility::Public,
                    name: "main".to_string(),
                    params: vec![
                        FuncParam {
                            param_type: Type::Int(IntType { width: 32, signed: true }),
                            param_name: "a".to_string()
                        }
                    ],
                    return_type: Box::new(Type::Int(IntType { width: 32, signed: true })),
                },
                body: vec![],
            })],
        };

        let mut typer = Typer::new();
        let _ = typer.type_program(&program);
    }

    #[test]
    #[should_panic(expected = "Identifier 'b' of type 'i64' cannot be used as type 'i32'")]
    fn assignment_with_mismatched_types() {
        // pub fn main() i32 {
        //     i64 a = 3
        //     i64 b = a
        //     i32 c = b  // should error
        // }

        let program = Program {
            global_statements: vec![GlobalStatement::FuncDef(FuncDef {
                proto: FuncProto {
                    func_visibility: FuncVisibility::Public,
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Box::new(Type::Int(IntType { width: 32, signed: true })),
                },
                body: vec![
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "a".to_string(),
                        var_value: Expr::IntLiteral(IntLiteral { negative: false, value: "3".to_string() }),
                        var_type: Type::Int(IntType { signed: true, width: 64 }),
                    }),
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "b".to_string(),
                        var_value: Expr::Identifier("a".to_string()),
                        var_type: Type::Int(IntType { signed: true, width: 64 }),
                    }),
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "c".to_string(),
                        var_value: Expr::Identifier("b".to_string()), // this should panic, since b (i64) can't be in c (i32)
                        var_type: Type::Int(IntType { width: 32, signed: true }),
                    }),
                ],
            })],
        };

        let mut typer = Typer::new();
        let _ = typer.type_program(&program);
    }

    #[test]
    fn valid_assignments() {
        // pub fn main() u8 {
        //     u8 a = 3
        //     u8 b = a
        //     ret b
        // }

        let program = Program {
            global_statements: vec![GlobalStatement::FuncDef(FuncDef {
                proto: FuncProto {
                    func_visibility: FuncVisibility::Public,
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Box::new(Type::Int(IntType { width: 8, signed: false })),
                },
                body: vec![
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "a".to_string(),
                        var_value: Expr::IntLiteral(IntLiteral { negative: false, value: "3".to_string() }),
                        var_type: Type::Int(IntType { width: 8, signed: false }),
                    }),
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "b".to_string(),
                        var_value: Expr::Identifier("a".to_string()),
                        var_type: Type::Int(IntType { width: 8, signed: false }),
                    }),
                    Statement::Return(Some(Expr::Identifier("b".to_string()))),
                ],
            })],
        };

        let expected_typed_program = TypedProgram {
            global_statements: vec![TypedGlobalStatement::FuncDef(TypedFuncDef {
                proto: FuncProto {
                    func_visibility: FuncVisibility::Public,
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Box::new(Type::Int(IntType { width: 8, signed: false })),
                },
                body: vec![
                    TypedStatement::VarDeclaration(TypedVarDeclaration {
                        var_name: "a".to_string(),
                        var_value: TypedExpr::IntLiteral(TypedIntLiteral {
                            negative: false,
                            int_value: "3".to_string(),
                            int_type: IntType { width: 8, signed: false },
                        }),
                        var_type: Type::Int(IntType { width: 8, signed: false }),
                    }),
                    TypedStatement::VarDeclaration(TypedVarDeclaration {
                        var_name: "b".to_string(),
                        var_type: Type::Int(IntType { width: 8, signed: false }),
                        var_value: TypedExpr::Identifier(TypedIdentifier {
                            name: "a".to_string(),
                            id_type: Type::Int(IntType { width: 8, signed: false }),
                        }),
                    }),
                    TypedStatement::Return(Some(TypedExpr::Identifier(TypedIdentifier {
                        name: "b".to_string(),
                        id_type: Type::Int(IntType { width: 8, signed: false }),
                    }))),
                ],
            })],
        };

        let mut typer = Typer::new();
        let actual_typed_program = typer.type_program(&program);
        assert_eq!(expected_typed_program, actual_typed_program);
    }

    #[test]
    #[should_panic(expected = "Cannot cast from signed type 'i32' to unsigned type 'u8'")]
    fn cast_signed_to_unsigned() {
        // pub fn main() u8 {
        //     i32 a = 3
        //     ret (u8) a
        // }

        let program = Program {
            global_statements: vec![GlobalStatement::FuncDef(FuncDef {
                proto: FuncProto {
                    func_visibility: FuncVisibility::Public,
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Box::new(Type::Int(IntType { width: 8, signed: false })),
                },
                body: vec![
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "a".to_string(),
                        var_value: Expr::IntLiteral(IntLiteral {
                            negative: false,
                            value: "3".to_string(),
                        }),
                        var_type: Type::Int(IntType { width: 32, signed: true }),
                    }),
                    Statement::Return(Some(Expr::Unary(Unary {
                        operator: UnaryOperator::Cast(Type::Int(IntType { width: 8, signed: false })),
                        operand: Box::new(Expr::Identifier("a".to_string())),
                    }))),
                ],
            })],
        };

        let mut typer = Typer::new();
        let _ = typer.type_program(&program);  // should panic
    }

    #[test]
    fn cast_u32_to_u8() {
        // pub fn main() u8 {
        //     u32 a = 3
        //     ret (u8) a
        // }

        let program = Program {
            global_statements: vec![GlobalStatement::FuncDef(FuncDef {
                proto: FuncProto {
                    func_visibility: FuncVisibility::Public,
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Box::new(Type::Int(IntType { width: 8, signed: false })),
                },
                body: vec![
                    Statement::VarDeclaration(VarDeclaration {
                        var_name: "a".to_string(),
                        var_value: Expr::IntLiteral(IntLiteral {
                            negative: false,
                            value: "3".to_string(),
                        }),
                        var_type: Type::Int(IntType { width: 32, signed: false }),
                    }),
                    Statement::Return(Some(Expr::Unary(Unary {
                        operator: UnaryOperator::Cast(Type::Int(IntType { width: 8, signed: false })),
                        operand: Box::new(Expr::Identifier("a".to_string())),
                    }))),
                ],
            })],
        };

        let expected_typed_program = TypedProgram {
            global_statements: vec![TypedGlobalStatement::FuncDef(TypedFuncDef {
                proto: FuncProto {
                    func_visibility: FuncVisibility::Public,
                    name: "main".to_string(),
                    params: vec![],
                    return_type: Box::new(Type::Int(IntType { width: 8, signed: false })),
                },
                body: vec![
                    TypedStatement::VarDeclaration(TypedVarDeclaration {
                        var_name: "a".to_string(),
                        var_value: TypedExpr::IntLiteral(TypedIntLiteral {
                            negative: false,
                            int_value: "3".to_string(),
                            int_type: IntType { width: 32, signed: false },
                        }),
                        var_type: Type::Int(IntType { width: 32, signed: false }),
                    }),
                    TypedStatement::Return(Some(TypedExpr::Unary(TypedUnary {
                        operator: UnaryOperator::Cast(Type::Int(IntType { width: 8, signed: false })),
                        operand: Box::new(TypedExpr::Identifier(TypedIdentifier {
                            name: "a".to_string(),
                            id_type: Type::Int(IntType { width: 32, signed: false }),
                        })),
                        result_type: Type::Int(IntType { width: 8, signed: false }),
                    }))),
                ],
            })],
        };

        let mut typer = Typer::new();
        let actual_typed_program = typer.type_program(&program);
        assert_eq!(expected_typed_program, actual_typed_program);
    }
}
