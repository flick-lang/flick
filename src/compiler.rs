use std::collections::HashMap;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use core::ffi::c_ulonglong;
use std::ffi::CString;

use crate::ast::{BinaryOperator, Expr, Statement, VarDeclaration, WhileLoop};
use crate::token::{Token, Type};

pub struct Compiler<'a> {
    statements: &'a [Statement],
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    named_values: HashMap<String, LLVMValueRef>,
}

impl<'a> Compiler<'a> {
    pub fn new(statements: &'a [Statement]) -> Self {
        let context = unsafe { LLVMContextCreate() };
        let module_name = CString::new("global_mod").unwrap(); // todo name of module
        let module = unsafe { LLVMModuleCreateWithNameInContext(module_name.as_ptr(), context) };
        let builder = unsafe { LLVMCreateBuilderInContext(context) };
        let named_values = HashMap::new();

        Self {
            statements,
            context,
            module,
            builder,
            named_values,
        }
    }

    pub fn codegen(&self) {
        for statement in self.statements {
            self.codegen_statement(statement);
        }
    }

    fn codegen_statement(&self, statement: &Statement) -> LLVMValueRef {
        // match statement {
        //     Statement::VarDeclaration(v) => self.codegen_var_declaration(v),
        //     Statement::WhileLoop(w) => self.codegen_while_loop(w),
        //     Statement::Expr(e) => self.codegen_expr(e),
        // }
        todo!()
    }

    fn codegen_var_declaration(&self, var_declaration: &VarDeclaration) -> LLVMValueRef {
        todo!()
    }

    fn codegen_while_loop(&self, while_loop: &WhileLoop) -> LLVMValueRef {
        todo!()
    }

    fn codegen_expr(&self, expr: &Expr) -> LLVMValueRef {
        match expr {
            Expr::Identifier(id) => self.codegen_identifier(id.as_str()),
            Expr::I64Literal(value) => self.codegen_int(*value),
            Expr::BinExpr(_) => todo!(),
            Expr::CallExpr(_) => todo!(),
            Expr::IndexExpr(_) => todo!(),
        }
    }

    fn codegen_identifier(&self, identifier: &str) -> LLVMValueRef {
        todo!()
    }

    fn codegen_int(&self, value: i64) -> LLVMValueRef {
        // Unsigned (u)longlong because we need underlying bit data to then sign extend
        unsafe {
            LLVMConstInt(
                LLVMIntTypeInContext(self.context, 64),
                value as c_ulonglong,
                LLVMBool::from(true),
            )
        }
    }
}

impl<'a> Drop for Compiler<'a> {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.context);
        }
    }
}
