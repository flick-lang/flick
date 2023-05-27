use std::collections::HashMap;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::ffi::CString;

use crate::ast::Statement;

pub struct Compiler<'a> {
    statements: &'a [Statement],
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    named_values: HashMap<String, ValueRef>,
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

    pub fn codegen(&mut self) {
        for statement in self.statements {
            codegen_statement(statement);
        }
    }

    pub fn codegen_statement(&self, statement: Statement) {
        match statement {
            s @ Statement::VarDeclaration { .. } => self.codegen_var_declaration(s),
            s @ Statement::WhileLoop { .. } => self.codegen_while_loop(s),
            s @ Statement::Expr(_) => self.codegen_expr(s),
        }
    }
}
