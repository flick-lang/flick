use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction;
use llvm_sys::analysis::LLVMVerifyFunction;
use std::ffi::{c_uint, c_ulonglong, CString};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMIntPredicate::*;

use crate::compiler::scope_manager::ScopeManager;
use crate::lexer::token::Type;
use crate::parser::ast::*;

macro_rules! cstr {
    ($str_literal:expr) => {
        concat!($str_literal, "\0").as_ptr() as *const _
    };
}

pub struct Compiler {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    scope_manager: ScopeManager,
}

impl Compiler {
    pub fn new() -> Self {
        unsafe {
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(cstr!("module"), context);
            let builder = LLVMCreateBuilderInContext(context);
            let named_value_manager = ScopeManager::new();

            Self {
                context,
                module,
                builder,
                scope_manager: named_value_manager,
            }
        }
    }

    pub fn print_ir(&self) {
        unsafe { LLVMDumpModule(self.module) }
    }

    pub fn compile(&mut self, program: &Program) {
        unsafe {
            for func_def in program.func_defs.iter() {
                self.compile_func_def(func_def);
            }
        }
    }

    unsafe fn compile_func_def(&mut self, func_def: &FuncDef) {
        let func_name = CString::new(func_def.name.as_str()).unwrap();
        let func = LLVMGetNamedFunction(self.module, func_name.as_ptr());
        if !func.is_null() {
            panic!("Cannot redefine function '{}'", func_def.name);
        }

        // function does not yet exist, let's generate it:

        let return_type = self.to_llvm_type(func_def.return_type);
        let num_params = func_def.params.len() as c_uint;
        let mut param_types: Vec<_> = func_def
            .params
            .iter()
            .map(|p| self.to_llvm_type(p.param_type))
            .collect();

        let func_type = LLVMFunctionType(return_type, param_types.as_mut_ptr(), num_params, 0);

        let func = LLVMAddFunction(self.module, func_name.as_ptr(), func_type);

        if func.is_null() {
            panic!("Error defining function '{}'", func_def.name);
        }

        for (i, param) in func_def.params.iter().enumerate() {
            let param_value_ref = LLVMGetParam(func, i as c_uint);
            let param_name = CString::new(param.param_name.as_str()).unwrap();
            let param_name_len = param.param_name.len();
            LLVMSetValueName2(param_value_ref, param_name.as_ptr(), param_name_len);
        }

        let entry_block = LLVMAppendBasicBlockInContext(self.context, func, cstr!("entry"));
        LLVMPositionBuilderAtEnd(self.builder, entry_block);

        self.scope_manager.enter_scope();

        for (i, param) in func_def.params.iter().enumerate() {
            let param_name = param.param_name.as_str();
            let param_value_ref = LLVMGetParam(func, i as c_uint);
            let alloca = self.create_entry_block_alloca(func, param_name, param.param_type);
            LLVMBuildStore(self.builder, param_value_ref, alloca);
            self.scope_manager.set_value(param_name, alloca);
        }

        for statement in func_def.body.iter() {
            // TODO: Error if compiling statement fails?
            self.compile_statement(statement)
        }

        self.scope_manager.exit_scope();

        LLVMVerifyFunction(func, LLVMPrintMessageAction);
    }

    unsafe fn compile_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::VarDeclaration(v) => self.compile_var_declaration(v),
            Statement::WhileLoop(w) => self.compile_while_loop(w),
            Statement::ExprStatement(e) => self.compile_expr_statement(e),
            Statement::ReturnStatement(r) => self.compile_ret_statement(r),
        }
    }

    unsafe fn to_llvm_type(&self, t: Type) -> LLVMTypeRef {
        match t {
            Type::I64 => LLVMIntTypeInContext(self.context, 64),
            Type::Void => LLVMVoidTypeInContext(self.context),
        }
    }

    fn compile_while_loop(&self, while_loop: &WhileLoop) {
        todo!()
    }

    unsafe fn get_cur_function(&self) -> LLVMValueRef {
        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder))
    }

    unsafe fn compile_assignment(&mut self, lhs: &Expr, rhs: &Expr) -> LLVMValueRef {
        let var_name = match lhs {
            Expr::Identifier(id) => id,
            _ => panic!("Setting non-variables isn't supported"),
        };

        let value = self.compile_expr(rhs);
        let alloca = match self.scope_manager.get_value(var_name) {
            Some(alloca) => alloca,
            None => panic!("Setting a variable that has not been declared"),
        };
        LLVMBuildStore(self.builder, value, alloca);
        value
    }

    unsafe fn compile_var_declaration(&mut self, var_declaration: &VarDeclaration) {
        let func = self.get_cur_function();
        let var_name = var_declaration.var_name.as_str();
        let var_type = var_declaration.var_type;
        let alloca = self.create_entry_block_alloca(func, var_name, var_type);
        self.scope_manager.set_value(var_name, alloca);

        if let Some(value_expr) = &var_declaration.var_value {
            let value = self.compile_expr(value_expr);
            LLVMBuildStore(self.builder, value, alloca);
        }
    }

    unsafe fn compile_expr_statement(&mut self, expr: &Expr) {
        let _ = self.compile_expr(expr);
    }

    unsafe fn compile_expr(&mut self, expr: &Expr) -> LLVMValueRef {
        match expr {
            Expr::Identifier(id) => self.compile_identifier(id),
            Expr::I64Literal(x) => self.compile_i64_literal(*x),
            Expr::BinExpr(bin_expr) => self.compile_bin_expr(bin_expr),
            Expr::CallExpr(call_expr) => self.compile_call_expr(call_expr),
            // Expr::IndexExpr(index_expr) => self.compile_index_expr(index_expr),
        }
    }

    unsafe fn compile_ret_statement(&mut self, ret_value: &Option<Expr>) {
        match ret_value {
            Some(expr) => LLVMBuildRet(self.builder, self.compile_expr(expr)),
            None => LLVMBuildRetVoid(self.builder),
        };
    }

    unsafe fn compile_identifier(&mut self, id: &str) -> LLVMValueRef {
        let alloca_ref = match self.scope_manager.get_value(id) {
            Some(alloca_ref) => alloca_ref,
            None => panic!("Compiler error: undefined identifier '{}'", id),
        };
        let alloca_type = LLVMGetAllocatedType(alloca_ref);
        let name = CString::new(id).unwrap();
        LLVMBuildLoad2(self.builder, alloca_type, alloca_ref, name.as_ptr())
    }

    unsafe fn compile_i64_literal(&self, x: i64) -> LLVMValueRef {
        LLVMConstInt(self.to_llvm_type(Type::I64), x as c_ulonglong, 1)
    }

    unsafe fn compile_bin_expr(&mut self, bin_expr: &BinExpr) -> LLVMValueRef {
        use BinaryOperator::*;

        // TODO: Rework this
        if bin_expr.operator == Assign {
            return self.compile_assignment(&bin_expr.left, &bin_expr.right);
        }

        let lhs = self.compile_expr(&bin_expr.left);
        let rhs = self.compile_expr(&bin_expr.right);

        match bin_expr.operator {
            Add => LLVMBuildAdd(self.builder, lhs, rhs, cstr!("add")),
            Subtract => LLVMBuildSub(self.builder, lhs, rhs, cstr!("sub")),
            Multiply => LLVMBuildMul(self.builder, lhs, rhs, cstr!("mul")),
            Divide => LLVMBuildSDiv(self.builder, lhs, rhs, cstr!("div")),

            NotEqualTo => LLVMBuildICmp(self.builder, LLVMIntNE, lhs, rhs, cstr!("neq")),
            EqualTo => LLVMBuildICmp(self.builder, LLVMIntEQ, lhs, rhs, cstr!("eq")),
            LessThan => LLVMBuildICmp(self.builder, LLVMIntSLT, lhs, rhs, cstr!("lt")),
            GreaterThan => LLVMBuildICmp(self.builder, LLVMIntSGT, lhs, rhs, cstr!("gt")),
            LessOrEqualTo => LLVMBuildICmp(self.builder, LLVMIntSLE, lhs, rhs, cstr!("lte")),
            GreaterOrEqualTo => LLVMBuildICmp(self.builder, LLVMIntSGE, lhs, rhs, cstr!("gte")),

            Assign => LLVMBuildStore(self.builder, rhs, lhs),
        }
    }

    unsafe fn compile_call_expr(&mut self, call_expr: &CallExpr) -> LLVMValueRef {
        let func_name = CString::new(call_expr.function_name.as_str()).unwrap();
        let func = LLVMGetNamedFunction(self.module, func_name.as_ptr());

        if func.is_null() {
            panic!("Unknown function '{}' referenced", call_expr.function_name);
        }

        let num_args = LLVMCountParams(func) as usize;
        if num_args != call_expr.args.len() {
            panic!(
                "Incorrect # arguments passed to function '{}'",
                call_expr.function_name
            );
        }

        // todo compare arg and param types!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // todo compare arg and param types!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // todo compare arg and param types!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // todo compare arg and param types!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // todo compare arg and param types!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // todo compare arg and param types!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // todo compare arg and param types!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // todo compare arg and param types!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        let mut arg_values: Vec<_> = call_expr
            .args
            .iter()
            .map(|expr| self.compile_expr(expr))
            .collect();

        // TODO: Add more useful error message on info abt WHAT argument is null
        if arg_values.iter().any(|value_ref| value_ref.is_null()) {
            panic!(
                "One of the arguments to function '{}' was null",
                call_expr.function_name
            )
        }

        let func_type = LLVMGlobalGetValueType(func);
        LLVMBuildCall2(
            self.builder,
            func_type,
            func,
            arg_values.as_mut_ptr(),
            arg_values.len() as c_uint,
            cstr!("call"),
        )
    }

    // fn compile_index_expr(&self, index_expr: &IndexExpr) -> LLVMValueRef {
    //     todo!()
    // }

    // TODO: Have this function get_cur_function instead of taking it as an argument
    unsafe fn create_entry_block_alloca(
        &self,
        func: LLVMValueRef,
        var_name: &str,
        var_type: Type,
    ) -> LLVMValueRef {
        // todo  reposition self.builder instead of creating temp builder
        let temp_builder = LLVMCreateBuilderInContext(self.context);
        let entry_block = LLVMGetEntryBasicBlock(func);
        LLVMPositionBuilderAtEnd(temp_builder, entry_block);
        let name = CString::new(var_name).unwrap();
        let alloca = LLVMBuildAlloca(temp_builder, self.to_llvm_type(var_type), name.as_ptr());
        LLVMDisposeBuilder(temp_builder);
        alloca
    }
}

impl Drop for Compiler {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}
