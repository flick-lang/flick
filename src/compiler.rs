use std::collections::HashMap;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use core::ffi::{c_uint, c_ulonglong};
use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction;
use llvm_sys::analysis::LLVMVerifyFunction;
use llvm_sys::debuginfo::LLVMDIBuilderCreateFunction;
use std::ffi::CString;

use crate::ast::{Expr, FuncDef, Statement, VarDeclaration, WhileLoop};
use crate::token::Type;

pub struct Compiler {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    named_values: HashMap<String, LLVMValueRef>,
}

impl Compiler {
    pub fn new() -> Self {
        unsafe {
            let context = LLVMContextCreate();
            let module_name = CString::new("global_mod").unwrap(); // todo name of module
            let module = LLVMModuleCreateWithNameInContext(module_name.as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);
            let named_values = HashMap::new();

            Self {
                context,
                module,
                builder,
                named_values,
            }
        }
    }

    pub fn codegen(&mut self, statements: &[Statement]) {
        for statement in statements {
            self.codegen_statement(statement);
        }
    }

    pub fn print(&self) {
        unsafe {
            LLVMDumpModule(self.module);
        }
    }

    fn codegen_statement(&mut self, statement: &Statement) -> LLVMValueRef {
        match statement {
            Statement::VarDeclaration(v) => self.codegen_var_declaration(v),
            Statement::WhileLoop(w) => self.codegen_while_loop(w),
            Statement::Expr(e) => self.codegen_expr(e),
            Statement::FuncDef(f) => self.codegen_func_def(f),
        }
    }

    fn codegen_var_declaration(&self, var_declaration: &VarDeclaration) -> LLVMValueRef {
        unsafe {
            let var_name = CString::new(var_declaration.var_name.clone()).unwrap();
            let var_type = self.to_llvm_type(var_declaration.var_type);
            let alloca = LLVMBuildAlloca(self.builder, var_type, var_name.as_ptr());

            let var_value = self.codegen_expr(&var_declaration.var_value);
            LLVMBuildStore(self.builder, var_value, alloca)

            // TODO: what to return?

            //   // Remember the old variable binding so that we can restore the binding when
            //   // we unrecurse.
            //   OldBindings.push_back(NamedValues[VarName]);
            //
            //   // Remember this binding.
            //   NamedValues[VarName] = Alloca;
        }
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

    fn codegen_func_def(&mut self, func_def: &FuncDef) -> LLVMValueRef {
        unsafe {
            let return_type = self.to_llvm_type(func_def.return_type);

            let mut param_types = Vec::new();
            for param in func_def.params.iter() {
                param_types.push(self.to_llvm_type(param.param_type));
            }
            let function_type = LLVMFunctionType(
                return_type,
                param_types.as_mut_ptr(),
                param_types.len() as c_uint,
                LLVMBool::from(false),
            );

            let function_name = CString::new(func_def.name.clone()).unwrap();

            let function = LLVMAddFunction(self.module, function_name.as_ptr(), function_type);
            // TODO: 'function' might be null if LLVM fails to allocate -> result or smth to handle it?

            // todo correctly set names of function params
            for (i, p) in func_def.params.iter().enumerate() {
                let param = LLVMGetArgOperand(function, i as c_uint);
                // TODO: Write a better function for converting between rust strings and cstring pointers
                let param_name = CString::new(p.param_name.clone()).unwrap();
                LLVMSetValueName2(param, param_name.as_ptr(), p.param_name.len())
            }

            // TODO:
            //   if (!TheFunction->empty())
            //     return (Function*)LogErrorV("Function cannot be redefined.");

            let bb_name = b"entry\0".as_ptr() as *const _;
            let bb = LLVMAppendBasicBlockInContext(self.context, function, bb_name);
            LLVMPositionBuilderAtEnd(self.builder, bb);

            // TODO: Named value hashmap
            //   // Record the function arguments in the NamedValues map.
            //   NamedValues.clear();
            //   for (auto &Arg : TheFunction->args())
            //     NamedValues[std::string(Arg.getName())] = &Arg;

            // Codegen the body
            for (i, statement) in func_def.body.iter().enumerate() {
                // TODO: Multiline body of function
                let statement = self.codegen_statement(statement);

                let is_last_statement = i == func_def.body.len() - 1;
                if is_last_statement {
                    LLVMBuildRet(self.builder, statement);
                }
            }

            LLVMVerifyFunction(function, LLVMPrintMessageAction);

            function
        }
    }

    unsafe fn get_parent_function(&self) -> LLVMValueRef {
        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder))
    }

    unsafe fn to_llvm_type(&self, our_type: Type) -> LLVMTypeRef {
        match our_type {
            Type::I64 => LLVMIntTypeInContext(self.context, 64),
            Type::Void => LLVMVoidTypeInContext(self.context),
        }
    }
}

impl Drop for Compiler {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMContextDispose(self.context);
        }
    }
}
