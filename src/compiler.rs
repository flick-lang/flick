use std::collections::HashMap;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use core::ffi::{c_uint, c_ulonglong};
use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction;
use llvm_sys::analysis::LLVMVerifyFunction;
use llvm_sys::LLVMIntPredicate::*;
use std::ffi::CString;

use crate::ast::{BinExpr, BinaryOperator, Expr, FuncDef, Statement, VarDeclaration, WhileLoop};
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

    fn codegen_var_declaration(&mut self, var_declaration: &VarDeclaration) -> LLVMValueRef {
        unsafe {
            let var_name = CString::new(var_declaration.var_name.clone()).unwrap();
            let var_type = self.to_llvm_type(var_declaration.var_type);
            let alloca = LLVMBuildAlloca(self.builder, var_type, var_name.as_ptr());

            let var_value = self.codegen_expr(&var_declaration.var_value);
            LLVMBuildStore(self.builder, var_value, alloca);

            //   // Remember the old variable binding so that we can restore the binding when
            //   // we unrecurse.
            //   OldBindings.push_back(NamedValues[VarName]);

            self.named_values
                .insert(var_declaration.var_name.clone(), alloca); // TODO: Remove redundant clone

            // TODO: what to return?
            // let name = b"var_dec_load\0".as_ptr() as *const _;
            // LLVMBuildLoad2(self.builder, var_type, alloca, name)
            var_value
        }
    }

    fn codegen_while_loop(&self, while_loop: &WhileLoop) -> LLVMValueRef {
        todo!()
    }

    fn codegen_expr(&self, expr: &Expr) -> LLVMValueRef {
        match expr {
            Expr::Identifier(id) => self.codegen_identifier(id.as_str()),
            Expr::I64Literal(value) => self.codegen_int(*value),
            Expr::BinExpr(bin_expr) => self.codegen_bin_expr(bin_expr),
            Expr::CallExpr(_) => todo!(),
            Expr::IndexExpr(_) => todo!(),
        }
    }

    // TODO: Better/cleaner implementation of this function?
    fn codegen_identifier(&self, identifier: &str) -> LLVMValueRef {
        if !self.named_values.contains_key(identifier) {
            panic!("Variable not defined");
        }

        unsafe {
            let alloca = *self.named_values.get(identifier).unwrap();
            let name = b"ident_load\0".as_ptr() as *const _;
            LLVMBuildLoad2(self.builder, LLVMGetAllocatedType(alloca), alloca, name)
        }
    }

    fn codegen_int(&self, value: i64) -> LLVMValueRef {
        // Unsigned (u)longlong because we need underlying bit data to then sign extend
        unsafe {
            LLVMConstInt(
                LLVMIntTypeInContext(self.context, 64),
                value as c_ulonglong,
                // TODO: Make this a constant LLVMTrue and the opposite LLVMFalse
                LLVMBool::from(true),
            )
        }
    }

    fn codegen_bin_expr(&self, bin_expr: &BinExpr) -> LLVMValueRef {
        // NOTE: Dereference and then reference is necessary to get rid of Box
        let left = self.codegen_expr(&*bin_expr.left);
        let right = self.codegen_expr(&*bin_expr.right);

        // if (!L || !R)
        //     return nullptr;

        unsafe {
            match bin_expr.operator {
                BinaryOperator::Add => {
                    LLVMBuildAdd(self.builder, left, right, b"add\0".as_ptr() as *const _)
                }
                BinaryOperator::Subtract => {
                    LLVMBuildSub(self.builder, left, right, b"sub\0".as_ptr() as *const _)
                }
                BinaryOperator::Multiply => {
                    LLVMBuildMul(self.builder, left, right, b"mul\0".as_ptr() as *const _)
                }
                // TODO: Signed vs. unsigned division?
                BinaryOperator::Divide => {
                    LLVMBuildSDiv(self.builder, left, right, b"sdiv\0".as_ptr() as *const _)
                }

                // TODO: Not always integer compare and signed comparisons??
                BinaryOperator::NotEqualTo => LLVMBuildICmp(
                    self.builder,
                    LLVMIntNE,
                    left,
                    right,
                    b"neq\0".as_ptr() as *const _,
                ),
                BinaryOperator::EqualTo => LLVMBuildICmp(
                    self.builder,
                    LLVMIntEQ,
                    left,
                    right,
                    b"eq\0".as_ptr() as *const _,
                ),
                BinaryOperator::LessThan => LLVMBuildICmp(
                    self.builder,
                    LLVMIntSLT,
                    left,
                    right,
                    b"<\0".as_ptr() as *const _,
                ),
                BinaryOperator::GreaterThan => LLVMBuildICmp(
                    self.builder,
                    LLVMIntSGT,
                    left,
                    right,
                    b">\0".as_ptr() as *const _,
                ),
                BinaryOperator::LessOrEqualTo => LLVMBuildICmp(
                    self.builder,
                    LLVMIntSLE,
                    left,
                    right,
                    b"<=\0".as_ptr() as *const _,
                ),
                BinaryOperator::GreaterOrEqualTo => LLVMBuildICmp(
                    self.builder,
                    LLVMIntSLT,
                    left,
                    right,
                    b">=\0".as_ptr() as *const _,
                ),

                BinaryOperator::PlusEq => todo!(),
                BinaryOperator::TimesEq => todo!(),
                BinaryOperator::MinusEq => todo!(),
                BinaryOperator::DivideEq => todo!(),
                BinaryOperator::Assign => todo!(),
            }
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

            // Set names of all function params
            for (i, p) in func_def.params.iter().enumerate() {
                let param = LLVMGetParam(function, i as c_uint);

                // Set name of function parameters
                let param_name = CString::new(p.param_name.clone()).unwrap();
                LLVMSetValueName2(param, param_name.as_ptr(), p.param_name.len());
            }

            // TODO:
            //   if (!TheFunction->empty())
            //     return (Function*)LogErrorV("Function cannot be redefined.");

            let bb_name = b"entry\0".as_ptr() as *const _;
            let bb = LLVMAppendBasicBlockInContext(self.context, function, bb_name);
            LLVMPositionBuilderAtEnd(self.builder, bb);

            // Add argument to named_values hashmap
            self.named_values.clear();
            // TODO: Remove copied for loop?
            for (i, p) in func_def.params.iter().enumerate() {
                let param = LLVMGetParam(function, i as c_uint);

                // TODO: Remove redundant statements below
                let param_type = self.to_llvm_type(p.param_type);
                let param_name = CString::new(p.param_name.clone()).unwrap();

                let alloca = LLVMBuildAlloca(self.builder, param_type, param_name.as_ptr());
                LLVMBuildStore(self.builder, param, alloca);
                self.named_values.insert(p.param_name.clone(), alloca); // TODO: Remove redundant clone
            }

            // Codegen the body
            for (i, statement) in func_def.body.iter().enumerate() {
                // TODO: Multiline body of function
                let statement = self.codegen_statement(statement);

                let is_last_statement = i == func_def.body.len() - 1;
                if is_last_statement {
                    match func_def.return_type {
                        Type::Void => LLVMBuildRetVoid(self.builder),
                        _ => LLVMBuildRet(self.builder, statement),
                    };
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
