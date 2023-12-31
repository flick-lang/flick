use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction;
use llvm_sys::analysis::LLVMVerifyFunction;
use std::ffi::{c_char, c_uint, c_ulonglong, CStr, CString};
use std::mem::MaybeUninit;
use std::path::Path;

use llvm_sys::core::*;
use llvm_sys::error::LLVMGetErrorMessage;
use llvm_sys::prelude::*;
use llvm_sys::target::{
    LLVMSetModuleDataLayout, LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter,
    LLVM_InitializeNativeTarget,
};
use llvm_sys::target_machine::LLVMCodeGenFileType::LLVMObjectFile;
use llvm_sys::target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault;
use llvm_sys::target_machine::LLVMCodeModel::LLVMCodeModelDefault;
use llvm_sys::target_machine::LLVMRelocMode::LLVMRelocDefault;
use llvm_sys::target_machine::{
    LLVMCreateTargetDataLayout, LLVMCreateTargetMachine, LLVMDisposeTargetMachine,
    LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple, LLVMTarget, LLVMTargetMachineEmitToFile,
    LLVMTargetMachineRef,
};
use llvm_sys::transforms::pass_builder::*;
use llvm_sys::LLVMIntPredicate::*;
use llvm_sys::LLVMLinkage::{LLVMExternalLinkage, LLVMInternalLinkage};

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
    target_machine: LLVMTargetMachineRef,
    scope_manager: ScopeManager,
    pass_builder: LLVMPassBuilderOptionsRef,
}

impl Compiler {
    unsafe fn get_target_from_triple(triple: *const c_char) -> *mut LLVMTarget {
        let mut target = std::ptr::null_mut();
        let mut err_str = MaybeUninit::uninit();
        if LLVMGetTargetFromTriple(triple, &mut target, err_str.as_mut_ptr()) != 0 {
            panic!(
                "Error getting target from triple ({:?})",
                err_str.assume_init()
            );
        }
        target
    }

    pub fn new() -> Self {
        unsafe {
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(cstr!("module"), context);
            let builder = LLVMCreateBuilderInContext(context);
            let scope_manager = ScopeManager::new();

            // TODO: make this an option for the compiler to choose which target to initialize
            if LLVM_InitializeNativeTarget() == 1 {
                panic!("Error initializing native target")
            }
            if LLVM_InitializeNativeAsmParser() == 1 {
                panic!("Error initializing native ASM Parser")
            }
            if LLVM_InitializeNativeAsmPrinter() == 1 {
                panic!("Error initializing native ASM printer")
            }

            // Configure module
            let triple = LLVMGetDefaultTargetTriple(); // this computer's OS triple
            LLVMSetTarget(module, triple);

            let cpu = cstr!("generic");
            let features = cstr!("");
            let target = Self::get_target_from_triple(triple);
            let target_machine = LLVMCreateTargetMachine(
                target,
                triple,
                cpu,
                features,
                LLVMCodeGenLevelDefault,
                LLVMRelocDefault,
                LLVMCodeModelDefault,
            );

            let target_data_layout = LLVMCreateTargetDataLayout(target_machine);
            LLVMSetModuleDataLayout(module, target_data_layout);

            // Configure pass manager
            let pass_builder = LLVMCreatePassBuilderOptions();

            // LLVMPassBuilderOptionsSetCallGraphProfile(pass_builder, 1);
            // LLVMPassBuilderOptionsSetDebugLogging(pass_builder, 1);
            // LLVMPassBuilderOptionsSetForgetAllSCEVInLoopUnroll(pass_builder, 1);
            // LLVMPassBuilderOptionsSetInlinerThreshold(pass_builder, 75);
            // LLVMPassBuilderOptionsSetLicmMssaNoAccForPromotionCap(pass_builder, 1);
            // LLVMPassBuilderOptionsSetLicmMssaOptCap(pass_builder, 1);
            // LLVMPassBuilderOptionsSetLoopInterleaving(pass_builder, 1);
            // LLVMPassBuilderOptionsSetLoopUnrolling(pass_builder, 1);
            // LLVMPassBuilderOptionsSetLoopVectorization(pass_builder, 1);
            // LLVMPassBuilderOptionsSetMergeFunctions(pass_builder, 1);
            // LLVMPassBuilderOptionsSetSLPVectorization(pass_builder, 1);
            // LLVMPassBuilderOptionsSetVerifyEach(pass_builder, 1);

            Self {
                context,
                module,
                builder,
                target_machine,
                scope_manager,
                pass_builder,
            }
        }
    }

    pub fn print_ir(&self) {
        unsafe { LLVMDumpModule(self.module) }
    }

    pub fn optimize(&mut self) {
        unsafe {
            let passes = cstr!("default<O2>");
            let res = LLVMRunPasses(self.module, passes, self.target_machine, self.pass_builder);
            if !res.is_null() {
                let error_string = CStr::from_ptr(LLVMGetErrorMessage(res));
                panic!("Error running optimizations: {:?}", error_string);
            }
        }
    }

    pub fn to_file(&self, path: &impl AsRef<Path>) {
        unsafe {
            let mut path_cchars: Vec<_> = path
                .as_ref()
                .to_string_lossy()
                .chars()
                .map(|b| b as c_char)
                .collect();
            path_cchars.push(0); // Null character

            let mut err_str = MaybeUninit::uninit();
            let result = LLVMTargetMachineEmitToFile(
                self.target_machine,
                self.module,
                path_cchars.as_mut_ptr(),
                LLVMObjectFile,
                err_str.as_mut_ptr(),
            );

            if result == 1 {
                // TODO: Actually print error
                panic!("Error emitting object file ({:?})", err_str.assume_init());
            }
        }
    }

    pub fn compile(&mut self, program: &Program) {
        unsafe {
            for func_def in program.func_defs.iter() {
                self.compile_func_proto(&func_def.proto);
            }
            for func_def in program.func_defs.iter() {
                self.compile_func_def(func_def);
            }
        }
    }

    // TODO: Remove all as_str in favor of &
    unsafe fn compile_func_proto(&mut self, func_proto: &FuncProto) {
        if self.scope_manager.get_func(&func_proto.name).is_some() {
            panic!("Cannot redefine function '{}'", func_proto.name);
        }

        let return_type = self.to_llvm_type(func_proto.return_type);
        let num_params = func_proto.params.len() as c_uint;
        let mut param_types: Vec<_> = func_proto
            .params
            .iter()
            .map(|p| self.to_llvm_type(p.param_type))
            .collect();

        let func_type = LLVMFunctionType(return_type, param_types.as_mut_ptr(), num_params, 0);

        let func_name = CString::new(func_proto.name.as_str()).unwrap();
        let func = LLVMAddFunction(self.module, func_name.as_ptr(), func_type);

        if func.is_null() {
            panic!("Error defining function '{}'", func_proto.name);
        }

        for (i, param) in func_proto.params.iter().enumerate() {
            let param_value_ref = LLVMGetParam(func, i as c_uint);
            let param_name = CString::new(param.param_name.as_str()).unwrap();
            let param_name_len = param.param_name.len();
            LLVMSetValueName2(param_value_ref, param_name.as_ptr(), param_name_len);
        }

        // TODO: Remove variable shadowing
        let func_name = &func_proto.name;
        // TODO: Remove clone (like accept owned through function?)
        let func_proto = func_proto.clone();
        self.scope_manager.set_func(func_name, func_proto, func);
    }

    unsafe fn compile_func_def(&mut self, func_def: &FuncDef) {
        let func_name = CString::new(func_def.proto.name.as_str()).unwrap();
        let func = LLVMGetNamedFunction(self.module, func_name.as_ptr());
        if func.is_null() {
            panic!(
                "The prototype for function '{}' has not been defined",
                func_def.proto.name
            );
        }

        // TODO: Should linkage be set in the compile_func_proto function?
        match func_def.is_public {
            true => LLVMSetLinkage(func, LLVMExternalLinkage),
            false => LLVMSetLinkage(func, LLVMInternalLinkage),
        }

        let entry_block = LLVMAppendBasicBlockInContext(self.context, func, cstr!("entry"));
        LLVMPositionBuilderAtEnd(self.builder, entry_block);

        self.scope_manager.enter_scope();

        for (i, param) in func_def.proto.params.iter().enumerate() {
            let param_name = param.param_name.as_str();
            let param_type = param.param_type;
            let param_value_ref = LLVMGetParam(func, i as c_uint);
            let alloca = self.create_entry_block_alloca(func, param_name, param.param_type);
            LLVMBuildStore(self.builder, param_value_ref, alloca);
            self.scope_manager.set_var(param_name, param_type, alloca);
        }

        for statement in func_def.body.iter() {
            // TODO: Error if compiling statement fails?
            self.compile_statement(statement)
        }

        // TODO: Is this the best way of implicitly returning void?
        match func_def.body.last() {
            Some(Statement::Return(_)) => {}
            _ => self.compile_ret_statement(&None),
        }

        self.scope_manager.exit_scope();

        if LLVMVerifyFunction(func, LLVMPrintMessageAction) == 1 {
            panic!("Function '{}' is not valid", func_def.proto.name);
        }
    }

    unsafe fn compile_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::VarDeclarations(v) => self.compile_var_declarations(v),
            Statement::WhileLoop(w) => self.compile_while_loop(w),
            Statement::Expr(e) => self.compile_expr_statement(e),
            Statement::Return(r) => self.compile_ret_statement(r),
        }
    }

    unsafe fn to_llvm_type(&self, t: Type) -> LLVMTypeRef {
        match t {
            Type::I64 => LLVMIntTypeInContext(self.context, 64),
            Type::Void => LLVMVoidTypeInContext(self.context),
        }
    }

    fn compile_while_loop(&self, _while_loop: &WhileLoop) {
        todo!()
    }

    unsafe fn get_cur_function(&self) -> LLVMValueRef {
        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder))
    }

    unsafe fn compile_assignment(&mut self, assign: &Assign, expected_type: Type) -> LLVMValueRef {
        let value = self.compile_expr(assign.value.as_ref(), expected_type);
        let var = match self.scope_manager.get_var(&assign.name) {
            Some(var) => var,
            None => panic!("Setting a variable that has not been declared"),
        };

        if var.var_type != expected_type {
            panic!(
                "Assigning to variable '{}' with type '{}' but expected to have type '{}'",
                assign.name, var.var_type, expected_type
            );
        }

        let alloca = var.value;
        LLVMBuildStore(self.builder, value, alloca);
        value
    }

    unsafe fn compile_var_declarations(&mut self, var_declarations: &[VarDeclaration]) {
        let func = self.get_cur_function();
        for var_declaration in var_declarations {
            let var_name = var_declaration.var_name.as_str();
            let var_type = var_declaration.var_type;
            let alloca = self.create_entry_block_alloca(func, var_name, var_type);
            self.scope_manager.set_var(var_name, var_type, alloca);

            if let Some(value_expr) = &var_declaration.var_value {
                let value = self.compile_expr(value_expr, var_type);
                LLVMBuildStore(self.builder, value, alloca);
            }
        }
    }

    unsafe fn compile_expr_statement(&mut self, expr: &Expr) {
        let _ = self.compile_expr(expr, Type::Void);
    }

    unsafe fn compile_expr(&mut self, expr: &Expr, expected_type: Type) -> LLVMValueRef {
        match expr {
            Expr::Identifier(id) => self.compile_identifier(id, expected_type),
            Expr::I64Literal(x) => self.compile_i64_literal(*x, expected_type),
            Expr::Binary(bin_expr) => self.compile_bin_expr(bin_expr, expected_type),
            Expr::Call(call_expr) => self.compile_call_expr(call_expr, expected_type),
            Expr::Assign(assign) => self.compile_assignment(assign, expected_type),
        }
    }

    unsafe fn compile_ret_statement(&mut self, ret_value: &Option<Expr>) {
        let cur_func = self.get_cur_function();
        let mut len = MaybeUninit::uninit();
        let func_name_buf = LLVMGetValueName2(cur_func, len.as_mut_ptr());
        let func_name_cstr = CStr::from_ptr(func_name_buf);
        let func_name = func_name_cstr.to_str().unwrap();
        let func = match self.scope_manager.get_func(func_name) {
            Some(func) => func,
            None => panic!("Can't return when not in a function"),
        };
        let return_type = func.proto.return_type;

        // TODO once we set up type checking and once we can do
        // Expr::get_type(), we should make sure that ret_value matches
        // LLVMGETReturnType(self.cur_function())

        match ret_value {
            Some(expr) => LLVMBuildRet(self.builder, self.compile_expr(expr, return_type)),
            None => LLVMBuildRetVoid(self.builder),
        };
    }

    unsafe fn compile_identifier(&mut self, id: &str, expected_type: Type) -> LLVMValueRef {
        let var = match self.scope_manager.get_var(id) {
            Some(var) => var,
            None => panic!("Compiler error: undefined identifier '{}'", id),
        };

        if var.var_type != expected_type {
            panic!(
                "Identifier has type '{}' but expected '{}'",
                var.var_type, expected_type
            );
        }

        let alloca_ref = var.value;
        let alloca_type = LLVMGetAllocatedType(alloca_ref);
        let name = CString::new(id).unwrap();
        LLVMBuildLoad2(self.builder, alloca_type, alloca_ref, name.as_ptr())
    }

    unsafe fn compile_i64_literal(&self, x: i64, expected_type: Type) -> LLVMValueRef {
        // TODO: Only sign extend if its signed int
        LLVMConstInt(self.to_llvm_type(expected_type), x as c_ulonglong, 1)
    }

    unsafe fn compile_bin_expr(&mut self, bin_expr: &Binary, expected_type: Type) -> LLVMValueRef {
        use BinaryOperator::*;

        let lhs = self.compile_expr(&bin_expr.left, expected_type);
        let rhs = self.compile_expr(&bin_expr.right, expected_type);

        // TODO: Signed vs unsigned operations depending on expected_type
        match bin_expr.operator {
            Add => LLVMBuildAdd(self.builder, lhs, rhs, cstr!("add")),
            Subtract => LLVMBuildSub(self.builder, lhs, rhs, cstr!("sub")),
            Multiply => LLVMBuildMul(self.builder, lhs, rhs, cstr!("mul")),
            Divide => LLVMBuildSDiv(self.builder, lhs, rhs, cstr!("sdiv")),

            NotEqualTo => LLVMBuildICmp(self.builder, LLVMIntNE, lhs, rhs, cstr!("neq")),
            EqualTo => LLVMBuildICmp(self.builder, LLVMIntEQ, lhs, rhs, cstr!("eq")),
            LessThan => LLVMBuildICmp(self.builder, LLVMIntSLT, lhs, rhs, cstr!("slt")),
            GreaterThan => LLVMBuildICmp(self.builder, LLVMIntSGT, lhs, rhs, cstr!("sgt")),
            LessOrEqualTo => LLVMBuildICmp(self.builder, LLVMIntSLE, lhs, rhs, cstr!("sle")),
            GreaterOrEqualTo => LLVMBuildICmp(self.builder, LLVMIntSGE, lhs, rhs, cstr!("sge")),
        }
    }

    unsafe fn compile_call_expr(&mut self, call_expr: &Call, expected_type: Type) -> LLVMValueRef {
        let func = match self.scope_manager.get_func(&call_expr.function_name) {
            Some(func) => func.clone(), // TODO: Remove the clone
            None => panic!("Unknown function '{}' referenced", call_expr.function_name),
        };

        if func.proto.return_type != expected_type {
            panic!(
                "Function '{}' has return type '{}' but expected '{}'",
                func.proto.name, func.proto.return_type, expected_type
            );
        }

        let func_ref = func.value;

        let num_params = LLVMCountParams(func_ref) as usize;
        if num_params != call_expr.args.len() {
            panic!(
                "Incorrect # arguments passed to function '{}'",
                call_expr.function_name
            );
        }

        let mut arg_values = Vec::new();
        for i in 0..num_params {
            let param = func.proto.params.get(i).unwrap();
            let arg = call_expr.args.get(i).unwrap();

            let value = self.compile_expr(arg, param.param_type);

            if value.is_null() {
                panic!(
                    "Arg num '{}' when calling function '{}' was null",
                    i, call_expr.function_name
                );
            }

            arg_values.push(value);
        }

        let func_type = LLVMGlobalGetValueType(func_ref);
        LLVMBuildCall2(
            self.builder,
            func_type,
            func_ref,
            arg_values.as_mut_ptr(),
            arg_values.len() as c_uint,
            cstr!("call"),
        )
    }

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
            LLVMDisposePassBuilderOptions(self.pass_builder);
            LLVMDisposeTargetMachine(self.target_machine);
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}
