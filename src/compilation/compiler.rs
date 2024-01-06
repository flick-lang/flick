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

use crate::compilation::scope_manager::ScopeManager;
use crate::lexing::token::Type;
use crate::parsing::ast::*;

/// Converts a `&str`, like `"hi"`, into a pointer to a null-terminated C-style str.
macro_rules! cstr {
    ($str_literal:expr) => {
        concat!($str_literal, "\0").as_ptr() as *const _
    };
}

/// A struct that takes an [abstract syntax tree][a] and converts it into LLVM code.
///
/// # Example usage
///
/// ```
/// # use flick::{ast, Compiler};
/// let mut compiler = Compiler::new();
/// let syntax_tree = ast::Program {
///     // generated during parsing
///     # func_defs: vec![]
/// } ;
/// compiler.compile(&syntax_tree);
/// compiler.optimize();
/// compiler.print_ir();  // or compiler.to_file("out")
/// ```
///
/// [a]: crate::parsing::ast
pub struct Compiler {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    target_machine: LLVMTargetMachineRef,
    scope_manager: ScopeManager,
    pass_builder: LLVMPassBuilderOptionsRef,
}

impl Compiler {
    /// Converts a string like `x86_64-unknown-freebsd` into the corresponding [LLVMTarget].
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

    /// Creates a new instance, setting up relevant llvm-sys boilerplate.
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

            // TODO(tbreydo): outdated: remove this stuff?

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

    /// This function prints the LLVM IR generated so far (via methods like [compile][a]).
    ///
    /// [a]: Compiler::compile
    pub fn print_ir(&self) {
        unsafe { LLVMDumpModule(self.module) }
    }

    /// This function optimizes the LLVM IR generated so far (via methods like [compile][a]).
    ///
    /// [a]: Compiler::compile
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

    /// This function dumps to a file the LLVM IR generated so far (via methods like [compile][a]).
    ///
    /// [a]: Compiler::compile
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

    /// This function compiles the provided program; once compiled, its LLVM IR can be [optimized][a],
    /// [printed][b], or [written to a file][c].
    ///
    /// The general idea is that `LLVMValueRef` instances are created recursively for various expressions,
    /// function calls, and function definitions. See implementation for details.
    ///
    /// [a]: Compiler::optimize
    /// [b]: Compiler::print_ir
    /// [c]: Compiler::to_file
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

    /// Registers a function prototype, panicking if the function has already been defined.
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

        // TODO: Design: Remove variable shadowing
        //  By that, do you think we meant allow variables to shadow functions inside a scope?
        //  If so, I think we do that...? Not sure.
        let func_name = &func_proto.name;
        // TODO: Minor: Remove clone (like accept owned through function?)
        let func_proto = func_proto.clone();
        self.scope_manager.set_func(func_name, func_proto, func);
    }

    /// Complies a function definition, assuming the function's prototype has been compiled.
    unsafe fn compile_func_def(&mut self, func_def: &FuncDef) {
        let func_name = CString::new(func_def.proto.name.as_str()).unwrap();
        let func = LLVMGetNamedFunction(self.module, func_name.as_ptr());
        if func.is_null() {
            panic!(
                "The prototype for function '{}' has not been defined",
                func_def.proto.name
            );
        }

        // TODO: QUICK: Should linkage be set in the compile_func_proto function?
        //  YES! IT SHOULD! Let's move is_public to be a part of func_def.proto.
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
            self.compile_statement(statement);
        }

        // TODO: minor: Is this the best way of implicitly returning void?
        //  It's correct. It might be redundant if the second-to-last statement
        //  was a return. But I think this is fine.
        // Implicitly return void if no return statement
        match func_def.body.last() {
            Some(Statement::Return(_)) => {}
            _ => self.compile_ret_statement(&None),
        }

        self.scope_manager.exit_scope();

        if LLVMVerifyFunction(func, LLVMPrintMessageAction) == 1 {
            panic!("Function '{}' is not valid", func_def.proto.name);
        }
    }

    /// Compiles a statement, assuming the LLVM builder is building inside a function body.
    unsafe fn compile_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::VarDeclarations(v) => self.compile_var_declarations(v),
            Statement::WhileLoop(w) => self.compile_while_loop(w),
            Statement::Assignment(a) => self.compile_assignment_statement(a),
            Statement::Return(r) => self.compile_ret_statement(r),
        }
    }

    /// Converts Flick's [Type] enum to llvm-sys's [LLVMTypeRef].
    unsafe fn to_llvm_type(&self, t: Type) -> LLVMTypeRef {
        match t {
            Type::Int { width } => LLVMIntTypeInContext(self.context, width),
            Type::Void => LLVMVoidTypeInContext(self.context),
        }
    }

    /// Compiles a while loop, assuming the LLVM builder is building inside a function body.
    unsafe fn compile_while_loop(&mut self, while_loop: &WhileLoop) {
        let cur_func = self.get_cur_function();
        let cond_block = LLVMAppendBasicBlockInContext(self.context, cur_func, cstr!("cond"));
        let loop_block = LLVMAppendBasicBlockInContext(self.context, cur_func, cstr!("loop"));
        // Build block to go to after loop is done executing
        let after_block = LLVMAppendBasicBlockInContext(self.context, cur_func, cstr!("after"));

        LLVMBuildBr(self.builder, cond_block);
        // todo: consider deleting?
        // Insert an explicit fall through from the current block to the loop_block.
        LLVMPositionBuilderAtEnd(self.builder, cond_block);

        // TODO: types: Should have type bool
        // Build a boolean LLVMValueRef with value 'condition != 0', which represents whether we should continue looping
        let condition = self.compile_expr(&while_loop.condition, Type::Int { width: 1 });
        LLVMBuildCondBr(self.builder, condition, loop_block, after_block);

        // Start insertion in loop_block.
        LLVMPositionBuilderAtEnd(self.builder, loop_block);

        self.scope_manager.enter_scope();
        for statement in while_loop.body.iter() {
            self.compile_statement(statement);
        }
        self.scope_manager.exit_scope();

        LLVMBuildBr(self.builder, cond_block);

        LLVMPositionBuilderAtEnd(self.builder, after_block);
    }

    /// Returns the function currently being built by the compiler.
    unsafe fn get_cur_function(&self) -> LLVMValueRef {
        let cur_func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
        if cur_func.is_null() {
            panic!("Builder is not inside a function; can't get current function.");
        }
        cur_func
    }

    /// Compiles 0 or more variable declarations.
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

    /// Compiles an assignment expression like `foo = 28` (and panics if `foo`'s type can't store 28).
    unsafe fn compile_assignment_statement(&mut self, assign: &Assignment) {
        let var = match self.scope_manager.get_var(&assign.name) {
            Some(var) => *var,
            None => panic!("Setting a variable that has not been declared"),
        };

        let value = self.compile_expr(assign.value.as_ref(), var.var_type);
        let alloca = var.value;
        LLVMBuildStore(self.builder, value, alloca);
    }

    /// Compiles an expression, panicking if its value's type doesn't match the expected type.
    ///
    /// Note: if `expected_type` is `None`, then no type-checking is performed (because the caller
    /// doesn't actually know what the type must be).
    unsafe fn compile_expr(&mut self, expr: &Expr, expected_type: Type) -> LLVMValueRef {
        match expr {
            Expr::Identifier(id) => self.compile_identifier(id, expected_type),
            Expr::I64Literal(x) => self.compile_int_literal(*x, expected_type),
            Expr::Binary(bin_expr) => self.compile_bin_expr(bin_expr, expected_type),
            Expr::Comparison(comparison) => self.compile_comparison_expr(comparison, expected_type),
            Expr::Call(call_expr) => self.compile_call_expr(call_expr, expected_type),
        }
    }

    /// Compiles a return statement, panicking if the builder isn't inside a function.
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
        //  Expr::get_type(), we should make sure that ret_value matches
        //  LLVMGETReturnType(self.cur_function())

        match ret_value {
            Some(expr) => LLVMBuildRet(self.builder, self.compile_expr(expr, return_type)),
            None => LLVMBuildRetVoid(self.builder),
        };
    }

    /// Compiles an identifier expression (variable value) with an expected type.
    unsafe fn compile_identifier(&mut self, id: &str, expected_type: Type) -> LLVMValueRef {
        let var = match self.scope_manager.get_var(id) {
            Some(var) => var,
            None => panic!("Compiler error: undefined identifier '{}'", id),
        };

        if var.var_type != expected_type {
            panic!(
                "Identifier '{}' has type '{}' but expected '{}'",
                id, var.var_type, expected_type
            );
        }

        let alloca_ref = var.value;
        let alloca_type = LLVMGetAllocatedType(alloca_ref);
        let name = CString::new(id).unwrap();
        LLVMBuildLoad2(self.builder, alloca_type, alloca_ref, name.as_ptr())
    }

    /// Compiles an integer literal expression.
    unsafe fn compile_int_literal(&self, x: i64, expected_type: Type) -> LLVMValueRef {
        // TODO: signed-ints: Only sign extend if its signed in

        match expected_type {
            t @ Type::Int { .. } => LLVMConstInt(self.to_llvm_type(t), x as c_ulonglong, 1),
            t => unreachable!("Unsupported int literal type '{}'", t),
        }
    }

    /// Compiles a binary expression (recursively compiling left- and right-hand sides).
    ///
    /// Note: if `expected_type` is `None`, then no type-checking is performed (because the caller
    /// doesn't actually know what the type must be).
    unsafe fn compile_bin_expr(&mut self, bin_expr: &Binary, expected_type: Type) -> LLVMValueRef {
        use BinaryOperator::*;

        // TODO: typing: binary expression should dictate types here
        let lhs = self.compile_expr(&bin_expr.left, expected_type);
        let rhs = self.compile_expr(&bin_expr.right, expected_type);

        // TODO: signed-ints: Signed vs unsigned operations depending on expected_type
        match bin_expr.operator {
            Add => LLVMBuildAdd(self.builder, lhs, rhs, cstr!("add")),
            Subtract => LLVMBuildSub(self.builder, lhs, rhs, cstr!("sub")),
            Multiply => LLVMBuildMul(self.builder, lhs, rhs, cstr!("mul")),
            Divide => LLVMBuildSDiv(self.builder, lhs, rhs, cstr!("sdiv")),
        }
    }

    /// Compiles a comparison expression.
    ///
    /// Note that this function accept an `expected_type`, only to confirm that it is `i1` (boolean),
    /// since comparison expressions.
    unsafe fn compile_comparison_expr(
        &mut self,
        comparison: &Comparison,
        expected_type: Type,
    ) -> LLVMValueRef {
        use ComparisonOperator::*;

        match expected_type {
            Type::Int { width: 1 } => {}
            t => panic!("Expected type '{}' but got comparison expression", t),
        }

        let lhs_type = self.get_expr_type(&comparison.left);
        let rhs_type = self.get_expr_type(&comparison.right);
        let expr_type = match (lhs_type, rhs_type) {
            (Type::Int { width: w1 }, Type::Int { width: w2 }) => Type::Int { width: w1.max(w2) },
            (t1, t2) => panic!("'{}' {} '{}' is not supported", t1, comparison.operator, t2),
        };

        let lhs = self.compile_expr(&comparison.left, expr_type);
        let rhs = self.compile_expr(&comparison.right, expr_type);

        match comparison.operator {
            NotEqualTo => LLVMBuildICmp(self.builder, LLVMIntNE, lhs, rhs, cstr!("neq")),
            EqualTo => LLVMBuildICmp(self.builder, LLVMIntEQ, lhs, rhs, cstr!("eq")),
            LessThan => LLVMBuildICmp(self.builder, LLVMIntSLT, lhs, rhs, cstr!("slt")),
            GreaterThan => LLVMBuildICmp(self.builder, LLVMIntSGT, lhs, rhs, cstr!("sgt")),
            LessOrEqualTo => LLVMBuildICmp(self.builder, LLVMIntSLE, lhs, rhs, cstr!("sle")),
            GreaterOrEqualTo => LLVMBuildICmp(self.builder, LLVMIntSGE, lhs, rhs, cstr!("sge")),
        }
    }

    /// Compiles a function call, ensuring its signature has the expected return type.
    ///
    /// Note: if `expected_type` is `None`, then no type-checking is performed (because the caller
    /// doesn't actually know what the type must be).
    unsafe fn compile_call_expr(&mut self, call_expr: &Call, expected_type: Type) -> LLVMValueRef {
        let func = match self.scope_manager.get_func(&call_expr.function_name) {
            Some(func) => func.clone(), // TODO: Remove the clone
            None => panic!("Unknown function '{}' referenced", call_expr.function_name),
        };

        if func.proto.return_type != expected_type {
            panic!(
                "Function '{}' has return type '{}' but returned a value of type '{}'",
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

    /// Creates an LLVM 'alloca', which can then be used to set up a local variable.
    unsafe fn create_entry_block_alloca(
        &self,
        func: LLVMValueRef,
        var_name: &str,
        var_type: Type,
    ) -> LLVMValueRef {
        // todo reposition self.builder instead of creating temp builder
        let temp_builder = LLVMCreateBuilderInContext(self.context);
        let entry_block = LLVMGetEntryBasicBlock(func);
        LLVMPositionBuilderAtEnd(temp_builder, entry_block);
        let name = CString::new(var_name).unwrap();
        let alloca = LLVMBuildAlloca(temp_builder, self.to_llvm_type(var_type), name.as_ptr());
        LLVMDisposeBuilder(temp_builder);
        alloca
    }

    /// Returns the smallest type that can store expression.
    unsafe fn get_expr_type(&self, expr: &Expr) -> Type {
        // TODO: style: figure out where our compiler stores/uses prototypes of operators
        //  Like, how do we use <(2, 3) -> bool is allowed and <("hello", hi") -> bool is too.
        match expr {
            Expr::Identifier(x) => match self.scope_manager.get_var(x) {
                Some(v) => v.var_type,
                None => panic!("Variable has to already have been declared"),
            },
            Expr::I64Literal(_) => Type::Int { width: 64 },
            Expr::Binary(b) => self.get_binary_expr_type(b),
            Expr::Comparison(_) => Type::Int { width: 1 },
            Expr::Call(f) => match self.scope_manager.get_func(&f.function_name) {
                Some(f) => f.proto.return_type,
                None => panic!("Function has to be declared before it can be called"),
            },
        }
    }

    /// Returns the smallest type that can store a binary expression.
    unsafe fn get_binary_expr_type(&self, bin_expr: &Binary) -> Type {
        let lhs_type = self.get_expr_type(&bin_expr.left);
        let rhs_type = self.get_expr_type(&bin_expr.right);

        match (lhs_type, rhs_type) {
            (Type::Int { width: w1 }, Type::Int { width: w2 }) => Type::Int { width: w1.max(w2) },
            (t1, t2) => panic!("'{}' {} '{}' is not supported", t1, bin_expr.operator, t2),
        }
    }
}

impl Drop for Compiler {
    /// Disposes the underlying llvm-sys C objects so that we don't leak memory.
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
