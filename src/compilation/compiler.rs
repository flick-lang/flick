use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction;
use llvm_sys::analysis::LLVMVerifyFunction;
use std::ffi::{c_char, c_uint, CStr, CString};
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

use crate::ast::*;
use crate::typed_ast::*;
use crate::types::FuncType;
use crate::ScopeManager;
use crate::Type;

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
/// # use flick::{typed_ast, Compiler};
/// let mut compiler = Compiler::new();
/// let syntax_tree = typed_ast::TypedProgram {
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
    scope_manager: ScopeManager<LLVMValueRef>,
    pass_builder: LLVMPassBuilderOptionsRef,
}

impl Compiler {
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
    pub fn compile(&mut self, program: &TypedProgram) {
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
        if self.scope_manager.get(&func_proto.name).is_some() {
            panic!("Cannot redefine '{}'", func_proto.name);
        }

        // TODO: Remove creating new box?
        let return_type = Box::new(func_proto.return_type.clone());
        let param_types: Vec<_> = func_proto
            .params
            .iter()
            .map(|p| p.param_type.clone())
            .collect();

        let func_type = Type::Func(FuncType {
            param_types,
            return_type,
        });

        let func_llvm_type = self.to_llvm_type(&func_type);
        let func_name = CString::new(func_proto.name.as_str()).unwrap();
        let func = LLVMAddFunction(self.module, func_name.as_ptr(), func_llvm_type);

        if LLVMIsNull(func) == 1 {
            panic!("Error defining function '{}'", func_proto.name);
        }

        for (i, param) in func_proto.params.iter().enumerate() {
            let param_value_ref = LLVMGetParam(func, i as c_uint);
            let param_name = CString::new(param.param_name.as_str()).unwrap();
            let param_name_len = param.param_name.len();
            LLVMSetValueName2(param_value_ref, param_name.as_ptr(), param_name_len);
        }

        match func_proto.is_public {
            true => LLVMSetLinkage(func, LLVMExternalLinkage),
            false => LLVMSetLinkage(func, LLVMInternalLinkage),
        }

        // TODO: Design: Remove variable shadowing
        //  By that, do you think we meant allow variables to shadow functions inside a scope?
        //  If so, I think we do that...? Not sure.
        let func_name = &func_proto.name;
        self.scope_manager.set(func_name, func);
    }

    /// Complies a function definition, assuming the function's prototype has been compiled.
    unsafe fn compile_func_def(&mut self, func_def: &TypedFuncDef) {
        let func_name = CString::new(func_def.proto.name.as_str()).unwrap();
        let func = LLVMGetNamedFunction(self.module, func_name.as_ptr());
        if LLVMIsNull(func) == 1 {
            panic!(
                "The prototype for function '{}' has not been defined",
                func_def.proto.name
            );
        }

        let entry_block = LLVMAppendBasicBlockInContext(self.context, func, cstr!("entry"));
        LLVMPositionBuilderAtEnd(self.builder, entry_block);

        self.scope_manager.enter_scope();

        for (i, param) in func_def.proto.params.iter().enumerate() {
            let param_name = param.param_name.as_str();
            let param_type = &param.param_type;
            let param_value_ref = LLVMGetParam(func, i as c_uint);
            let alloca = self.create_entry_block_alloca(func, param_name, param_type);
            LLVMBuildStore(self.builder, param_value_ref, alloca);
            self.scope_manager.set(param_name, alloca);
        }

        for statement in func_def.body.iter() {
            // TODO: Error if compiling statement fails?
            self.compile_statement(statement);
        }

        self.scope_manager.exit_scope();

        // TODO: Convert LLVM into Flick error by using LLVMReturnStatusAction
        if LLVMVerifyFunction(func, LLVMPrintMessageAction) == 1 {
            panic!("Function '{}' is not valid", func_def.proto.name);
        }
    }

    /// Compiles a statement, assuming the LLVM builder is building inside a function body.
    unsafe fn compile_statement(&mut self, statement: &TypedStatement) {
        match statement {
            TypedStatement::VarDeclaration(v) => self.compile_var_declaration(v),
            TypedStatement::WhileLoop(w) => self.compile_while_loop(w),
            TypedStatement::Assignment(a) => self.compile_assignment_statement(a),
            TypedStatement::Return(r) => self.compile_ret_statement(r),
        }
    }

    /// Compiles 0 or more variable declarations.
    unsafe fn compile_var_declaration(&mut self, var_declaration: &TypedVarDeclaration) {
        let func = self.get_cur_function();
        let var_name = var_declaration.var_name.as_str();
        let var_type = &var_declaration.var_type;
        let alloca = self.create_entry_block_alloca(func, var_name, var_type);

        self.scope_manager.set(var_name, alloca);

        let value = self.compile_expr(&var_declaration.var_value);
        LLVMBuildStore(self.builder, value, alloca);
    }

    /// Compiles a while loop, assuming the LLVM builder is building inside a function body.
    unsafe fn compile_while_loop(&mut self, while_loop: &TypedWhileLoop) {
        let cur_func = self.get_cur_function();
        let cond_block = LLVMAppendBasicBlockInContext(self.context, cur_func, cstr!("cond"));
        let loop_block = LLVMAppendBasicBlockInContext(self.context, cur_func, cstr!("loop"));
        // Build block to go to after loop is done executing
        let after_block = LLVMAppendBasicBlockInContext(self.context, cur_func, cstr!("after"));

        LLVMBuildBr(self.builder, cond_block);
        // todo: consider deleting?
        // Insert an explicit fall through from the current block to the loop_block.
        LLVMPositionBuilderAtEnd(self.builder, cond_block);

        let condition = self.compile_expr(&while_loop.condition);
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

    /// Compiles an assignment expression like `foo = 28` (and panics if `foo`'s type can't store 28).
    unsafe fn compile_assignment_statement(&mut self, assign: &TypedAssignment) {
        let alloca = match self.scope_manager.get(&assign.name) {
            Some(v) => *v,
            None => panic!("Setting a variable that has not been declared"),
        };

        // TODO: Maybe allow redefining the function?
        if !LLVMIsAFunction(alloca).is_null() {
            // TODO: Split this into a function
            let cur_func = self.get_cur_function();
            let mut len = MaybeUninit::uninit();
            let func_name_buf = LLVMGetValueName2(cur_func, len.as_mut_ptr());
            let func_name_cstr = CStr::from_ptr(func_name_buf);
            let func_name = func_name_cstr.to_str().unwrap();
            panic!("Cannot assign a value to function '{}'", func_name);
        }

        let value = self.compile_expr(&assign.value);
        LLVMBuildStore(self.builder, value, alloca);
    }

    /// Compiles a return statement, panicking if the builder isn't inside a function.
    unsafe fn compile_ret_statement(&mut self, ret_value: &Option<TypedExpr>) {
        // TODO: Ensure user is inside function (otherwise they shouldn't be able to return)

        match ret_value {
            Some(expr) => LLVMBuildRet(self.builder, self.compile_expr(expr)),
            None => LLVMBuildRetVoid(self.builder),
        };
    }

    /// Compiles an expression, panicking if its value's type doesn't match the expected type.
    ///
    /// Note: if `expected_type` is `None`, then no type-checking is performed (because the caller
    /// doesn't actually know what the type must be).
    unsafe fn compile_expr(&mut self, expr: &TypedExpr) -> LLVMValueRef {
        match expr {
            TypedExpr::Identifier(id) => self.compile_identifier(id),
            TypedExpr::IntLiteral(int_literal) => self.compile_int_literal(int_literal),
            TypedExpr::Binary(bin_expr) => self.compile_bin_expr(bin_expr),
            TypedExpr::Comparison(comparison) => self.compile_comparison_expr(comparison),
            TypedExpr::Call(call_expr) => self.compile_call_expr(call_expr),
        }
    }

    /// Compiles an identifier expression (variable value) with an expected type.
    unsafe fn compile_identifier(&mut self, id: &TypedIdentifier) -> LLVMValueRef {
        let alloca = match self.scope_manager.get(id.name.as_str()) {
            Some(v) => *v,
            None => panic!("Compiler error: undefined identifier '{}'", id.name),
        };

        let alloca_type = self.to_llvm_type(&id.id_type);
        let name = CString::new(id.name.as_str()).unwrap();
        LLVMBuildLoad2(self.builder, alloca_type, alloca, name.as_ptr())
    }

    /// Compiles an integer literal expression.
    unsafe fn compile_int_literal(&self, int_literal: &TypedIntLiteral) -> LLVMValueRef {
        let int_type = self.to_llvm_type(&Type::Int(int_literal.int_type));
        let value_cstr = CString::new(int_literal.int_value.as_str()).unwrap();
        LLVMConstIntOfString(int_type, value_cstr.as_ptr(), 10)
    }

    /// Compiles a binary expression (recursively compiling left- and right-hand sides).
    unsafe fn compile_bin_expr(&mut self, bin_expr: &TypedBinary) -> LLVMValueRef {
        use BinaryOperator::*;

        let lhs = self.compile_expr(&bin_expr.left);
        let rhs = self.compile_expr(&bin_expr.right);

        if LLVMTypeOf(lhs) != LLVMTypeOf(rhs) {
            panic!("Left and right hand side of binary expression do not have the same type")
        }

        match bin_expr.operator {
            Add => LLVMBuildAdd(self.builder, lhs, rhs, cstr!("add")),
            Subtract => LLVMBuildSub(self.builder, lhs, rhs, cstr!("sub")),
            Multiply => LLVMBuildMul(self.builder, lhs, rhs, cstr!("mul")),
            // TODO: signed-ints: Signed vs unsigned division
            Divide => LLVMBuildSDiv(self.builder, lhs, rhs, cstr!("sdiv")),
        }
    }

    /// Compiles a comparison expression.
    ///
    /// Note that this function accept an `expected_type`, only to confirm that it is `i1` (boolean),
    /// since comparison expressions.
    unsafe fn compile_comparison_expr(&mut self, comparison: &TypedComparison) -> LLVMValueRef {
        use ComparisonOperator::*;

        let lhs = self.compile_expr(&comparison.left);
        let rhs = self.compile_expr(&comparison.right);

        if LLVMTypeOf(lhs) != LLVMTypeOf(rhs) {
            panic!("Left and right hand side of binary expression do not have the same type")
        }

        // TODO: Signed comparisons
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
    unsafe fn compile_call_expr(&mut self, call_expr: &TypedCall) -> LLVMValueRef {
        let func = match self.scope_manager.get(&call_expr.function_name) {
            Some(v) => *v,
            None => unreachable!("Undefined functions should be handled by typer"),
        };

        if LLVMIsAFunction(func).is_null() {
            unreachable!(
                "Calls like foo() where foo isn't callable (e.g. i32) should be handled by typer"
            )
        }

        let num_params = call_expr.function_type.param_types.len();
        if num_params != call_expr.args.len() {
            unreachable!("Number of arguments should be handled by typer");
        }

        let mut arg_values = Vec::with_capacity(call_expr.args.len());
        for i in 0..num_params {
            let arg = call_expr.args.get(i).unwrap();
            let value = self.compile_expr(arg);
            arg_values.push(value);
        }

        let func_type = self.to_llvm_type(&Type::Func(call_expr.function_type.clone()));
        LLVMBuildCall2(
            self.builder,
            func_type,
            func,
            arg_values.as_mut_ptr(),
            arg_values.len() as c_uint,
            cstr!("call"),
        )
    }

    /// Converts Flick's [Type] enum to llvm-sys's [LLVMTypeRef].
    unsafe fn to_llvm_type(&self, t: &Type) -> LLVMTypeRef {
        match t {
            Type::Int(int_type) => LLVMIntTypeInContext(self.context, int_type.width),
            Type::Void => LLVMVoidTypeInContext(self.context),
            Type::Func(func_type) => {
                let return_type = self.to_llvm_type(func_type.return_type.as_ref());
                let num_params = func_type.param_types.len() as c_uint;
                let mut param_types: Vec<_> = func_type
                    .param_types
                    .iter()
                    .map(|p| self.to_llvm_type(p))
                    .collect();

                LLVMFunctionType(return_type, param_types.as_mut_ptr(), num_params, 0)
            }
        }
    }

    /// Creates an LLVM 'alloca', which can then be used to set up a local variable.
    unsafe fn create_entry_block_alloca(
        &self,
        func: LLVMValueRef,
        var_name: &str,
        var_type: &Type,
    ) -> LLVMValueRef {
        // todo reposition self.builder instead of creating temp builder
        let temp_builder = LLVMCreateBuilderInContext(self.context);
        let entry_block = LLVMGetEntryBasicBlock(func);
        LLVMPositionBuilderAtEnd(temp_builder, entry_block);
        // TODO: Error handling instead of unwrap
        let name = CString::new(var_name).unwrap();
        let alloca = LLVMBuildAlloca(temp_builder, self.to_llvm_type(var_type), name.as_ptr());
        LLVMDisposeBuilder(temp_builder);
        alloca
    }

    /// Returns the function currently being built by the compiler.
    unsafe fn get_cur_function(&self) -> LLVMValueRef {
        let cur_func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
        if LLVMIsNull(cur_func) == 1 {
            panic!("Builder is not inside a function; can't get current function.");
        }
        cur_func
    }

    //
    //
    //
    //
    //
    //
    //
    //
    //
    //
    //
    // /// Returns the smallest type that can store expression.
    // unsafe fn get_expr_type(&self, expr: &Expr) -> Type {
    //     // TODO: style: figure out where our compiler stores/uses prototypes of operators
    //     //  Like, how do we use <(2, 3) -> bool is allowed and <("hello", hi") -> bool is too.
    //     match expr {
    //         Expr::Identifier(x) => match self.scope_manager.get(x) {
    //             Some(ScopeManagerValue::Var(v)) => v.var_type,
    //             Some(ScopeManagerValue::Func(f)) => panic!("Function do not have a type yet"),
    //             None => panic!("Variable has to already have been declared"),
    //         },
    //         Expr::I64Literal(_) => Type::Int { width: 64 },
    //         Expr::Binary(b) => self.get_binary_expr_type(b),
    //         Expr::Comparison(_) => Type::Int { width: 1 },
    //         Expr::Call(f) => match self.scope_manager.get(&f.function_name) {
    //             Some(ScopeManagerValue::Func(f)) => f.proto.return_type,
    //             Some(ScopeManagerValue::Var(v)) => {
    //                 panic!("Unsupported: most likely this will be removed")
    //             }
    //             None => panic!("Function has to be declared before it can be called"),
    //         },
    //     }
    // }
    //
    // /// Returns the smallest type that can store a binary expression.
    // unsafe fn get_binary_expr_type(&self, bin_expr: &Binary) -> Type {
    //     let lhs_type = self.get_expr_type(&bin_expr.left);
    //     let rhs_type = self.get_expr_type(&bin_expr.right);
    //
    //     match (lhs_type, rhs_type) {
    //         (Type::Int { width: w1 }, Type::Int { width: w2 }) => Type::Int { width: w1.max(w2) },
    //         (t1, t2) => panic!("'{}' {} '{}' is not supported", t1, bin_expr.operator, t2),
    //     }
    // }
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

/// As suggested by Clippy's [new_without_default][a], since [Compiler::new()] doesn't
/// take any arguments, Compiler should implement Default.
///
/// [a]: https://rust-lang.github.io/rust-clippy/master/index.html#/new_without_default
impl Default for Compiler {
    fn default() -> Self {
        Compiler::new()
    }
}
