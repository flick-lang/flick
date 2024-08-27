use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction;
use llvm_sys::analysis::LLVMVerifyFunction;
use std::ffi::{c_char, c_uint, CStr, CString};
use std::mem::MaybeUninit;
use std::path::Path;

use llvm_sys::core::*;
use llvm_sys::LLVMIntPredicate;
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
use llvm_sys::LLVMLinkage::{LLVMExternalLinkage, LLVMInternalLinkage};

use crate::ast::*;
use crate::typed_ast::*;
use crate::types::IntType;
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
///     # global_statements: vec![]
/// };
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
            let passes = cstr!("default<O1>");
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
            self.scope_manager.enter_scope();
            for global_statement in program.global_statements.iter() {
                // TODO: In the future when we have additional global statements, maybe move this into a new function called 'preprocess_global_statement' or something like that
                match global_statement {
                    TypedGlobalStatement::Extern(p) => self.compile_func_proto(p),
                    TypedGlobalStatement::FuncDef(f) => self.compile_func_proto(&f.proto)
                }
            }
            for global_statement in program.global_statements.iter() {
                self.compile_global_statement(global_statement);
            }
            self.scope_manager.exit_scope()
        }
    }

    /// Compiles a function prototype (or a )
    unsafe fn compile_global_statement(&mut self, global_statement: &TypedGlobalStatement) {
        if let TypedGlobalStatement::FuncDef(func_def) = global_statement {
            self.compile_func_def(func_def);
        }
    }

    /// Registers a function prototype, panicking if the function has already been defined.
    unsafe fn compile_func_proto(&mut self, func_proto: &FuncProto) {
        if self.scope_manager.get(&func_proto.name).is_some() {
            panic!("Cannot redefine '{}'", func_proto.name);
        }

        let func_type = Type::Func(func_proto.clone());
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

        match func_proto.func_visibility {
            FuncVisibility::Public => LLVMSetLinkage(func, LLVMExternalLinkage),
            FuncVisibility::Private => LLVMSetLinkage(func, LLVMInternalLinkage),
            FuncVisibility::Extern => LLVMSetLinkage(func, LLVMExternalLinkage),
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
            let alloca = self.create_alloca(param_name, param_type);

            LLVMBuildStore(self.builder, param_value_ref, alloca);
            self.scope_manager.set(param_name, alloca);
        }

        self.compile_body(&func_def.body);

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
            TypedStatement::Call(c) => _ = self.compile_call(c),
            TypedStatement::If(i) => self.compile_if_statement(i),
        }
    }

    /// Compiles a variable declaration.
    unsafe fn compile_var_declaration(&mut self, var_declaration: &TypedVarDeclaration) {
        if self.get_cur_function().is_none() {
            panic!("Cannot compile var declaration outside of a function");
        }
        
        let var_name = var_declaration.var_name.as_str();
        let var_type = &var_declaration.var_type;
        let alloca = self.create_alloca(var_name, var_type);

        self.scope_manager.set(var_name, alloca);

        let value = self.compile_expr(&var_declaration.var_value);
        LLVMBuildStore(self.builder, value, alloca);
    }

    /// Compiles a while loop, assuming the LLVM builder is building inside a function body.
    unsafe fn compile_while_loop(&mut self, while_loop: &TypedWhileLoop) {
        let cur_func = match self.get_cur_function() {
            Some(func) => func,
            None => panic!("Cannot compile while declaration outside of a function"),
        };
        let cond_block = LLVMAppendBasicBlockInContext(self.context, cur_func, cstr!("cond"));
        let loop_block = LLVMCreateBasicBlockInContext(self.context, cstr!("loop"));
        // Build block to go to after loop is done executing
        let after_block = LLVMCreateBasicBlockInContext(self.context, cstr!("after"));

        LLVMBuildBr(self.builder, cond_block);
        LLVMPositionBuilderAtEnd(self.builder, cond_block);

        let condition = self.compile_expr(&while_loop.condition);
        LLVMBuildCondBr(self.builder, condition, loop_block, after_block);

        // Start insertion in loop_block.
        LLVMAppendExistingBasicBlock(cur_func, loop_block);
        LLVMPositionBuilderAtEnd(self.builder, loop_block);
        
        if !self.compile_body(&while_loop.body) {
            LLVMBuildBr(self.builder, cond_block);
        }
        
        LLVMAppendExistingBasicBlock(cur_func, after_block);
        LLVMPositionBuilderAtEnd(self.builder, after_block);
    }

    /// Compiles an assignment expression like `foo = 28` (and panics if `foo`'s type can't store 28).
    // TODO: should we remove the panics from here since they're already in Typer
    unsafe fn compile_assignment_statement(&mut self, assign: &TypedAssignment) {
        let alloca = match self.scope_manager.get(&assign.name) {
            Some(v) => *v,
            None => panic!("Setting a variable that has not been declared"),
        };

        // TODO: allow assigning functions with matching types to each other
        if !LLVMIsAFunction(alloca).is_null() {
            panic!("Cannot assign a value to function '{}'", assign.name);
        }

        let value = self.compile_expr(&assign.value);
        LLVMBuildStore(self.builder, value, alloca);
    }

    /// Compiles a return statement, panicking if the builder isn't inside a function.
    unsafe fn compile_ret_statement(&mut self, ret_value: &Option<TypedExpr>) {
        if self.get_cur_function().is_none() {
            panic!("Cannot compile ret statement outside of a function");
        }

        match ret_value {
            Some(expr) => LLVMBuildRet(self.builder, self.compile_expr(expr)),
            None => LLVMBuildRetVoid(self.builder),
        };
    }

    /// Compiles a function body, assuming the LLVM builder is building inside a function body.
    /// 
    /// Returns `true` if the body returns (contains a return statement), `false` otherwise.
    unsafe fn compile_body(&mut self, body: &[TypedStatement]) -> bool {
        let mut body_returns = false;
        self.scope_manager.enter_scope();
        for statement in body {
            self.compile_statement(statement);
            // If we have a termination instruction, stop compiling statements
            if let TypedStatement::Return(_) = statement { 
                body_returns = true;
                break;
             }
        }
        self.scope_manager.exit_scope();
        body_returns
    }

    /// This method compiles an typed if statementi.
    /// 
    /// # Notes
    /// - If `if_statement` has no else block, this method will still produce an empty else block;
    ///   it will be optimized away during LLVM's optimization passes anyway.
    unsafe fn compile_if_statement(&mut self, if_statement: &TypedIf) {
        let cur_func = match self.get_cur_function() {
            Some(func) => func,
            None => panic!("Cannot compile if statement outside of a function"),
        };

        let cond_block = LLVMAppendBasicBlockInContext(self.context, cur_func, cstr!("cond"));
        // then_block will be appended once cond_block is built
        let then_block = LLVMCreateBasicBlockInContext(self.context, cstr!("then"));
        // else_block will be appended once then_block is built
        let else_block = LLVMCreateBasicBlockInContext(self.context, cstr!("else"));
        // merge_block will be appended once else_block is built
        let merge_block = LLVMCreateBasicBlockInContext(self.context, cstr!("merge"));

        LLVMBuildBr(self.builder, cond_block);
        LLVMPositionBuilderAtEnd(self.builder, cond_block);

        let condition = self.compile_expr(&if_statement.condition);

        let target_else_block = match &if_statement.else_body {
            Some(_) => else_block,
            None => merge_block,
        };
        LLVMBuildCondBr(self.builder, condition, then_block, target_else_block);

        // ------------------------ THEN BLOCK ------------------------------
        // Start insertion in then_block.
        LLVMAppendExistingBasicBlock(cur_func, then_block);
        LLVMPositionBuilderAtEnd(self.builder, then_block);
        if !self.compile_body(&if_statement.then_body) {
            // Build branch to merge_block after if statement 
            LLVMBuildBr(self.builder, merge_block);
        }

        // ------------------------ ELSE BLOCK ------------------------------
        if let Some(else_body) = &if_statement.else_body {
            LLVMAppendExistingBasicBlock(cur_func, else_block); // start building else block
            LLVMPositionBuilderAtEnd(self.builder, else_block);
            if !self.compile_body(else_body) {
                LLVMBuildBr(self.builder, merge_block);
            }
        }

        // ------------------------ MERGE BLOCK ------------------------------

        LLVMAppendExistingBasicBlock(cur_func, merge_block);
        LLVMPositionBuilderAtEnd(self.builder, merge_block);
    }

    /// Compiles an expression, panicking if its value's type doesn't match the expected type.
    ///
    /// Note: if `expected_type` is `None`, then no type-checking is performed (because the caller
    /// doesn't actually know what the type must be).
    unsafe fn compile_expr(&mut self, expr: &TypedExpr) -> LLVMValueRef {
        match expr {
            TypedExpr::Identifier(id) => self.compile_identifier(id),
            TypedExpr::IntLiteral(int_literal) => self.compile_int_literal(int_literal),
            TypedExpr::BoolLiteral(bool_literal) => self.compile_bool_literal(*bool_literal),
            TypedExpr::Binary(bin_expr) => self.compile_bin_expr(bin_expr),
            TypedExpr::Comparison(comparison) => self.compile_comparison_expr(comparison),
            TypedExpr::Call(call) => self.compile_call(call),
            TypedExpr::Unary(unary) => self.compile_unary(unary),
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
    /// 
    /// This function converts a `TypedIntLiteral` into an LLVM constant integer value.
    /// If `int_literal.negative` is true, the value is negated.
    unsafe fn compile_int_literal(&self, int_literal: &TypedIntLiteral) -> LLVMValueRef {
        let int_type = self.to_llvm_type(&Type::Int(int_literal.int_type));
        let value_str = match int_literal.negative {
            true => format!("-{}", int_literal.int_value),
            false => int_literal.int_value.clone(),
        };
        let value_cstr = CString::new(value_str).unwrap();
        LLVMConstIntOfString(int_type, value_cstr.as_ptr(), 10)
    }

    /// Compiles an bool literal expression (true/false, also known as 1/0).
    unsafe fn compile_bool_literal(&self, bool_literal: bool) -> LLVMValueRef {
        let bool_type = self.to_llvm_type(&Type::Bool);
        LLVMConstInt(bool_type, bool_literal as u64, 0)
    }

    /// Compiles a binary expression (recursively compiling left- and right-hand sides).
    unsafe fn compile_bin_expr(&mut self, bin_expr: &TypedBinary) -> LLVMValueRef {
        use BinaryOperator::*;

        let lhs = self.compile_expr(&bin_expr.left);
        let rhs = self.compile_expr(&bin_expr.right);

        if LLVMTypeOf(lhs) != LLVMTypeOf(rhs) {
            panic!("Binary expr: type(LHS) != type(RHS) should've been handled by Typer")
        }

        let int_type = match bin_expr.result_type {
            Type::Int(int_type) => int_type,
            _ => panic!("Unsupported lhs and rhs types for binary expr; can only handle integers"),
        };

        match bin_expr.operator {
            Add => LLVMBuildAdd(self.builder, lhs, rhs, cstr!("add")),
            Subtract => LLVMBuildSub(self.builder, lhs, rhs, cstr!("sub")),
            Multiply => LLVMBuildMul(self.builder, lhs, rhs, cstr!("mul")),
            // TODO: signed-ints: Signed vs unsigned division
            Divide => match int_type {
                IntType { signed: true, .. } => LLVMBuildSDiv(self.builder, lhs, rhs, cstr!("sdiv")),
                IntType { signed: false, .. } => LLVMBuildUDiv(self.builder, lhs, rhs, cstr!("udiv")),
            },
            Remainder => match int_type {
                IntType { signed: true, .. } => LLVMBuildSRem(self.builder, lhs, rhs, cstr!("srem")),
                IntType { signed: false, .. } => LLVMBuildURem(self.builder, lhs, rhs, cstr!("urem")),
            }
        }
    }

    /// Compiles a unary expression.
    unsafe fn compile_unary(&mut self, unary: &TypedUnary) -> LLVMValueRef {
        let operand = self.compile_expr(&unary.operand);
        let source_type = &unary.operand.get_type();
        match &unary.operator {
            UnaryOperator::Cast(cast_type) => self.compile_cast(operand, cast_type, source_type),
        }
    }

    /// Compiles a cast expression.
    unsafe fn compile_cast(&mut self, operand: LLVMValueRef, cast_type: &Type, source_type: &Type) -> LLVMValueRef {
        let (cast_int_type, source_int_type) = match (cast_type, source_type) {
            (Type::Int(cast), Type::Int(source)) => (cast, source),
            (cast, source) => {
                panic!("Unsupported cast from {} to {} should've been handled by the typer", cast, source)
            }
        };

        if cast_int_type.width < source_int_type.width {
            LLVMBuildTrunc(self.builder, operand, self.to_llvm_type(cast_type), cstr!("trunc"))
        } else if cast_int_type.width > source_int_type.width {
            if cast_int_type.signed {
                LLVMBuildSExt(self.builder, operand, self.to_llvm_type(cast_type), cstr!("sext"))
            } else {
                LLVMBuildZExt(self.builder, operand, self.to_llvm_type(cast_type), cstr!("zext"))
            }
        } else {
            // Width is the same. No need to cast.
            // (Note: Typer ensures source_int_type.signed == cast_int_type.signed)
            operand
        }
    }

    /// Compiles a comparison expression.
    unsafe fn compile_comparison_expr(&mut self, comparison: &TypedComparison) -> LLVMValueRef {
        let lhs = self.compile_expr(&comparison.left);
        let rhs = self.compile_expr(&comparison.right);

        if LLVMTypeOf(lhs) != LLVMTypeOf(rhs) {
            panic!("Comparison: type(LHS) != type(RHS) should've been handled by Typer")
        }

        match comparison.operand_type {
            Type::Int(int_type) => LLVMBuildICmp(self.builder, self.comparison_int_op(comparison.operator, int_type), lhs, rhs, cstr!("")),
            _ => panic!("Unsupported lhs and rhs types for comparison; can only handle integers"),
        }
    }

    unsafe fn comparison_int_op(&mut self, operator: ComparisonOperator, result_type: IntType) -> LLVMIntPredicate {
        use LLVMIntPredicate::*;
        
        match operator {
            ComparisonOperator::NotEqualTo => LLVMIntNE,
            ComparisonOperator::EqualTo => LLVMIntEQ,
            ComparisonOperator::LessThan => match result_type {
                IntType { signed: true, .. } => LLVMIntSLT,
                IntType { signed: false, .. } => LLVMIntULT,
            },
            ComparisonOperator::GreaterThan => match result_type {
                IntType { signed: true, .. } => LLVMIntSGT,
                IntType { signed: false, .. } => LLVMIntUGT,
            },
            ComparisonOperator::LessOrEqualTo => match result_type {
                IntType { signed: true, .. } => LLVMIntSLE,
                IntType { signed: false, .. } => LLVMIntULE,
            },
            ComparisonOperator::GreaterOrEqualTo => match result_type {
                IntType { signed: true, .. } => LLVMIntSGE,
                IntType { signed: false, .. } => LLVMIntUGE,
            },
        }
    }

    /// Compiles a typed function call
    unsafe fn compile_call(&mut self, call: &TypedCall) -> LLVMValueRef {
        let func = match self.scope_manager.get(&call.function_name) {
            Some(v) => *v,
            None => panic!("Undefined functions should be handled by typer"),
        };

        if LLVMIsAFunction(func).is_null() {
            panic!(
                "Calls like foo() where foo isn't callable (e.g. i32) should be handled by typer"
            )
        }

        let num_params = call.function_proto.params.len();
        if num_params != call.args.len() {
            panic!("Number of arguments should be handled by typer");
        }

        let mut arg_values = Vec::with_capacity(call.args.len());
        for i in 0..num_params {
            let arg = call.args.get(i).unwrap();
            let value = self.compile_expr(arg);
            arg_values.push(value);
        }

        let func_type = self.to_llvm_type(&Type::Func(call.function_proto.clone()));
        LLVMBuildCall2(
            self.builder,
            func_type,
            func,
            arg_values.as_mut_ptr(),
            arg_values.len() as c_uint,
            cstr!(""),
        )
    }

    /// Converts Flick's [Type] enum to llvm-sys's [LLVMTypeRef].
    unsafe fn to_llvm_type(&self, t: &Type) -> LLVMTypeRef {
        match t {
            Type::Int(int_type) => LLVMIntTypeInContext(self.context, int_type.width),
            Type::Bool => LLVMInt1TypeInContext(self.context),
            Type::Void => LLVMVoidTypeInContext(self.context),
            Type::Func(func_proto) => {
                let return_type = self.to_llvm_type(func_proto.return_type.as_ref());
                let num_params = func_proto.params.len() as c_uint;
                let mut param_types: Vec<_> = func_proto
                    .params
                    .iter()
                    .map(|p| self.to_llvm_type(&p.param_type))
                    .collect();

                LLVMFunctionType(return_type, param_types.as_mut_ptr(), num_params, 0)
            }
        }
    }

    /// Creates an LLVM 'alloca', which can then be used to set up a local variable.
    unsafe fn create_alloca(&self, var_name: &str, var_type: &Type) -> LLVMValueRef {
        let var_name_c = CString::new(var_name).unwrap();
        LLVMBuildAlloca(self.builder, self.to_llvm_type(var_type), var_name_c.as_ptr())
    }

    /// Returns the function currently being built by the compiler.
    unsafe fn get_cur_function(&self) -> Option<LLVMValueRef> {
        let cur_func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
        if LLVMIsNull(cur_func) == 1 {
            return None;
        }
        Some(cur_func)
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

/// As suggested by Clippy's [new_without_default][a], since [Compiler::new()] doesn't
/// take any arguments, Compiler should implement Default.
///
/// [a]: https://rust-lang.github.io/rust-clippy/master/index.html#/new_without_default
impl Default for Compiler {
    fn default() -> Self {
        Compiler::new()
    }
}
