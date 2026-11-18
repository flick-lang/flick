use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction;
use llvm_sys::analysis::LLVMVerifyFunction;
use std::ffi::{CStr, CString, c_char, c_uint};
use std::path::Path;

use llvm_sys::LLVMLinkage::{LLVMExternalLinkage, LLVMInternalLinkage};
use llvm_sys::bit_writer::LLVMWriteBitcodeToFile;
use llvm_sys::core::*;
use llvm_sys::error::{LLVMDisposeErrorMessage, LLVMGetErrorMessage};
use llvm_sys::prelude::*;
use llvm_sys::target::{
    LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeTarget, LLVMDisposeTargetData,
    LLVMSetModuleDataLayout,
};
use llvm_sys::target_machine::LLVMCodeGenFileType::{self, LLVMAssemblyFile, LLVMObjectFile};
use llvm_sys::target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault;
use llvm_sys::target_machine::LLVMCodeModel::LLVMCodeModelDefault;
use llvm_sys::target_machine::LLVMRelocMode::LLVMRelocPIC;
use llvm_sys::target_machine::{
    LLVMCreateTargetDataLayout, LLVMCreateTargetMachine, LLVMDisposeTargetMachine,
    LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple, LLVMTarget, LLVMTargetMachineEmitToFile,
    LLVMTargetMachineRef,
};
use llvm_sys::transforms::pass_builder::*;
use llvm_sys::{LLVMIntPredicate, LLVMTypeKind};

use crate::ScopeManager;
use crate::ast::*;
use crate::typed_ast::*;
use crate::types::{IntType, Type};

/// The oldest macOS that compiled programs run on, unless `MACOSX_DEPLOYMENT_TARGET` overrides it.
/// macOS 11 is the first to run on Apple Silicon, and also runs on Intel.
const DEFAULT_MACOS_DEPLOYMENT_TARGET: &str = "11.0";

/// Converts a path into the null-terminated string LLVM expects.
fn path_to_cstring(path: &Path) -> CString {
    CString::new(path.to_string_lossy().into_owned()).expect("path contains a null byte")
}

/// Copies a string that LLVM allocated and documents as freed with `LLVMDisposeMessage` (such as
/// an `ErrorMessage` out-parameter after a call that reported failure), then frees it.
///
/// # Safety
/// `message` must be such a string, and must not be used afterwards.
unsafe fn take_message(message: *mut c_char) -> String {
    // SAFETY: Upheld by the caller; LLVM requires these messages to be freed with LLVMDisposeMessage.
    unsafe {
        let owned = CStr::from_ptr(message).to_string_lossy().into_owned();
        LLVMDisposeMessage(message);
        owned
    }
}

/// Returns the triple to compile for, which is the computer flick itself was built for.
///
/// On macOS this is built the way rustc does it. LLVM's default triple contains the running OS
/// version (e.g. `arm64-apple-darwin25.6.0`), and LLVM records a triple's OS version as the minimum
/// OS the output runs on, so instead this uses `MACOSX_DEPLOYMENT_TARGET` if set, and otherwise
/// [DEFAULT_MACOS_DEPLOYMENT_TARGET].
fn target_triple() -> CString {
    let triple = if cfg!(target_os = "macos") {
        let arch = std::env::consts::ARCH;
        let deployment_target = std::env::var("MACOSX_DEPLOYMENT_TARGET")
            .unwrap_or_else(|_| DEFAULT_MACOS_DEPLOYMENT_TARGET.to_string());
        format!("{arch}-apple-macosx{deployment_target}")
    } else {
        // SAFETY: LLVM allocates the default triple for us to free with LLVMDisposeMessage.
        unsafe { take_message(LLVMGetDefaultTargetTriple()) }
    };

    CString::new(triple).expect("target triple contains a null byte")
}

/// LLVM's largest integer width. LLVM only asserts this in debug builds; in release builds, wider
/// types silently get the wrong width.
///
/// Copied from `llvm::IntegerType::MAX_INT_BITS` (`llvm/IR/DerivedTypes.h`), which isn't part of
/// LLVM's C API, so llvm-sys can't expose it.
const MAX_INT_WIDTH: u32 = 1 << 23;

/// Panics unless values of type `t` can be stored, loaded, passed, and returned (i.e. it's sized).
///
/// Building an alloca, load, or store makes LLVM compute the type's alignment, which is undefined
/// behaviour for void and function types in release builds.
fn assert_value_type(t: &Type) {
    if !matches!(t, Type::Int(_) | Type::Bool) {
        panic!("'{t}' can't be the type of a value; this should've been handled by the typer");
    }
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
/// # Safety invariants
///
/// Every method is safe to call, even with a hand-built [TypedProgram], because:
/// - The LLVM objects in this struct are created in [Compiler::new], disposed in [Drop], and never
///   handed out, so they're valid for as long as `self` is. Every other LLVM value, type, and block
///   it uses is created through them.
/// - Before each LLVM call that misbehaves on bad input in release builds (where LLVM's own
///   assertions are compiled out), the input is checked, panicking if it's invalid: integer widths,
///   value types, the builder being inside a function, integer operands, parameter counts, and
///   callee types.
/// - Every other mistake in a [TypedProgram] only builds invalid IR, which `LLVMVerifyFunction`
///   rejects (with a panic) before it's optimized or emitted.
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
        // SAFETY: Only creates and configures fresh LLVM objects, which this struct then owns.
        unsafe {
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(c"module".as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);
            let scope_manager = ScopeManager::new();

            // TODO: make this an option for the compiler to choose which target to initialize
            if LLVM_InitializeNativeTarget() == 1 {
                panic!("Error initializing native target")
            }
            if LLVM_InitializeNativeAsmPrinter() == 1 {
                panic!("Error initializing native ASM printer")
            }

            // Configure module
            let triple = target_triple();
            LLVMSetTarget(module, triple.as_ptr());

            let cpu = c"generic".as_ptr();
            let features = c"".as_ptr();
            let target = Self::get_target_from_triple(&triple);
            let target_machine = LLVMCreateTargetMachine(
                target,
                triple.as_ptr(),
                cpu,
                features,
                LLVMCodeGenLevelDefault,
                LLVMRelocPIC,
                LLVMCodeModelDefault,
            );

            let target_data_layout = LLVMCreateTargetDataLayout(target_machine);
            LLVMSetModuleDataLayout(module, target_data_layout);
            LLVMDisposeTargetData(target_data_layout);

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
    fn get_target_from_triple(triple: &CStr) -> *mut LLVMTarget {
        // SAFETY: The triple is a valid C string, and LLVM only sets err_str on failure.
        unsafe {
            let mut target = std::ptr::null_mut();
            let mut err_str = std::ptr::null_mut();
            if LLVMGetTargetFromTriple(triple.as_ptr(), &mut target, &mut err_str) != 0 {
                panic!(
                    "Error getting target from triple ({})",
                    take_message(err_str)
                );
            }
            target
        }
    }

    /// This function prints the LLVM IR generated so far (via methods like [compile][a]).
    ///
    /// [a]: Compiler::compile
    pub fn print_ir(&self) {
        // SAFETY: See the invariants on [Compiler].
        unsafe { LLVMDumpModule(self.module) }
    }

    /// This function optimizes the LLVM IR generated so far (via methods like [compile][a]).
    ///
    /// [a]: Compiler::compile
    pub fn optimize(&mut self) {
        // SAFETY: See the invariants on [Compiler]; the pass string is a valid C string.
        unsafe {
            let passes = c"default<O1>".as_ptr();
            let error = LLVMRunPasses(self.module, passes, self.target_machine, self.pass_builder);
            if !error.is_null() {
                // Consumes the error; the message must be freed with LLVMDisposeErrorMessage
                let message = LLVMGetErrorMessage(error);
                let owned = CStr::from_ptr(message).to_string_lossy().into_owned();
                LLVMDisposeErrorMessage(message);
                panic!("Error running optimizations: {owned}");
            }
        }
    }

    /// This function writes the code generated so far (via methods like [compile][a]) to an object file.
    ///
    /// [a]: Compiler::compile
    pub fn to_file(&self, path: &impl AsRef<Path>) {
        self.emit_machine_code(path.as_ref(), LLVMObjectFile);
    }

    /// This function writes the code generated so far (via methods like [compile][a]) to a target
    /// assembly `.s` file.
    ///
    /// [a]: Compiler::compile
    pub fn asm_to_file(&self, path: &impl AsRef<Path>) {
        self.emit_machine_code(path.as_ref(), LLVMAssemblyFile);
    }

    fn emit_machine_code(&self, path: &Path, file_type: LLVMCodeGenFileType) {
        let path = path_to_cstring(path);
        let mut err_str = std::ptr::null_mut();
        // SAFETY: See the invariants on [Compiler]; LLVM only sets err_str on failure.
        unsafe {
            let result = LLVMTargetMachineEmitToFile(
                self.target_machine,
                self.module,
                path.as_ptr(),
                file_type,
                &mut err_str,
            );
            if result == 1 {
                panic!("Error emitting machine code ({})", take_message(err_str));
            }
        }
    }

    /// This function writes the LLVM IR generated so far (via methods like [compile][a]) to a
    /// textual `.ll` file.
    ///
    /// [a]: Compiler::compile
    pub fn ir_to_file(&self, path: &impl AsRef<Path>) {
        let path = path_to_cstring(path.as_ref());
        let mut err_str = std::ptr::null_mut();
        // SAFETY: See the invariants on [Compiler]; LLVM only sets err_str on failure.
        unsafe {
            if LLVMPrintModuleToFile(self.module, path.as_ptr(), &mut err_str) == 1 {
                panic!("Error emitting LLVM IR ({})", take_message(err_str));
            }
        }
    }

    /// This function writes the LLVM IR generated so far (via methods like [compile][a]) to a
    /// binary `.bc` bitcode file.
    ///
    /// [a]: Compiler::compile
    pub fn bitcode_to_file(&self, path: &impl AsRef<Path>) {
        let path = path_to_cstring(path.as_ref());
        // SAFETY: See the invariants on [Compiler].
        if unsafe { LLVMWriteBitcodeToFile(self.module, path.as_ptr()) } != 0 {
            panic!("Error emitting LLVM bitcode to {}", path.to_string_lossy());
        }
    }

    /// This function compiles the provided program; once compiled, its LLVM IR can be [optimized][a],
    /// [printed][b], or [written to a file][c].
    ///
    /// The general idea is that `LLVMValueRef` instances are created recursively for various expressions,
    /// function calls, and function definitions. See implementation for details.
    ///
    /// # Panics
    /// If the program is malformed in a way the type checker normally rules out.
    ///
    /// [a]: Compiler::optimize
    /// [b]: Compiler::print_ir
    /// [c]: Compiler::to_file
    pub fn compile(&mut self, program: &TypedProgram) {
        self.scope_manager.enter_scope();
        for global_statement in program.global_statements.iter() {
            // TODO: In the future when we have additional global statements, maybe move this into a new function called 'preprocess_global_statement' or something like that
            match global_statement {
                TypedGlobalStatement::Extern(p) => self.compile_func_proto(p),
                TypedGlobalStatement::FuncDef(f) => self.compile_func_proto(&f.proto),
            }
        }
        for global_statement in program.global_statements.iter() {
            self.compile_global_statement(global_statement);
        }
        self.scope_manager.exit_scope()
    }

    /// Compiles a global statement, whose prototype [compile][a] has already registered.
    ///
    /// [a]: Compiler::compile
    fn compile_global_statement(&mut self, global_statement: &TypedGlobalStatement) {
        if let TypedGlobalStatement::FuncDef(func_def) = global_statement {
            self.compile_func_def(func_def);
        }
    }

    /// Registers a function prototype, panicking if the function has already been defined.
    fn compile_func_proto(&mut self, func_proto: &FuncProto) {
        if self.scope_manager.get(&func_proto.name).is_some() {
            panic!("Cannot redefine '{}'", func_proto.name);
        }

        let func_type = Type::Func(func_proto.clone());
        let func_llvm_type = self.to_llvm_type(&func_type);
        let func_name = CString::new(func_proto.name.as_str()).unwrap();
        // SAFETY: See the invariants on [Compiler]. The function's type was built from these same
        // params, so every parameter index is in range.
        unsafe {
            let func = LLVMAddFunction(self.module, func_name.as_ptr(), func_llvm_type);

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
    }

    /// Complies a function definition, assuming the function's prototype has been compiled.
    fn compile_func_def(&mut self, func_def: &TypedFuncDef) {
        let func_name = CString::new(func_def.proto.name.as_str()).unwrap();
        // SAFETY: See the invariants on [Compiler]. Parameter indices are checked against the
        // function's real parameter count, since LLVMGetParam doesn't check them.
        unsafe {
            let func = LLVMGetNamedFunction(self.module, func_name.as_ptr());
            if func.is_null() {
                panic!(
                    "The prototype for function '{}' has not been defined",
                    func_def.proto.name
                );
            }

            let declared_param_count = LLVMCountParams(func) as usize;
            let defined_param_count = func_def.proto.params.len();
            if declared_param_count != defined_param_count {
                panic!(
                    "Function '{}' is declared with {declared_param_count} parameters but defined with {defined_param_count}",
                    func_def.proto.name
                );
            }

            let entry_block = LLVMAppendBasicBlockInContext(self.context, func, c"entry".as_ptr());
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
    }

    /// Compiles a statement, assuming the LLVM builder is building inside a function body.
    fn compile_statement(&mut self, statement: &TypedStatement) {
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
    fn compile_var_declaration(&mut self, var_declaration: &TypedVarDeclaration) {
        let var_name = var_declaration.var_name.as_str();
        let var_type = &var_declaration.var_type;
        let alloca = self.create_alloca(var_name, var_type);

        self.scope_manager.set(var_name, alloca);

        let value = self.compile_int_expr(&var_declaration.var_value);
        // SAFETY: See the invariants on [Compiler]; create_alloca checked we're in a function.
        unsafe {
            LLVMBuildStore(self.builder, value, alloca);
        }
    }

    /// Compiles a while loop, assuming the LLVM builder is building inside a function body.
    fn compile_while_loop(&mut self, while_loop: &TypedWhileLoop) {
        let cur_func = self.expect_cur_function("compile a while loop");
        // SAFETY: See the invariants on [Compiler].
        unsafe {
            let cond_block =
                LLVMAppendBasicBlockInContext(self.context, cur_func, c"cond".as_ptr());
            let loop_block = LLVMCreateBasicBlockInContext(self.context, c"loop".as_ptr());
            // Build block to go to after loop is done executing
            let after_block = LLVMCreateBasicBlockInContext(self.context, c"after".as_ptr());

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
    }

    /// Compiles an assignment expression like `foo = 28` (and panics if `foo`'s type can't store 28).
    // TODO: should we remove the panics from here since they're already in Typer
    fn compile_assignment_statement(&mut self, assign: &TypedAssignment) {
        self.expect_cur_function("assign to a variable");

        let alloca = match self.scope_manager.get(&assign.name) {
            Some(v) => *v,
            None => panic!("Setting a variable that has not been declared"),
        };

        // SAFETY: See the invariants on [Compiler].
        unsafe {
            // TODO: allow assigning functions with matching types to each other
            if !LLVMIsAFunction(alloca).is_null() {
                panic!("Cannot assign a value to function '{}'", assign.name);
            }

            let value = self.compile_int_expr(&assign.value);
            LLVMBuildStore(self.builder, value, alloca);
        }
    }

    /// Compiles a return statement, panicking if the builder isn't inside a function.
    fn compile_ret_statement(&mut self, ret_value: &Option<TypedExpr>) {
        self.expect_cur_function("compile a ret statement");

        // SAFETY: See the invariants on [Compiler].
        unsafe {
            match ret_value {
                Some(expr) => LLVMBuildRet(self.builder, self.compile_expr(expr)),
                None => LLVMBuildRetVoid(self.builder),
            };
        }
    }

    /// Compiles a function body, assuming the LLVM builder is building inside a function body.
    ///
    /// Returns `true` if the body always returns, in which case the current block is already
    /// terminated.
    fn compile_body(&mut self, body: &[TypedStatement]) -> bool {
        let mut body_returns = false;
        self.scope_manager.enter_scope();
        for statement in body {
            self.compile_statement(statement);
            // Anything after this is dead code, and the block is already terminated
            if statement.always_returns() {
                body_returns = true;
                break;
            }
        }
        self.scope_manager.exit_scope();
        body_returns
    }

    /// This method compiles an typed if statement.
    ///
    /// # Notes
    /// - Without an 'else' branch, this still produces an empty else block; it will be optimized
    ///   away during LLVM's optimization passes anyway.
    /// - The merge block (where control continues after the if statement) only exists if some
    ///   branch falls through. When both branches return, it would be an empty block without a
    ///   terminator, which LLVM rejects.
    fn compile_if_statement(&mut self, if_statement: &TypedIf) {
        let cur_func = self.expect_cur_function("compile an if statement");

        const FALLS_THROUGH: &str = "a branch that falls through implies a merge block";

        // SAFETY: See the invariants on [Compiler]. Each block is appended to the current function
        // exactly once, before the function is verified.
        unsafe {
            let cond_block =
                LLVMAppendBasicBlockInContext(self.context, cur_func, c"cond".as_ptr());
            // Each of these is appended once the block before it is built
            let then_block = LLVMCreateBasicBlockInContext(self.context, c"then".as_ptr());
            let else_block = LLVMCreateBasicBlockInContext(self.context, c"else".as_ptr());
            let merge_block = if if_statement.always_returns() {
                None
            } else {
                Some(LLVMCreateBasicBlockInContext(
                    self.context,
                    c"merge".as_ptr(),
                ))
            };

            LLVMBuildBr(self.builder, cond_block);
            LLVMPositionBuilderAtEnd(self.builder, cond_block);

            let condition = self.compile_expr(&if_statement.condition);
            LLVMBuildCondBr(self.builder, condition, then_block, else_block);

            // ------------------------ THEN BLOCK ------------------------------
            LLVMAppendExistingBasicBlock(cur_func, then_block);
            LLVMPositionBuilderAtEnd(self.builder, then_block);
            let then_body_returns = self.compile_body(&if_statement.then_body);
            if !then_body_returns {
                LLVMBuildBr(self.builder, merge_block.expect(FALLS_THROUGH));
            }

            // ------------------------ ELSE BLOCK ------------------------------
            // Without an 'else' branch, this block is empty and just falls through to the merge block
            let else_body = match &if_statement.else_body {
                Some(else_body) => else_body.as_slice(),
                None => &[],
            };
            LLVMAppendExistingBasicBlock(cur_func, else_block);
            LLVMPositionBuilderAtEnd(self.builder, else_block);
            let else_body_returns = self.compile_body(else_body);
            if !else_body_returns {
                LLVMBuildBr(self.builder, merge_block.expect(FALLS_THROUGH));
            }

            // ------------------------ MERGE BLOCK ------------------------------
            if let Some(merge_block) = merge_block {
                LLVMAppendExistingBasicBlock(cur_func, merge_block);
                LLVMPositionBuilderAtEnd(self.builder, merge_block);
            }
        }
    }

    /// Compiles an expression.
    fn compile_expr(&mut self, expr: &TypedExpr) -> LLVMValueRef {
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

    /// Compiles an expression whose value must be an integer (or bool), panicking otherwise.
    ///
    /// Arithmetic, casts, and stores need this: LLVM constant-folds arithmetic and casts right away,
    /// which is undefined behaviour for operands of the wrong type, and storing a void value (such
    /// as a void call's result) makes it compute the alignment of void.
    fn compile_int_expr(&mut self, expr: &TypedExpr) -> LLVMValueRef {
        let value = self.compile_expr(expr);
        // SAFETY: See the invariants on [Compiler].
        let value_kind = unsafe { LLVMGetTypeKind(LLVMTypeOf(value)) };
        if value_kind != LLVMTypeKind::LLVMIntegerTypeKind {
            panic!("Expected an integer value; this should've been handled by the typer");
        }
        value
    }

    /// Compiles an identifier expression (variable value) with an expected type.
    fn compile_identifier(&mut self, id: &TypedIdentifier) -> LLVMValueRef {
        self.expect_cur_function("load a variable");
        assert_value_type(&id.id_type);

        let alloca = match self.scope_manager.get(id.name.as_str()) {
            Some(v) => *v,
            None => panic!("Compiler error: undefined identifier '{}'", id.name),
        };

        let alloca_type = self.to_llvm_type(&id.id_type);
        let name = CString::new(id.name.as_str()).unwrap();
        // SAFETY: See the invariants on [Compiler].
        unsafe { LLVMBuildLoad2(self.builder, alloca_type, alloca, name.as_ptr()) }
    }

    /// Compiles an integer literal expression.
    ///
    /// This function converts a `TypedIntLiteral` into an LLVM constant integer value.
    /// If `int_literal.negative` is true, the value is negated.
    fn compile_int_literal(&self, int_literal: &TypedIntLiteral) -> LLVMValueRef {
        let int_type = self.to_llvm_type(&Type::Int(int_literal.int_type));
        let value_cstr = CString::new(int_literal.int_value.as_str()).unwrap();
        // SAFETY: See the invariants on [Compiler]. For a null-terminated string, invalid digits only
        // give a wrong value, never undefined behaviour.
        unsafe { LLVMConstIntOfString(int_type, value_cstr.as_ptr(), 10) }
    }

    /// Compiles an bool literal expression (true/false, also known as 1/0).
    fn compile_bool_literal(&self, bool_literal: bool) -> LLVMValueRef {
        let bool_type = self.to_llvm_type(&Type::Bool);
        // SAFETY: See the invariants on [Compiler]; both values fit in an i1.
        unsafe { LLVMConstInt(bool_type, bool_literal as u64, 0) }
    }

    /// Compiles a binary expression (recursively compiling left- and right-hand sides).
    fn compile_bin_expr(&mut self, bin_expr: &TypedBinary) -> LLVMValueRef {
        use BinaryOperator::*;

        let lhs = self.compile_int_expr(&bin_expr.left);
        let rhs = self.compile_int_expr(&bin_expr.right);

        let int_type = match bin_expr.result_type {
            Type::Int(int_type) => int_type,
            _ => panic!("Unsupported lhs and rhs types for binary expr; can only handle integers"),
        };

        // SAFETY: See the invariants on [Compiler]. Both operands are integers of the same type,
        // which LLVM's constant folding relies on.
        unsafe {
            if LLVMTypeOf(lhs) != LLVMTypeOf(rhs) {
                panic!("Binary expr: type(LHS) != type(RHS) should've been handled by Typer")
            }

            match bin_expr.operator {
                Add => LLVMBuildAdd(self.builder, lhs, rhs, c"add".as_ptr()),
                Subtract => LLVMBuildSub(self.builder, lhs, rhs, c"sub".as_ptr()),
                Multiply => LLVMBuildMul(self.builder, lhs, rhs, c"mul".as_ptr()),
                // TODO: signed-ints: Signed vs unsigned division
                Divide => match int_type {
                    IntType { signed: true, .. } => {
                        LLVMBuildSDiv(self.builder, lhs, rhs, c"sdiv".as_ptr())
                    }
                    IntType { signed: false, .. } => {
                        LLVMBuildUDiv(self.builder, lhs, rhs, c"udiv".as_ptr())
                    }
                },
                Remainder => match int_type {
                    IntType { signed: true, .. } => {
                        LLVMBuildSRem(self.builder, lhs, rhs, c"srem".as_ptr())
                    }
                    IntType { signed: false, .. } => {
                        LLVMBuildURem(self.builder, lhs, rhs, c"urem".as_ptr())
                    }
                },
            }
        }
    }

    /// Compiles a unary expression.
    fn compile_unary(&mut self, unary: &TypedUnary) -> LLVMValueRef {
        match &unary.operator {
            UnaryOperator::Cast(cast_type) => self.compile_cast(&unary.operand, cast_type),
            UnaryOperator::Negate => self.compile_negation(&unary.operand),
        }
    }

    /// Compiles a cast expression.
    fn compile_cast(&mut self, operand: &TypedExpr, cast_type: &Type) -> LLVMValueRef {
        let source_type = operand.get_result_type();
        let cast_int_type = match (cast_type, &source_type) {
            (Type::Int(cast), Type::Int(_)) => cast,
            (cast, source) => {
                panic!(
                    "Unsupported cast from {} to {} should've been handled by the typer",
                    source, cast
                )
            }
        };

        let cast_llvm_type = self.to_llvm_type(cast_type);
        let operand = self.compile_int_expr(operand);
        // SAFETY: See the invariants on [Compiler]. The source width comes from the operand itself
        // rather than the AST, because LLVM folds casts of constants right away, and folding an
        // invalid trunc or ext is undefined behaviour.
        unsafe {
            let source_width = LLVMGetIntTypeWidth(LLVMTypeOf(operand));
            let cast_width = cast_int_type.width;

            if cast_width < source_width {
                LLVMBuildTrunc(self.builder, operand, cast_llvm_type, c"trunc".as_ptr())
            } else if cast_width > source_width {
                if cast_int_type.signed {
                    LLVMBuildSExt(self.builder, operand, cast_llvm_type, c"sext".as_ptr())
                } else {
                    LLVMBuildZExt(self.builder, operand, cast_llvm_type, c"zext".as_ptr())
                }
            } else {
                // Width is the same. No need to cast.
                // (Note: Typer ensures source_int_type.signed == cast_int_type.signed)
                operand
            }
        }
    }

    /// Compiles a negation expression.
    fn compile_negation(&mut self, operand: &TypedExpr) -> LLVMValueRef {
        if !matches!(
            operand.get_result_type(),
            Type::Int(IntType { signed: true, .. })
        ) {
            panic!(
                "Unsupported type for negation, can only handle integers; this should have been handled by typer"
            );
        }

        let operand = self.compile_int_expr(operand);
        // SAFETY: See the invariants on [Compiler]; the operand is an integer.
        unsafe { LLVMBuildNeg(self.builder, operand, c"neg".as_ptr()) }
    }

    /// Compiles a comparison expression.
    fn compile_comparison_expr(&mut self, comparison: &TypedComparison) -> LLVMValueRef {
        let lhs = self.compile_int_expr(&comparison.left);
        let rhs = self.compile_int_expr(&comparison.right);

        // SAFETY: See the invariants on [Compiler]. Both operands are integers of the same type,
        // which LLVM's constant folding relies on.
        unsafe {
            if LLVMTypeOf(lhs) != LLVMTypeOf(rhs) {
                panic!("Comparison: type(LHS) != type(RHS) should've been handled by Typer")
            }

            match comparison.operand_type {
                Type::Int(int_type) => LLVMBuildICmp(
                    self.builder,
                    self.comparison_int_op(comparison.operator, int_type),
                    lhs,
                    rhs,
                    c"".as_ptr(),
                ),
                _ => {
                    panic!("Unsupported lhs and rhs types for comparison; can only handle integers")
                }
            }
        }
    }

    fn comparison_int_op(
        &mut self,
        operator: ComparisonOperator,
        result_type: IntType,
    ) -> LLVMIntPredicate {
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
    fn compile_call(&mut self, call: &TypedCall) -> LLVMValueRef {
        let func = match self.scope_manager.get(&call.function_name) {
            Some(v) => *v,
            None => panic!("Undefined functions should be handled by typer"),
        };

        let num_params = call.function_proto.params.len();
        if num_params != call.args.len() {
            panic!("Number of arguments should be handled by typer");
        }

        let func_type = self.to_llvm_type(&Type::Func(call.function_proto.clone()));

        // SAFETY: See the invariants on [Compiler]. The callee is checked to be a function of the
        // claimed type, which the verifier doesn't check: a mismatch would build a call that
        // verifies but misbehaves at runtime.
        unsafe {
            if LLVMIsAFunction(func).is_null() {
                panic!(
                    "Calls like foo() where foo isn't callable (e.g. i32) should be handled by typer"
                )
            }
            if LLVMGlobalGetValueType(func) != func_type {
                panic!(
                    "Call to '{}' doesn't match its prototype; this should've been handled by the typer",
                    call.function_name
                )
            }

            let mut arg_values = Vec::with_capacity(call.args.len());
            for arg in &call.args {
                let value = self.compile_expr(arg);
                arg_values.push(value);
            }

            LLVMBuildCall2(
                self.builder,
                func_type,
                func,
                arg_values.as_mut_ptr(),
                arg_values.len() as c_uint,
                c"".as_ptr(),
            )
        }
    }

    /// Converts Flick's [Type] enum to llvm-sys's [LLVMTypeRef], panicking if LLVM can't represent
    /// it: integer widths must be in LLVM's range, and functions can only take and return values.
    fn to_llvm_type(&self, t: &Type) -> LLVMTypeRef {
        // SAFETY: See the invariants on [Compiler]; widths and signatures are checked first.
        unsafe {
            match t {
                Type::Int(int_type) => {
                    let width = int_type.width;
                    if !(1..=MAX_INT_WIDTH).contains(&width) {
                        panic!(
                            "Integer widths must be between 1 and {MAX_INT_WIDTH}, not {width}; this should've been handled by the typer"
                        );
                    }
                    LLVMIntTypeInContext(self.context, width)
                }
                Type::Bool => LLVMInt1TypeInContext(self.context),
                Type::Void => LLVMVoidTypeInContext(self.context),
                Type::Func(func_proto) => {
                    let return_type = func_proto.return_type.as_ref();
                    if *return_type != Type::Void {
                        assert_value_type(return_type);
                    }

                    let return_llvm_type = self.to_llvm_type(return_type);
                    let num_params = func_proto.params.len() as c_uint;
                    let mut param_types = Vec::with_capacity(func_proto.params.len());
                    for param in &func_proto.params {
                        assert_value_type(&param.param_type);
                        param_types.push(self.to_llvm_type(&param.param_type));
                    }

                    LLVMFunctionType(return_llvm_type, param_types.as_mut_ptr(), num_params, 0)
                }
            }
        }
    }

    /// Creates an LLVM 'alloca', which can then be used to set up a local variable.
    fn create_alloca(&self, var_name: &str, var_type: &Type) -> LLVMValueRef {
        self.expect_cur_function("allocate a variable");
        assert_value_type(var_type);

        let var_llvm_type = self.to_llvm_type(var_type);
        let var_name_c = CString::new(var_name).unwrap();
        // SAFETY: See the invariants on [Compiler]; we're in a function, and the type is sized.
        unsafe { LLVMBuildAlloca(self.builder, var_llvm_type, var_name_c.as_ptr()) }
    }

    /// Returns the function currently being built by the compiler, if any.
    fn get_cur_function(&self) -> Option<LLVMValueRef> {
        // SAFETY: See the invariants on [Compiler]; the block is only used if it exists.
        unsafe {
            let insert_block = LLVMGetInsertBlock(self.builder);
            if insert_block.is_null() {
                return None;
            }

            let cur_func = LLVMGetBasicBlockParent(insert_block);
            if cur_func.is_null() {
                return None;
            }

            Some(cur_func)
        }
    }

    /// Returns the function currently being built, panicking if there isn't one.
    ///
    /// Allocas, loads, and stores need one, since LLVM looks up the builder's function to build them
    /// (a null dereference without one).
    fn expect_cur_function(&self, action: &str) -> LLVMValueRef {
        match self.get_cur_function() {
            Some(cur_func) => cur_func,
            None => panic!("Cannot {action} outside of a function"),
        }
    }
}

impl Drop for Compiler {
    /// Disposes the underlying llvm-sys C objects so that we don't leak memory.
    fn drop(&mut self) {
        // SAFETY: These objects are owned by this struct and disposed exactly once, here.
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

#[cfg(test)]
mod tests {
    use super::*;

    // These build invalid programs by hand, the way a library user could, to check that every
    // input LLVM can't handle safely panics instead of reaching LLVM.

    fn uint(width: u32) -> IntType {
        IntType {
            signed: false,
            width,
        }
    }

    fn proto(name: &str, params: &[Type], return_type: Type) -> FuncProto {
        let params = params
            .iter()
            .enumerate()
            .map(|(i, param_type)| FuncParam {
                param_type: param_type.clone(),
                param_name: format!("p{i}"),
            })
            .collect();
        FuncProto {
            func_visibility: FuncVisibility::Public,
            name: name.to_string(),
            params,
            return_type: Box::new(return_type),
        }
    }

    fn literal(value: &str, int_type: IntType) -> TypedExpr {
        TypedExpr::IntLiteral(TypedIntLiteral {
            int_value: value.to_string(),
            int_type,
        })
    }

    /// `pub fn main() u8 { <body> }`, after the given externs
    fn program(externs: Vec<FuncProto>, body: Vec<TypedStatement>) -> TypedProgram {
        let mut global_statements: Vec<_> = externs
            .into_iter()
            .map(TypedGlobalStatement::Extern)
            .collect();
        global_statements.push(TypedGlobalStatement::FuncDef(TypedFuncDef {
            proto: proto("main", &[], Type::Int(uint(8))),
            body,
        }));
        TypedProgram { global_statements }
    }

    fn declare(name: &str, var_type: Type, var_value: TypedExpr) -> TypedStatement {
        TypedStatement::VarDeclaration(TypedVarDeclaration {
            var_name: name.to_string(),
            var_type,
            var_value,
        })
    }

    fn ret_zero() -> TypedStatement {
        TypedStatement::Return(Some(literal("0", uint(8))))
    }

    #[test]
    fn no_current_function_before_building_one() {
        let compiler = Compiler::new();
        assert_eq!(compiler.get_cur_function(), None);
    }

    #[test]
    #[should_panic(expected = "Integer widths must be between 1 and 8388608, not 0")]
    fn rejects_zero_width_integers() {
        let body = vec![
            declare("x", Type::Int(uint(0)), literal("0", uint(0))),
            ret_zero(),
        ];
        Compiler::new().compile(&program(vec![], body));
    }

    #[test]
    #[should_panic(expected = "Integer widths must be between 1 and 8388608, not 8388609")]
    fn rejects_integers_wider_than_llvm_allows() {
        let width = MAX_INT_WIDTH + 1;
        let body = vec![
            declare("x", Type::Int(uint(width)), literal("0", uint(width))),
            ret_zero(),
        ];
        Compiler::new().compile(&program(vec![], body));
    }

    #[test]
    #[should_panic(expected = "'void' can't be the type of a value")]
    fn rejects_void_variables() {
        let body = vec![declare("x", Type::Void, literal("0", uint(8))), ret_zero()];
        Compiler::new().compile(&program(vec![], body));
    }

    #[test]
    #[should_panic(expected = "'void' can't be the type of a value")]
    fn rejects_void_parameters() {
        let externs = vec![proto("takes_void", &[Type::Void], Type::Void)];
        Compiler::new().compile(&program(externs, vec![ret_zero()]));
    }

    #[test]
    #[should_panic(expected = "'void' can't be the type of a value")]
    fn rejects_loading_void_identifiers() {
        let void_x = TypedExpr::Identifier(TypedIdentifier {
            name: "x".to_string(),
            id_type: Type::Void,
        });
        let body = vec![
            declare("x", Type::Int(uint(8)), literal("0", uint(8))),
            TypedStatement::Return(Some(void_x)),
        ];
        Compiler::new().compile(&program(vec![], body));
    }

    #[test]
    #[should_panic(expected = "Expected an integer value")]
    fn rejects_storing_a_void_call_result() {
        let nothing = proto("nothing", &[], Type::Void);
        let call = TypedExpr::Call(TypedCall {
            function_name: "nothing".to_string(),
            function_proto: nothing.clone(),
            args: vec![],
        });
        let body = vec![declare("x", Type::Int(uint(8)), call), ret_zero()];
        Compiler::new().compile(&program(vec![nothing], body));
    }

    #[test]
    #[should_panic(expected = "Call to 'takes_u8' doesn't match its prototype")]
    fn rejects_calls_whose_prototype_doesnt_match_the_callee() {
        let takes_u8 = proto("takes_u8", &[Type::Int(uint(8))], Type::Void);
        let claimed = proto("takes_u8", &[Type::Int(uint(16))], Type::Void);
        let call = TypedCall {
            function_name: "takes_u8".to_string(),
            function_proto: claimed,
            args: vec![literal("1", uint(16))],
        };
        let body = vec![TypedStatement::Call(call), ret_zero()];
        Compiler::new().compile(&program(vec![takes_u8], body));
    }

    #[test]
    #[should_panic(expected = "type(LHS) != type(RHS)")]
    fn rejects_arithmetic_on_mismatched_types() {
        let sum = TypedExpr::Binary(TypedBinary {
            left: Box::new(literal("1", uint(8))),
            operator: BinaryOperator::Add,
            right: Box::new(literal("1", uint(16))),
            result_type: Type::Int(uint(8)),
        });
        let body = vec![TypedStatement::Return(Some(sum))];
        Compiler::new().compile(&program(vec![], body));
    }

    #[test]
    fn casts_use_the_operands_real_width() {
        // The sum is really a u16 but claims to be a u4, so trusting the claim would build an
        // invalid zext from u16 to u8, which LLVM would fold as a constant (undefined behaviour)
        let mislabeled_sum = TypedExpr::Binary(TypedBinary {
            left: Box::new(literal("300", uint(16))),
            operator: BinaryOperator::Add,
            right: Box::new(literal("1", uint(16))),
            result_type: Type::Int(uint(4)),
        });
        let cast = TypedExpr::Unary(TypedUnary {
            operator: UnaryOperator::Cast(Type::Int(uint(8))),
            operand: Box::new(mislabeled_sum),
            result_type: Type::Int(uint(8)),
        });
        let body = vec![TypedStatement::Return(Some(cast))];
        Compiler::new().compile(&program(vec![], body));
    }

    #[test]
    #[should_panic(expected = "Function 'main' is not valid")]
    fn the_verifier_rejects_other_invalid_programs() {
        // Returning a u16 from a function that returns u8 is invalid IR, but nothing undefined
        let body = vec![TypedStatement::Return(Some(literal("0", uint(16))))];
        Compiler::new().compile(&program(vec![], body));
    }

    #[test]
    #[should_panic(expected = "declared with 1 parameters but defined with 0")]
    fn rejects_definitions_with_a_different_parameter_count() {
        // compile() always defines a function with the prototype it registered, so this can only
        // be reached directly
        let mut compiler = Compiler::new();
        compiler.compile_func_proto(&proto("f", &[Type::Int(uint(8))], Type::Void));
        let definition = TypedFuncDef {
            proto: proto("f", &[], Type::Void),
            body: vec![TypedStatement::Return(None)],
        };
        compiler.compile_func_def(&definition);
    }

    #[test]
    #[should_panic(expected = "Cannot allocate a variable outside of a function")]
    fn rejects_allocating_outside_a_function() {
        Compiler::new().create_alloca("x", &Type::Int(uint(8)));
    }

    #[test]
    #[should_panic(expected = "Cannot load a variable outside of a function")]
    fn rejects_loading_outside_a_function() {
        let id = TypedIdentifier {
            name: "x".to_string(),
            id_type: Type::Int(uint(8)),
        };
        Compiler::new().compile_identifier(&id);
    }
}
