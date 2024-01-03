use crate::lexing::token::Type;
use crate::parsing::ast::FuncProto;
use llvm_sys::prelude::LLVMValueRef;
use std::collections::HashMap;

/// Struct to store a variable's [Flick type](Type) as well as the corresponding [LLVM object][a].
///
/// [a]: LLVMValueRef
#[derive(Copy, Clone, Debug)]
pub struct Var {
    // TODO(tbreydo): Minor: do we need to derive Copy? why is it inconsistent with Func below?
    pub var_type: Type,
    pub value: LLVMValueRef,
}

/// Struct to store a function's [Flick prototype](FuncProto) as well as the corresponding
/// [LLVM object][a].
///
/// [a]: LLVMValueRef
#[derive(Clone, Debug)]
pub struct Func {
    pub proto: FuncProto,
    pub value: LLVMValueRef,
}

// TODO: Design: what kind of namespaces do we want? Should i64 foo be allowed inside fn foo() { ... }?
//  What about inside fn bar () { ... }?
/// This data structure manages namespaces/scopes for variables and functions.
///
/// For example, in the following code, there are two scopes, and it would be nice to
/// have an easy way to manage them during compilation.
///
/// ```text
/// i64 a = 3      // outer scope
/// if foo() {     // inner scope
///     i64 a = 2  // inner scope
/// }              // inner scope
/// print(a)       // outer scope
/// ```
///
/// Also, giving a variable the same name as a function is silly but should still work:
///
/// ```text
/// // file: namespaces.fl
///
/// fn foo() {
///     i64 foo = 2  
///     print(foo)
/// }
/// ```
///
/// # Example usage
///
/// Suppose the variable `x` is defined in more than one scope. Here's how the manager would
/// handle it:
///
/// ```
/// # use std::ptr;
/// # use std::ptr::null;
/// # use llvm_sys::LLVMValue;
/// # use llvm_sys::prelude::LLVMValueRef;
/// use flick::scope_manager::*;
/// use flick::token::Type;
///
/// # let inner_val: LLVMValueRef = ptr::null_mut();
/// # let outer_val: LLVMValueRef = ptr::null_mut();
///
/// // suppose outer_val and inner_val are different LLVMValueRefs:
///
/// let mut scope_manager = ScopeManager::new();
/// scope_manager.set_var("x", Type::Int { bit_width: 64}, outer_val);
/// scope_manager.get_var("x");  // outer x
///
/// // ...
///
/// scope_manager.enter_scope();  // enter inner scope
///
/// // ...
///
/// scope_manager.get_var("x");  // outer x
/// scope_manager.set_var("x", Type::Int { bit_width: 64}, inner_val);
/// scope_manager.get_var("x");  // inner x
///
/// // ...
///
/// scope_manager.exit_scope();  // back to outer scope
///
/// scope_manager.get_var("x");  // outer x
/// ```
pub struct ScopeManager {
    vars: Vec<HashMap<String, Var>>,
    funcs: Vec<HashMap<String, Func>>,
}

impl ScopeManager {
    pub fn new() -> Self {
        Self {
            vars: vec![HashMap::new()],
            funcs: vec![HashMap::new()],
        }
    }

    /// Pushes a new scope; old objects can be overwritten but will regain their
    /// value after [exit_scope()](ScopeManager::exit_scope()).
    pub fn enter_scope(&mut self) {
        self.vars.push(HashMap::new());
        self.funcs.push(HashMap::new());
    }

    /// Pops the current scope; re-enters the next-innermost scope.
    ///
    /// Note: if this function is called with just one scope on the stack, this function
    /// will panic.
    pub fn exit_scope(&mut self) {
        if self.vars.len() != self.funcs.len() {
            panic!("Vars and funcs scopes don't have the depth")
        }

        if self.vars.len() == 1 && self.funcs.len() == 1 {
            panic!("cannot exit the global scope")
        }

        self.vars.pop();
        self.funcs.pop();
    }

    /// Searches through all scopes (starting with the innermost scope) for a variable named `name`.
    pub fn get_var(&self, name: &str) -> Option<&Var> {
        self.vars.iter().rev().find_map(|s| s.get(name))
    }

    /// Sets a variable named `name` in the current scope.
    pub fn set_var(&mut self, name: &str, var_type: Type, value: LLVMValueRef) {
        let cur_scope = self.vars.last_mut().unwrap();
        cur_scope.insert(name.to_string(), Var { var_type, value });
    }

    // TODO: Quick fix: if LLVMValueRef hashes then we should use that for lookup.
    //  Take a look at get_func() usage and see how hard we work to convert ValueRef to name.
    /// Searches through all scopes (starting with the innermost scope) for a function named `name`.
    pub fn get_func(&self, name: &str) -> Option<&Func> {
        self.funcs.iter().rev().find_map(|s| s.get(name))
    }

    /// Sets a function named `name` in the current scope.
    pub fn set_func(&mut self, name: &str, proto: FuncProto, value: LLVMValueRef) {
        let cur_scope = self.funcs.last_mut().unwrap();
        cur_scope.insert(name.to_string(), Func { proto, value });
    }
}

/// As suggested by Clippy's [new_without_default][a], since [ScopeManager::new()] doesn't
/// take any arguments, ScopeManager should implement Default.
///
/// [a]: https://rust-lang.github.io/rust-clippy/master/index.html#/new_without_default
impl Default for ScopeManager {
    fn default() -> Self {
        ScopeManager::new()
    }
}
