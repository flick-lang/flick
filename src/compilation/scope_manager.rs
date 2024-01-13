use std::collections::HashMap;

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
/// use llvm_sys::core::{LLVMConstInt, LLVMIntType};
/// use llvm_sys::prelude::*;
/// use flick::scope_manager::*;
/// use flick::token::Type;
///
/// let outer_val: LLVMValueRef = unsafe { LLVMConstInt(LLVMIntType(64), 20345, 1) };
/// let inner_val: LLVMValueRef = unsafe { LLVMConstInt(LLVMIntType(64), 1999, 1) };
///
/// let mut scope_manager = ScopeManager::new();
///
/// assert!(scope_manager.get("x").is_none());
///
/// scope_manager.set("x", outer_val);
/// assert_eq!(scope_manager.get("x"), Some(&outer_val));
///
/// // ...
///
/// scope_manager.enter_scope();  // enter inner scope
///
/// // ...
///
/// assert_eq!(scope_manager.get("x"), Some(&outer_val));
/// scope_manager.set("x", inner_val);
/// assert_eq!(scope_manager.get("x"), Some(&inner_val));
///
/// // ...
///
/// scope_manager.exit_scope();  // back to outer scope
///
/// assert_eq!(scope_manager.get("x"), Some(&outer_val));
/// ```
pub struct ScopeManager<T> {
    values: Vec<HashMap<String, T>>,
}

impl<T> ScopeManager<T> {
    pub fn new() -> Self {
        Self {
            values: vec![HashMap::new()],
        }
    }

    /// Pushes a new scope; old objects can be overwritten but will regain their
    /// value after [exit_scope()](ScopeManager::exit_scope()).
    pub fn enter_scope(&mut self) {
        self.values.push(HashMap::new());
    }

    /// Pops the current scope; re-enters the next-innermost scope.
    ///
    /// Note: if this function is called with just one scope on the stack, this function
    /// will panic.
    pub fn exit_scope(&mut self) {
        if self.values.len() == 1 {
            panic!("cannot exit the global scope")
        }

        self.values.pop();
    }

    /// Searches through all scopes (starting with the innermost scope) for a value named `name`.
    pub fn get(&self, name: &str) -> Option<&T> {
        self.values.iter().rev().find_map(|s| s.get(name))
    }

    /// Sets a value named `name` in the current scope.
    pub fn set(&mut self, name: &str, value: T) {
        let cur_scope = self.values.last_mut().unwrap();
        // TODO: Remove to_string (by accepting references with lifetimes?)
        cur_scope.insert(name.to_string(), value);
    }
}

/// As suggested by Clippy's [new_without_default][a], since [ScopeManager::new()] doesn't
/// take any arguments, ScopeManager should implement Default.
///
/// [a]: https://rust-lang.github.io/rust-clippy/master/index.html#/new_without_default
impl<T> Default for ScopeManager<T> {
    fn default() -> Self {
        ScopeManager::new()
    }
}
