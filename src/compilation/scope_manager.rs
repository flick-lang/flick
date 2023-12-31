use crate::lexing::token::Type;
use crate::parsing::ast::FuncProto;
use llvm_sys::prelude::LLVMValueRef;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
pub struct Var {
    pub var_type: Type,
    pub value: LLVMValueRef,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub proto: FuncProto,
    pub value: LLVMValueRef,
}

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

    pub fn enter_scope(&mut self) {
        self.vars.push(HashMap::new());
        self.funcs.push(HashMap::new());
    }

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

    pub fn get_var(&self, name: &str) -> Option<&Var> {
        self.vars.iter().rev().find_map(|s| s.get(name))
    }

    pub fn set_var(&mut self, name: &str, var_type: Type, value: LLVMValueRef) {
        let cur_scope = self.vars.last_mut().unwrap();
        cur_scope.insert(name.to_string(), Var { var_type, value });
    }

    pub fn get_func(&self, name: &str) -> Option<&Func> {
        self.funcs.iter().rev().find_map(|s| s.get(name))
    }

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
