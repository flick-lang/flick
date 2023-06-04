use crate::lexer::token::Type;
use crate::parser::ast::FuncProto;
use llvm_sys::prelude::LLVMValueRef;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
struct Var {
    var_type: Type,
    value: LLVMValueRef,
}

#[derive(Clone, Debug)]
struct Func {
    proto: FuncProto,
    value: LLVMValueRef,
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

    fn get_var(&self, name: &str) -> Option<&Var> {
        self.vars.iter().rev().find_map(|s| s.get(name))
    }

    pub fn get_var_value(&self, name: &str) -> Option<LLVMValueRef> {
        self.get_var(name).map(|v| v.value)
    }

    pub fn get_var_type(&self, name: &str) -> Option<Type> {
        self.get_var(name).map(|v| v.var_type)
    }

    pub fn set_var(&mut self, name: &str, var_type: Type, value: LLVMValueRef) {
        let cur_scope = self.vars.last_mut().unwrap();
        cur_scope.insert(name.to_string(), Var { var_type, value });
    }

    fn get_func(&self, name: &str) -> Option<&Func> {
        self.funcs.iter().rev().find_map(|s| s.get(name))
    }

    pub fn get_func_value(&self, name: &str) -> Option<LLVMValueRef> {
        self.get_func(name).map(|f| f.value)
    }

    pub fn get_func_proto(&self, name: &str) -> Option<&FuncProto> {
        self.get_func(name).map(|f| &f.proto)
    }

    pub fn set_func(&mut self, name: &str, proto: FuncProto, value: LLVMValueRef) {
        let cur_scope = self.funcs.last_mut().unwrap();
        cur_scope.insert(name.to_string(), Func { proto, value });
    }
}
