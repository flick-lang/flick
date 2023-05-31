use llvm_sys::prelude::LLVMValueRef;
use std::collections::HashMap;

pub struct ScopeManager {
    scopes: Vec<HashMap<String, LLVMValueRef>>,
}

impl ScopeManager {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() == 1 {
            panic!("cannot exit the global scope")
        }
        self.scopes.pop().unwrap();
    }

    pub fn get_var(&self, name: &str) -> Option<LLVMValueRef> {
        self.scopes.iter().rev().find_map(|s| s.get(name)).copied()
    }

    pub fn set_var(&mut self, name: &str, value: LLVMValueRef) {
        let cur_scope = self.scopes.last_mut().unwrap();
        cur_scope.insert(name.to_string(), value);
    }
}
