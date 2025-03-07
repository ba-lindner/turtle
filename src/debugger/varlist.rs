use std::collections::HashMap;

use crate::{tokens::{ValType, Value}, SymbolTable};

pub struct VarList {
    vars: HashMap<usize, Value>,
}

impl VarList {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn get_var(&mut self, id: usize, ty: ValType) -> &Value {
        self.vars.entry(id).or_insert_with(|| ty.default())
    }

    pub fn set_var(&mut self, id: usize, val: Value) {
        self.vars.insert(id, val);
    }

    pub fn dump(&self, symbols: &SymbolTable, global: bool) {
        for (&id, val) in &self.vars {
            if global {
                print!("@");
            }
            if let Some((name, _)) = symbols.get_index(id) {
                println!("{name} = {val}");
            } else {
                println!("#{id} = {val}");
            }
        }
    }
}
