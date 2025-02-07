use std::collections::HashMap;

use crate::SymbolTable;

pub struct VarList {
    vars: HashMap<usize, f64>,
}

impl VarList {
    pub fn new() -> Self {
        Self { vars: HashMap::new() }
    }

    pub fn get_var(&mut self, id: usize) -> f64 {
        *self.vars.entry(id).or_insert(0.0)
    }

    pub fn set_var(&mut self, id: usize, val: f64) {
        self.vars.insert(id, val);
    }

    pub fn dump(&self, symbols: &SymbolTable, global: bool) {
        for (&id, &val) in &self.vars {
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
