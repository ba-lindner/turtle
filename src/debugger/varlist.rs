use std::collections::HashMap;

use crate::tokens::{ValType, Value};

#[derive(Debug, Default, Clone, PartialEq)]
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

    pub fn iter(&self) -> <&VarList as IntoIterator>::IntoIter {
        self.into_iter()
    }
}

impl<'a> IntoIterator for &'a VarList {
    type Item = (&'a usize, &'a Value);
    type IntoIter = std::collections::hash_map::Iter<'a, usize, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.vars.iter()
    }
}
