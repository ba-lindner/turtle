use std::collections::HashMap;

pub struct Context {
    vars: HashMap<usize, bool>,
    loops: usize,
}

impl Context {
    pub fn new() -> Self {
        Self { 
            vars: HashMap::new(),
            loops: 0,
        }
    }

    pub fn insert(&mut self, name: usize, initialized: bool) {
        self.vars.insert(name, initialized);
    }

    pub fn has_var(&self, name: usize) -> bool {
        self.vars.get(&name) == Some(&true)
    }

    pub fn get_new(&mut self) -> Vec<usize> {
        self.vars.iter_mut().filter(|(_, init)| !**init).map(|(name, _)| *name).collect()
    }

    pub fn loop_index(&mut self) -> usize {
        self.loops += 1;
        self.loops - 1
    }
}

impl Clone for Context {
    fn clone(&self) -> Self {
        Self {
            vars: self.vars.clone(),
            loops: self.loops,
        }
    }
}