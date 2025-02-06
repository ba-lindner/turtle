use std::collections::HashMap;

pub struct Context {
    vars: HashMap<usize, bool>,
    loops: usize,
    pub nesting: usize,
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            loops: 0,
            nesting: 0,
        }
    }

    pub fn insert(&mut self, name: usize, initialized: bool) {
        self.vars.insert(name, initialized);
    }

    pub fn has_var(&mut self, name: usize) -> bool {
        let res = self.vars.get(&name).is_some();
        if self.nesting != 0 && !res {
            self.vars.insert(name, false);
            return true;
        }
        res
    }

    pub fn get_new(&self) -> Vec<usize> {
        if self.nesting == 0 {
            self.vars
                .iter()
                .filter_map(|(&name, &init)| (!init).then_some(name))
                .collect()
        } else {
            Vec::new()
        }
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
            nesting: self.nesting,
        }
    }
}
