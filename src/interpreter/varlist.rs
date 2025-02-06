pub struct VarList {
    vars: Vec<f64>,
}

impl VarList {
    pub fn new() -> Self {
        Self { vars: Vec::new() }
    }

    pub fn get_var(&mut self, id: usize) -> f64 {
        if self.vars.len() <= id {
            self.vars.append(&mut vec![0.0; id - self.vars.len() + 1]);
        }
        self.vars[id]
    }

    pub fn set_var(&mut self, id: usize, val: f64) {
        if self.vars.len() <= id {
            self.vars.append(&mut vec![0.0; id - self.vars.len() + 1]);
        }
        self.vars[id] = val;
    }
}
