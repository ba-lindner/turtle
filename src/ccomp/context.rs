use std::collections::HashMap;

/// A compilation context
///
/// A context that records whether variables have
/// been initialized or not aswell as a loop counter
/// used to generate names for loop variables.
pub struct Context {
    /// Variables available or declared in this context
    vars: HashMap<usize, bool>,
    /// Loop counter
    ///
    /// see [`Self::loop_index()`] for details
    loops: usize,
    /// Nesting depth of the context
    ///
    /// This is `0` for function contexts
    /// and a higher value for control structures
    pub nesting: usize,
}

impl Context {
    /// Create a new empty context
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            loops: 0,
            nesting: 0,
        }
    }

    /// Record a variable that was generated
    pub fn insert(&mut self, name: usize, initialized: bool) {
        self.vars.insert(name, initialized);
    }

    /// Check if a variable should be declared
    ///
    /// For contexts that represent control structures
    /// (such as if/else or loops), this will always return true.
    /// This is required as all (local) variables have function-wide
    /// scope and thus must be declared outside of any control structures.  
    pub fn has_var(&mut self, name: usize) -> bool {
        let res = self.vars.contains_key(&name);
        if self.nesting != 0 && !res {
            self.vars.insert(name, false);
            return true;
        }
        res
    }

    /// List uninitialized variables
    ///
    /// Again, behaviour differs for function and inner contexts.
    /// As only functions may declare variables, only those contexts return
    /// any values from this function.
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

    /// Get the next loop index
    ///
    /// `do <x> times ... done` loops require an additional variable that
    /// is not present in the turtle program. To avoid multiple variables with the
    /// same name in the C output, a counter is used and increased for each loop.
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
