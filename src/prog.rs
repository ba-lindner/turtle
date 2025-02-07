use indexmap::IndexMap;

use crate::{tokens::{ArgDefList, Expr, Statements}, Debugger, Identified, TurtleError};

/// A full and valid turtle program.
#[derive(Debug)]
pub struct TProgram {
    pub paths: Vec<PathDef>,
    pub calcs: Vec<CalcDef>,
    pub main: Statements,
    pub idents: IndexMap<String, Identified>,
}

impl TProgram {
    pub(crate) fn get_path(&self, name: usize) -> Result<&PathDef, TurtleError> {
        self.paths
            .iter()
            .find(|path| path.name == name)
            .ok_or(TurtleError::MissingDefinition(name))
    }

    pub(crate) fn get_calc(&self, name: usize) -> Result<&CalcDef, TurtleError> {
        self.calcs
            .iter()
            .find(|calc| calc.name == name)
            .ok_or(TurtleError::MissingDefinition(name))
    }

    pub fn interpret(&self, args: &[String]) {
        Debugger::new(self, args, "Turtle Interpreter").interpret();
    }

    pub fn debug(&self, args: &[String], bp: &[String]) {
        Debugger::new(self, args, "Turtle Debugger").run(bp);
    }
}

/// Path definition in turtle program
#[derive(Debug)]
pub struct PathDef {
    pub name: usize,
    pub args: ArgDefList,
    pub body: Statements,
}

/// Calc definition in turtle program
#[derive(Debug)]
pub struct CalcDef {
    pub name: usize,
    pub args: ArgDefList,
    pub body: Statements,
    pub ret: Expr,
}
