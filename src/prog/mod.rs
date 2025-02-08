use crate::{
    tokens::{ArgDefList, Expr, ParseToken, Statements},
    Debugger, Identified, SymbolTable, TurtleError,
};

use lexer::Lexer;
use parser::Parser;

pub mod lexer;
pub mod parser;

#[derive(Default)]
struct RawProg {
    paths: Vec<PathDef>,
    calcs: Vec<CalcDef>,
    main: Option<Statements>,
}

impl RawProg {
    fn insert_item(&mut self, item: ParseToken) -> Result<(), TurtleError> {
        match item {
            ParseToken::PathDef(def) => self.paths.push(def),
            ParseToken::CalcDef(def) => self.calcs.push(def),
            ParseToken::StartBlock(stm) => {
                if self.main.is_some() {
                    return Err(TurtleError::MultipleMains);
                }
                self.main = Some(stm);
            }
        }
        Ok(())
    }
}

/// A full and valid turtle program.
#[derive(Debug)]
pub struct TProgram {
    pub paths: Vec<PathDef>,
    pub calcs: Vec<CalcDef>,
    pub main: Statements,
    pub symbols: SymbolTable,
}

impl TProgram {
    pub fn from_file(file: &str) -> Result<Self, TurtleError> {
        let mut symbols = SymbolTable::new();
        let mut lex = Lexer::from_file(&mut symbols, file)?;
        let ltokens = lex.collect_tokens()?;
        let parser = Parser::new(&mut symbols, ltokens);
        let mut raw = RawProg::default();
        for item in parser {
            raw.insert_item(item?)?;
        }
        let Some(main) = raw.main else {
            return Err(TurtleError::MissingMain);
        };
        let res = TProgram {
            paths: raw.paths,
            calcs: raw.calcs,
            main,
            symbols,
        };
        for (id, (_, kind)) in res.symbols.iter().enumerate() {
            match kind {
                Identified::Unknown => {
                    return Err(TurtleError::UnidentifiedIdentifier(id));
                }
                Identified::Path => {
                    res.get_path(id)?;
                }
                Identified::Calc => {
                    res.get_calc(id)?;
                }
                _ => {}
            }
        }
        Ok(res)
    }

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
