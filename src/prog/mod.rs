use crate::{
    tokens::{ArgDefList, Expr, ParseToken, Block},
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
    main: Option<Block>,
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

    fn finish(self, symbols: SymbolTable) -> Result<TProgram, TurtleError> {
        let Some(main) = self.main else {
            return Err(TurtleError::MissingMain);
        };
        Ok(TProgram {
            name: None,
            paths: self.paths,
            calcs: self.calcs,
            main,
            symbols,
        })
    }
}

/// A full and valid turtle program.
#[derive(Debug)]
pub struct TProgram {
    pub name: Option<String>,
    pub paths: Vec<PathDef>,
    pub calcs: Vec<CalcDef>,
    pub main: Block,
    pub symbols: SymbolTable,
}

impl TProgram {
    pub fn parse(code: String) -> Result<Self, TurtleError> {
        let mut symbols = SymbolTable::new();
        let ltokens = Lexer::new(&mut symbols, code.chars()).collect_tokens()?;
        let parser = Parser::new(&mut symbols, ltokens);
        let mut raw = RawProg::default();
        for item in parser {
            raw.insert_item(item?)?;
        }
        let this = raw.finish(symbols)?;
        this.check_idents()?;
        Ok(this)
    }

    pub fn from_file(file: &str) -> Result<Self, TurtleError> {
        let code = std::fs::read_to_string(file)?;
        let mut this = Self::parse(code)?;
        this.name = Some(file.to_string());
        Ok(this)
    }

    fn check_idents(&self) -> Result<(), TurtleError> {
        for (id, (_, kind)) in self.symbols.iter().enumerate() {
            match kind {
                Identified::Unknown => {
                    return Err(TurtleError::UnidentifiedIdentifier(id));
                }
                Identified::Path => {
                    self.get_path(id)?;
                }
                Identified::Calc => {
                    self.get_calc(id)?;
                }
                _ => {}
            }
        }
        Ok(())
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

    pub fn debugger<'p>(&'p self, args: &[String], kind: &str) -> Debugger<'p> {
        let title = if let Some(name) = &self.name {
            format!("Turtle {kind} - {name}")
        } else {
            format!("Turtle {kind}")
        };
        Debugger::new(self, args, &title)
    }

    pub fn interpret(&self, args: &[String]) {
        self.debugger(args, "Interpreter").interpret();
    }

    pub fn debug(&self, args: &[String], bp: &[String]) {
        self.debugger(args, "Debugger").run(bp);
    }
}

/// Path definition in turtle program
#[derive(Debug)]
pub struct PathDef {
    pub name: usize,
    pub args: ArgDefList,
    pub body: Block,
}

/// Calc definition in turtle program
#[derive(Debug)]
pub struct CalcDef {
    pub name: usize,
    pub args: ArgDefList,
    pub body: Block,
    pub ret: Expr,
}
