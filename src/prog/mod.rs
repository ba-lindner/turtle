use crate::{
    debugger::ItpRunner, tokens::{ArgDefList, Block, Expr, ParseToken, ValType}, DebugRunner, Identified, SymbolTable, TurtleError
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
            ParseToken::StartBlock(block) => {
                if let Some(main) = &self.main {
                    return Err(TurtleError::MultipleMains(main.begin, block.begin));
                }
                self.main = Some(block);
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

    pub fn check_code(code: String, print_symbols: bool) {
        let mut symbols = SymbolTable::new();
        let pr_sym = |symbols: &SymbolTable| {
            if print_symbols {
                for (idx, (name, kind)) in symbols.iter().enumerate() {
                    println!("#{idx:<3} {name:<20} {kind}")
                }
            }
        };

        let ltokens = match Lexer::new(&mut symbols, code.chars()).collect_tokens() {
            Ok(tokens) => tokens,
            Err(why) => {
                eprintln!("{why}");
                pr_sym(&symbols);
                return;
            }
        };
        let parser = Parser::new(&mut symbols, ltokens);
        let mut raw = RawProg::default();
        for item in parser {
            match item {
                Ok(item) => if let Err(why) = raw.insert_item(item) {
                    eprintln!("{why}");
                    pr_sym(&symbols);
                    return;
                }
                Err(why) => {
                    eprintln!("{} at {}", *why, why.get_pos());
                    pr_sym(&symbols);
                    return;
                }
            }
        }
        let prog = match raw.finish(symbols) {
            Ok(prog) => prog,
            Err(why) => {
                eprintln!("{why}");
                return;
            }
        };
        if let Err(why) = prog.check_idents() {
            eprintln!("{why}");
            pr_sym(&prog.symbols);
            return;
        }
    }

    fn check_idents(&self) -> Result<(), TurtleError> {
        for (id, (_, kind)) in self.symbols.iter().enumerate() {
            match kind {
                Identified::Unknown => {
                    return Err(TurtleError::UnidentifiedIdentifier(id));
                }
                Identified::Path(args) => {
                    let path = self.get_path(id)?;
                    assert_eq!(path.args.len(), *args);
                }
                Identified::Calc(args) => {
                    let calc = self.get_calc(id)?;
                    assert_eq!(calc.args.len(), *args);
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

    pub fn title(&self, kind: &str) -> String {
        if let Some(name) = &self.name {
            format!("Turtle {kind} - {name}")
        } else {
            format!("Turtle {kind}")
        }
    }

    pub fn interpret(&self, args: &[String]) {
        ItpRunner::new(self, args, &self.title("Interpreter")).run();
    }

    pub fn debug(&self, args: &[String], breakpoints: &[String]) {
        let mut dbg = DebugRunner::new(self, args, &self.title("Debugger"));
        dbg.set_breakpoints(breakpoints);
        dbg.run();
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
    pub ret_ty: ValType,
    pub body: Block,
    pub ret: Expr,
}
