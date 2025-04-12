use crate::{
    features::FeatureConf,
    tokens::{ArgDefList, Block, EventKind, Expr, ParseToken, ValType},
    Identified, SymbolTable, TurtleError,
};

use lexer::Lexer;
use parser::Parser;
pub use semcheck::TypeError;

pub mod lexer;
mod optimization;
pub mod parser;
mod semcheck;
mod side_effects;

#[derive(Default)]
struct RawProg {
    paths: Vec<PathDef>,
    calcs: Vec<CalcDef>,
    main: Option<Block>,
    key_event: Option<PathDef>,
    mouse_event: Option<PathDef>,
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
            ParseToken::EventHandler(kind, func) => {
                let curr = match kind {
                    EventKind::Mouse => &mut self.mouse_event,
                    EventKind::Key => &mut self.key_event,
                };
                if let Some(evt) = curr {
                    return Err(TurtleError::MultipleEventHandler(
                        kind,
                        evt.body.begin,
                        func.body.begin,
                    ));
                }
                *curr = Some(func);
            }
        }
        Ok(())
    }

    fn finish(self, symbols: SymbolTable, features: FeatureConf) -> Result<TProgram, TurtleError> {
        let Some(main) = self.main else {
            return Err(TurtleError::MissingMain);
        };
        Ok(TProgram {
            name: None,
            features,
            paths: self.paths,
            calcs: self.calcs,
            main,
            key_event: self.key_event,
            mouse_event: self.mouse_event,
            symbols,
        })
    }
}

/// A full and valid turtle program.
#[derive(Debug)]
pub struct TProgram {
    pub name: Option<String>,
    pub features: FeatureConf,
    pub paths: Vec<PathDef>,
    pub calcs: Vec<CalcDef>,
    pub main: Block,
    pub key_event: Option<PathDef>,
    pub mouse_event: Option<PathDef>,
    pub symbols: SymbolTable,
}

impl TProgram {
    pub fn parse(
        code: String,
        print_symbols: bool,
        mut features: FeatureConf,
    ) -> Result<Self, TurtleError> {
        let mut symbols = SymbolTable::new();
        let ltokens = Lexer::new(&mut symbols, &mut features, code.chars()).collect_tokens()?;
        if print_symbols {
            for (idx, (name, _)) in symbols.iter().enumerate() {
                println!("#{idx:<3} {name}");
            }
        }
        let parser = Parser::new(&mut symbols, ltokens, &mut features);
        let mut raw = RawProg::default();
        for item in parser {
            raw.insert_item(item?)?;
        }
        features.finalize();
        let mut this = raw.finish(symbols, features)?;
        this.semantic_check()?;
        Ok(this)
    }

    pub fn from_file(
        file: &str,
        print_symbols: bool,
        features: FeatureConf,
    ) -> Result<Self, TurtleError> {
        let code = std::fs::read_to_string(file)?;
        let mut this = Self::parse(code, print_symbols, features)?;
        this.name = Some(file.to_string());
        Ok(this)
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

    pub fn optimize(&mut self) {
        self.main.const_fold();
        for calc in &mut self.calcs {
            calc.body.const_fold();
        }
        for path in &mut self.paths {
            path.body.const_fold();
        }
        if let Some(evt) = &mut self.key_event {
            evt.body.const_fold();
        }
        if let Some(evt) = &mut self.mouse_event {
            evt.body.const_fold();
        }
    }

    pub fn with_parser<T>(
        &self,
        code: &str,
        f: impl FnOnce(&mut Parser) -> Result<T, TurtleError>,
    ) -> Result<(T, SymbolTable), TurtleError> {
        let mut symbols = self.symbols.clone();
        let mut features = self.features;
        let ltokens = Lexer::new(&mut symbols, &mut features, code.chars()).collect_tokens()?;
        let mut parser = Parser::new(&mut symbols, ltokens, &mut features);
        let t = f(&mut parser)?;
        Ok((t, symbols))
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
