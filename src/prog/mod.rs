use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use std::{ops::Deref, str::FromStr};

use crate::{
    Identified, SymbolTable, TurtleError,
    features::FeatureConf,
    tokens::{ArgDefList, Block, EventKind, Expr, ParseToken, ValType, Value},
};

use lexer::Lexer;
use parser::Parser;
pub use semcheck::TypeError;
#[cfg(feature = "examples")]
use turtle_examples::Example;

pub mod lexer;
mod optimization;
pub mod parser;
pub(crate) mod semcheck;
mod side_effects;

#[derive(Default)]
struct RawProg {
    paths: Vec<PathDef>,
    calcs: Vec<CalcDef>,
    main: Option<Block>,
    params: Vec<Param>,
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
            ParseToken::Param(name, value) => {
                self.params.push(Param { name, value });
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
            params: self.params,
            key_event: self.key_event,
            mouse_event: self.mouse_event,
            symbols: symbols.clone(),
            extensions: Extensions {
                symbols: RwLock::new(symbols),
                paths: RwLock::new(Vec::new()),
                calcs: RwLock::new(Vec::new()),
                key_event: RwLock::new(None),
                mouse_event: RwLock::new(None),
            },
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
    pub params: Vec<Param>,
    key_event: Option<PathDef>,
    mouse_event: Option<PathDef>,
    pub symbols: SymbolTable,
    pub extensions: Extensions,
}

impl TProgram {
    pub fn parse(
        code: &str,
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
        let mut this = Self::parse(&code, print_symbols, features)?;
        this.name = Some(file.to_string());
        Ok(this)
    }

    #[cfg(feature = "examples")]
    pub fn from_example(ex: &Example) -> Result<Self, TurtleError> {
        let mut this = Self::parse(ex.code, false, FeatureConf::default())?;
        this.name = Some(ex.name.to_string());
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

    pub(crate) fn get_path(&self, name: usize) -> Result<MaybeRef<'_, PathDef>, TurtleError> {
        let find = |p: &&PathDef| p.name == name;
        let ext = self.extensions.paths.read();
        if let Ok(path) = RwLockReadGuard::try_map(ext, |ps| ps.iter().find(find)) {
            return Ok(MaybeRef::Lock(path));
        }
        let def = self.paths.iter().find(find);
        if let Some(path) = def {
            return Ok(MaybeRef::Ref(path));
        }
        Err(TurtleError::MissingDefinition(name))
    }

    pub(crate) fn get_calc(&self, name: usize) -> Result<MaybeRef<'_, CalcDef>, TurtleError> {
        let find = |c: &&CalcDef| c.name == name;
        let ext = self.extensions.calcs.read();
        if let Ok(calc) = RwLockReadGuard::try_map(ext, |cs| cs.iter().find(find)) {
            return Ok(MaybeRef::Lock(calc));
        }
        let def = self.calcs.iter().find(find);
        if let Some(path) = def {
            return Ok(MaybeRef::Ref(path));
        }
        Err(TurtleError::MissingDefinition(name))
    }

    pub(crate) fn get_event(&self, kind: EventKind) -> Option<MaybeRef<'_, PathDef>> {
        let (ext, default) = match kind {
            EventKind::Mouse => (&self.extensions.mouse_event, &self.mouse_event),
            EventKind::Key => (&self.extensions.key_event, &self.key_event),
        };
        if let Ok(evt) = RwLockReadGuard::try_map(ext.read(), |e| e.as_ref()) {
            Some(MaybeRef::Lock(evt))
        } else {
            default.as_ref().map(MaybeRef::Ref)
        }
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
    ) -> Result<T, TurtleError> {
        let mut symbols = self.extensions.symbols.write();
        let mut features = self.features;
        let ltokens = Lexer::new(&mut symbols, &mut features, code.chars()).collect_tokens()?;
        let mut parser = Parser::new(&mut symbols, ltokens, &mut features);
        f(&mut parser)
    }
}

impl FromStr for TProgram {
    type Err = TurtleError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s, false, FeatureConf::default())
    }
}

pub enum MaybeRef<'r, T> {
    Lock(MappedRwLockReadGuard<'r, T>),
    Ref(&'r T),
}

impl<T> Deref for MaybeRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            MaybeRef::Lock(r) => r,
            MaybeRef::Ref(r) => r,
        }
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

#[derive(Debug)]
pub struct Extensions {
    pub symbols: RwLock<SymbolTable>,
    pub paths: RwLock<Vec<PathDef>>,
    pub calcs: RwLock<Vec<CalcDef>>,
    pub key_event: RwLock<Option<PathDef>>,
    pub mouse_event: RwLock<Option<PathDef>>,
}

#[derive(Debug)]
pub struct Param {
    pub name: usize,
    pub value: Value,
}
