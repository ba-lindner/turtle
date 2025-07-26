use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use std::{ops::Deref, str::FromStr};

use crate::{
    ProgError, SymbolTable, TurtleError,
    debugger::FuncType,
    features::FeatureConf,
    pos::Span,
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

trait Otherwise {
    fn otherwise<T>(self, val: T) -> Result<T, Self>
    where
        Self: Sized;
}

impl<E> Otherwise for Vec<E> {
    fn otherwise<T>(self, val: T) -> Result<T, Self> {
        if self.is_empty() { Ok(val) } else { Err(self) }
    }
}

#[derive(Default)]
struct RawProg {
    paths: Vec<PathDef>,
    calcs: Vec<CalcDef>,
    main: Vec<Block>,
    params: Vec<Param>,
    key_event: Vec<PathDef>,
    mouse_event: Vec<PathDef>,
}

impl RawProg {
    fn parse(
        symbols: &mut SymbolTable,
        features: &mut FeatureConf,
        code: impl Iterator<Item = char>,
    ) -> Result<Self, TurtleError> {
        let ltokens = Lexer::new(symbols, features, code)
            .collect_tokens()
            .map_err(TurtleError::LexErrors)?;
        let items = Parser::new(symbols, features, ltokens)
            .collect_items()
            .map_err(TurtleError::ParseErrors)?;
        let mut this = Self::default();
        for item in items {
            match item {
                ParseToken::PathDef(path) => this.paths.push(path),
                ParseToken::CalcDef(calc) => this.calcs.push(calc),
                ParseToken::EventHandler(kind, handler) => match kind {
                    EventKind::Mouse => this.mouse_event.push(handler),
                    EventKind::Key => this.key_event.push(handler),
                },
                ParseToken::StartBlock(block) => this.main.push(block),
                ParseToken::Param(param) => this.params.push(param),
            }
        }
        Ok(this)
    }

    fn construct(
        self,
        symbols: SymbolTable,
        features: FeatureConf,
    ) -> Result<TProgram, Vec<ProgError>> {
        let mut errs = Vec::new();
        if self.main.len() > 1 {
            errs.push(ProgError::MultipleMains(
                self.main.iter().map(|b| b.begin).collect(),
            ));
        }
        if self.key_event.len() > 1 {
            errs.push(ProgError::MultipleFuncs(
                FuncType::Event(EventKind::Key),
                self.key_event.iter().map(|f| f.body.begin).collect(),
            ));
        }
        if self.mouse_event.len() > 1 {
            errs.push(ProgError::MultipleFuncs(
                FuncType::Event(EventKind::Mouse),
                self.mouse_event.iter().map(|f| f.body.begin).collect(),
            ));
        }
        let Some(main) = self.main.into_iter().next() else {
            errs.push(ProgError::MissingMain);
            return Err(errs);
        };
        errs.otherwise(TProgram {
            name: None,
            features,
            paths: self.paths,
            calcs: self.calcs,
            main,
            params: self.params,
            key_event: self.key_event.into_iter().next(),
            mouse_event: self.mouse_event.into_iter().next(),
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
        let raw = RawProg::parse(&mut symbols, &mut features, code.chars());
        if print_symbols && raw.is_err() {
            for (idx, (name, _)) in symbols.iter().enumerate() {
                println!("#{idx:<3} {name}");
            }
        }
        features.finalize();
        let mut this = raw?
            .construct(symbols, features)
            .map_err(TurtleError::ProgErrors)?;
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

    pub(crate) fn get_path(&self, name: usize) -> Result<MaybeRef<'_, PathDef>, ProgError> {
        let find = |p: &&PathDef| p.name == name;
        let ext = self.extensions.paths.read();
        if let Ok(path) = RwLockReadGuard::try_map(ext, |ps| ps.iter().find(find)) {
            return Ok(MaybeRef::Lock(path));
        }
        let def = self.paths.iter().find(find);
        if let Some(path) = def {
            return Ok(MaybeRef::Ref(path));
        }
        Err(ProgError::MissingDefinition(name))
    }

    pub(crate) fn get_calc(&self, name: usize) -> Result<MaybeRef<'_, CalcDef>, ProgError> {
        let find = |c: &&CalcDef| c.name == name;
        let ext = self.extensions.calcs.read();
        if let Ok(calc) = RwLockReadGuard::try_map(ext, |cs| cs.iter().find(find)) {
            return Ok(MaybeRef::Lock(calc));
        }
        let def = self.calcs.iter().find(find);
        if let Some(path) = def {
            return Ok(MaybeRef::Ref(path));
        }
        Err(ProgError::MissingDefinition(name))
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
        let ltokens = Lexer::new(&mut symbols, &mut features, code.chars())
            .collect_tokens()
            .map_err(TurtleError::LexErrors)?;
        let mut parser = Parser::new(&mut symbols, &mut features, ltokens);
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
    pub span: Span,
}
