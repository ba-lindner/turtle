use std::{
    collections::HashMap,
    f64::consts::PI,
    fmt::{Display, Write as _},
};

use crate::{
    pos::FilePos,
    prog::{CalcDef, PathDef},
    Pos, SymbolTable, TurtleError,
};

pub use expr::{BiOperator, Expr, ExprKind, TypeError, UnOperator};
pub use keywords::Keyword;
pub use predef_vars::PredefVar;
pub use statements::{Statement, StmtKind};
pub use values::{ValType, Value};

mod expr;
mod keywords;
mod predef_vars;
mod statements;
mod values;

pub type ArgList = Vec<Expr>;
pub type ArgDefList = Vec<(usize, ValType)>;

#[derive(Debug, PartialEq)]
pub struct Block {
    pub begin: FilePos,
    pub statements: Vec<Pos<Statement>>,
}

pub trait Narrate {
    fn narrate_buf(&self, symbols: &SymbolTable, buf: &mut String);

    fn narrate(&self, symbols: &SymbolTable) -> String {
        let mut buf = String::new();
        self.narrate_buf(symbols, &mut buf);
        buf
    }
}

impl Narrate for [Expr] {
    fn narrate_buf(&self, symbols: &SymbolTable, buf: &mut String) {
        for (idx, expr) in self.iter().enumerate() {
            if idx > 0 {
                let _ = write!(buf, ", ");
                expr.narrate_buf(symbols, buf);
            }
        }
    }
}

#[derive(Debug)]
pub enum ParseToken {
    PathDef(PathDef),
    CalcDef(CalcDef),
    StartBlock(Block),
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub pos: FilePos,
    pub kind: VariableKind,
}

#[derive(Debug, PartialEq)]
pub enum VariableKind {
    Local(usize, ValType),
    Global(usize, ValType),
    GlobalPreDef(PredefVar),
}

impl VariableKind {
    pub fn at(self, pos: FilePos) -> Variable {
        Variable { pos, kind: self }
    }
}

impl Variable {
    pub fn val_type(
        &mut self,
        locals: &mut HashMap<usize, ValType>,
        globals: &mut HashMap<usize, ValType>,
    ) -> Result<(ValType, bool, bool), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeError(e, self.pos);
        match &mut self.kind {
            VariableKind::Local(idx, vt) => {
                if let Some(l) = locals.get(idx) {
                    if *vt == ValType::Any {
                        *vt = *l;
                    } else {
                        vt.assert(*l).map_err(e_map)?;
                    }
                }
                Ok((*vt, *vt != ValType::Any, true))
            }
            VariableKind::Global(idx, vt) => {
                if let Some(g) = globals.get(idx) {
                    if *vt == ValType::Any {
                        *vt = *g;
                    } else {
                        vt.assert(*g).map_err(e_map)?;
                    }
                }
                Ok((*vt, true, *vt != ValType::Any))
            }
            VariableKind::GlobalPreDef(pdv) => Ok((pdv.val_type(), true, true)),
        }
    }

    pub fn expect_type(
        &mut self,
        ty: ValType,
        locals: &mut HashMap<usize, ValType>,
        globals: &mut HashMap<usize, ValType>,
    ) -> Result<(), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeError(e, self.pos);
        match &mut self.kind {
            VariableKind::Local(idx, vt) => {
                if *vt == ValType::Any {
                    if let Some(l) = locals.get(idx) {
                        l.assert(ty).map_err(e_map)?;
                        *vt = *l;
                    } else {
                        locals.insert(*idx, ty);
                        *vt = ty;
                    }
                } else {
                    vt.assert(ty).map_err(e_map)?;
                    if let Some(l) = locals.get(idx) {
                        vt.assert(*l).map_err(e_map)?;
                    }
                }
            }
            VariableKind::Global(idx, vt) => {
                if *vt == ValType::Any {
                    if let Some(g) = globals.get(idx) {
                        g.assert(ty).map_err(e_map)?;
                        *vt = *g;
                    } else {
                        globals.insert(*idx, ty);
                        *vt = ty;
                    }
                } else {
                    vt.assert(ty).map_err(e_map)?;
                    if let Some(g) = globals.get(idx) {
                        vt.assert(*g).map_err(e_map)?;
                    }
                }
            }
            VariableKind::GlobalPreDef(pdv) => pdv.val_type().assert(ty).map_err(e_map)?,
        }
        Ok(())
    }
}

impl Narrate for Variable {
    fn narrate_buf(&self, symbols: &SymbolTable, buf: &mut String) {
        let _ = match &self.kind {
            VariableKind::Local(id, _) => write!(
                buf,
                "{}",
                symbols
                    .get_index(*id)
                    .expect("missing variable in symbol table")
                    .0
            ),
            VariableKind::Global(id, _) => write!(
                buf,
                "@{}",
                symbols
                    .get_index(*id)
                    .expect("missing variable in symbol table")
                    .0
            ),
            VariableKind::GlobalPreDef(pdv) => write!(buf, "@{}", pdv.get_str()),
        };
    }
}

macro_rules! predef_funcs {
    ($($func:ident as $str:literal ($($arg:ident : $ty:ty),+) -> $ret:ty = $res:expr,)+) => {
        #[derive(Debug, PartialEq)]
        pub enum PredefFunc {
            $($func,)+
        }

        impl PredefFunc {
            pub fn eval(&self, args: &[Value]) -> Value {
                let mut idx = 0;
                match self {
                    $(
                        Self::$func => {
                            $(
                                let $arg: $ty = (&args[idx]).into();
                                #[allow(unused_assignments)]
                                { idx += 1; }
                            )+
                            $res.into()
                        }
                    )+
                }
            }

            pub fn parse(kw: Keyword) -> Option<Self> {
                match kw {
                    $(
                        Keyword::$func => Some(Self::$func),
                    )+
                    _ => None,
                }
            }

            pub fn args(&self) -> Vec<ValType> {
                match self {
                    $(
                        Self::$func => vec![
                            $(<$ty as Default>::default().into(),)+
                        ],
                    )+
                }
            }

            pub fn ret_type(&self) -> ValType {
                match self {
                    $(
                        Self::$func => <$ret as Default>::default().into(),
                    )+
                }
            }
        }

        impl Display for PredefFunc {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$func => write!(f, $str),
                    )+
                }
            }
        }
    };
}

predef_funcs! {
    Sin as "sin" (a: f64) -> f64 = (a * PI / 180.0).sin(),
    Cos as "cos" (a: f64) -> f64 = (a * PI / 180.0).cos(),
    Tan as "tan" (a: f64) -> f64 = (a * PI / 180.0).tan(),
    Sqrt as "sqrt" (x: f64) -> f64 = x.sqrt(),
    Rand as "rand" (min: f64, max: f64) -> f64 = min + (max - min) * rand::random::<f64>(),
}
