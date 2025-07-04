use std::{
    f64::consts::PI,
    fmt::{Display, Write as _},
};

use crate::{
    Pos, SymbolTable,
    pos::FilePos,
    prog::{CalcDef, PathDef},
};

pub use expr::{BiOperator, Expr, ExprKind, UnOperator};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventKind {
    Mouse,
    Key,
}

impl Display for EventKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EventKind::Mouse => write!(f, "mouse"),
            EventKind::Key => write!(f, "key"),
        }
    }
}

#[derive(Debug)]
pub enum ParseToken {
    PathDef(PathDef),
    CalcDef(CalcDef),
    EventHandler(EventKind, PathDef),
    StartBlock(Block),
    Param(usize, Value),
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub pos: FilePos,
    pub kind: VariableKind,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    ($($func:ident ($($arg:ident : $ty:ty),+) -> $ret:ty = $res:expr,)+) => {
        #[derive(Debug, PartialEq)]
        pub enum PredefFunc {
            $($func,)+
        }

        impl PredefFunc {
            pub fn eval(&self, args: &[Value]) -> Value {
                let mut args = args.iter();
                match self {
                    $(Self::$func => {
                        $(let $arg: $ty = args.next().unwrap().into();)+
                        $res.into()
                    })+
                }
            }

            pub fn parse(kw: Keyword) -> Option<Self> {
                match kw {
                    $(Keyword::$func => Some(Self::$func),)+
                    _ => None,
                }
            }

            pub fn args(&self) -> Vec<ValType> {
                match self {
                    $(Self::$func => vec![
                        $(<$ty as Default>::default().into(),)+
                    ],)+
                }
            }

            pub fn ret_type(&self) -> ValType {
                match self {
                    $(Self::$func => <$ret as Default>::default().into(),)+
                }
            }
        }

        impl Display for PredefFunc {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$func => Keyword::$func.fmt(f),)+
                }
            }
        }
    };
}

predef_funcs! {
    Sin (a: f64) -> f64 = (a * PI / 180.0).sin(),
    Cos (a: f64) -> f64 = (a * PI / 180.0).cos(),
    Tan (a: f64) -> f64 = (a * PI / 180.0).tan(),
    Sqrt (x: f64) -> f64 = x.sqrt(),
    Rand (min: f64, max: f64) -> f64 = min + (max - min) * rand::random::<f64>(),
    Substr (s: &str, start: f64, end: f64) -> &str = s[start as usize .. end as usize],
    Strlen (s: &str) -> f64 = s.len() as f64,
    Arctan (a: f64) -> f64 = a.atan() * 180.0 / PI,
}
