use std::{
    f64::consts::PI,
    fmt::Display,
    ops::{Deref, DerefMut},
};

use crate::{
    Disp, Spanned, SymbolTable,
    pos::{Positionable, Span},
    prog::{CalcDef, Param, PathDef},
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
    /// first token, used for errors
    pub begin: Span,
    /// full block
    pub full: Span,
    pub statements: Vec<Spanned<Statement>>,
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
    Param(Param),
}

#[derive(Debug, PartialEq)]
pub struct Variable(pub Spanned<VariableKind>);

impl Deref for Variable {
    type Target = Spanned<VariableKind>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Variable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum VariableKind {
    Local(usize, ValType),
    Global(usize, ValType),
    GlobalPreDef(PredefVar),
}

impl VariableKind {
    pub fn at(self, span: impl Into<Span>) -> Variable {
        Variable(self.with_span(span))
    }
}

impl Disp for Variable {
    fn disp(&self, f: &mut std::fmt::Formatter<'_>, symbols: &SymbolTable) -> std::fmt::Result {
        match &*self.0 {
            VariableKind::Local(id, _) => write!(
                f,
                "{}",
                symbols
                    .get_index(*id)
                    .expect("missing variable in symbol table")
                    .0
            ),
            VariableKind::Global(id, _) => write!(
                f,
                "@{}",
                symbols
                    .get_index(*id)
                    .expect("missing variable in symbol table")
                    .0
            ),
            VariableKind::GlobalPreDef(pdv) => write!(f, "@{}", pdv.get_str()),
        }
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
