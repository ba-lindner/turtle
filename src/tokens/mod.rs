use std::fmt::{Display, Write as _};

use crate::{
    prog::{CalcDef, PathDef},
    Pos, SymbolTable,
};

use self::predef_vars::PredefVar;

pub mod keywords;
pub mod predef_vars;

pub type ArgList = Vec<Expr>;
pub type ArgDefList = Vec<usize>;
pub type Statements = Vec<Pos<Statement>>;

pub trait Narrate {
    fn narrate_buf(&self, symbols: &SymbolTable, buf: &mut String) -> std::fmt::Result;

    fn narrate(&self, symbols: &SymbolTable) -> String {
        let mut buf = String::new();
        let _ = self.narrate_buf(symbols, &mut buf);
        buf
    }
}

impl Narrate for [Expr] {
    fn narrate_buf(&self, symbols: &SymbolTable, buf: &mut String) -> std::fmt::Result {
        for (idx, expr) in self.iter().enumerate() {
            if idx > 0 {
                let _ = write!(buf, ", ");
                let _ = expr.narrate_buf(symbols, buf);
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum ParseToken {
    PathDef(PathDef),
    CalcDef(CalcDef),
    StartBlock(Statements),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    MoveDist {
        dist: Expr,
        draw: bool,
        back: bool,
    },
    MoveHome(bool),
    Turn {
        left: bool,
        by: Expr,
    },
    Direction(Expr),
    Color(Expr, Expr, Expr),
    Clear,
    Stop,
    Finish,
    PathCall(usize, ArgList),
    Store(Expr, Variable),
    Calc {
        var: Variable,
        val: Expr,
        op: BiOperator,
    },
    Mark,
    MoveMark(bool),
    IfBranch(Cond, Statements),
    IfElseBranch(Cond, Statements, Statements),
    DoLoop(Expr, Statements),
    CounterLoop {
        counter: Variable,
        from: Expr,
        up: bool,
        to: Expr,
        step: Option<Expr>,
        body: Statements,
    },
    WhileLoop(Cond, Statements),
    RepeatLoop(Cond, Statements),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum StmtKind {
    Any,
    Turtle,
    Draw,
}

impl Statement {
    pub fn kind(&self) -> StmtKind {
        match self {
            Self::MoveDist { draw: true, .. } | Self::MoveHome(true) | Self::MoveMark(true) => {
                StmtKind::Draw
            }
            Self::Clear
            | Self::Direction(_)
            | Self::MoveDist { .. }
            | Self::MoveHome(_)
            | Self::MoveMark(_)
            | Self::Turn { .. } => StmtKind::Turtle,
            _ => StmtKind::Any,
        }
    }

    pub fn narrate(&self, symbols: &SymbolTable) {
        match self {
            Statement::MoveDist { dist, draw, back } => {
                println!(
                    "{} {}by {}",
                    if *draw { "walked" } else { "jumped" },
                    if *back { "back " } else { "" },
                    dist.narrate(symbols)
                )
            }
            Statement::MoveHome(draw) => {
                println!("{} home", if *draw { "walked" } else { "jumped" })
            }
            Statement::Turn { left, by } => {
                println!(
                    "turned {} by {}",
                    if *left { "left" } else { "right" },
                    by.narrate(symbols)
                )
            }
            Statement::Direction(expr) => {
                println!("set direction to {}", expr.narrate(symbols))
            }
            Statement::Color(r, g, b) => {
                println!(
                    "set color to ({}, {}, {})",
                    r.narrate(symbols),
                    g.narrate(symbols),
                    b.narrate(symbols),
                )
            }
            Statement::Clear => println!("cleared screen"),
            Statement::Stop => println!("stopped turtle"),
            Statement::Finish => println!("finished drawing"),
            Statement::PathCall(id, args) => {
                println!(
                    "started path {}({})",
                    symbols
                        .get_index(*id)
                        .expect("missing path in symbol table")
                        .0,
                    args.narrate(symbols),
                )
            }
            Statement::Store(expr, var) => {
                println!(
                    "stored {} to {}",
                    expr.narrate(symbols),
                    var.narrate(symbols),
                )
            }
            Statement::Calc { var, val, op } => {
                let var = var.narrate(symbols);
                let val = val.narrate(symbols);
                match op {
                    BiOperator::Add => println!("added {val} to {var}"),
                    BiOperator::Sub => println!("subtracted {val} from {var}"),
                    BiOperator::Mul => println!("multiplied {var} by {val}"),
                    BiOperator::Div => println!("divided {var} by {val}"),
                    BiOperator::Exp => unreachable!("cannot use ^ as statement"),
                }
            }
            Statement::Mark => println!("marked current location"),
            Statement::MoveMark(draw) => {
                println!("{} to last mark", if *draw { "walked" } else { "jumped" })
            }
            Statement::IfBranch(_, _) => println!("finished if"),
            Statement::IfElseBranch(_, _, _) => println!("finished if-else"),
            Statement::DoLoop(_, _) => println!("finished do loop"),
            Statement::CounterLoop { .. } => println!("finished counter loop"),
            Statement::WhileLoop(_, _) => println!("finished while loop"),
            Statement::RepeatLoop(_, _) => println!("finished repeat loop"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Variable {
    Local(usize),
    Global(usize),
    GlobalPreDef(PredefVar),
}

impl Narrate for Variable {
    fn narrate_buf(&self, symbols: &SymbolTable, buf: &mut String) -> std::fmt::Result {
        match self {
            Variable::Local(id) => write!(
                buf,
                "{}",
                symbols
                    .get_index(*id)
                    .expect("missing variable in symbol table")
                    .0
            ),
            Variable::Global(id) => write!(
                buf,
                "@{}",
                symbols
                    .get_index(*id)
                    .expect("missing variable in symbol table")
                    .0
            ),
            Variable::GlobalPreDef(pdv) => write!(buf, "@{}", pdv.get_str()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Const(f64),
    Variable(Variable),
    BiOperation(Box<Expr>, BiOperator, Box<Expr>),
    Negate(Box<Expr>),
    Absolute(Box<Expr>),
    Bracket(Box<Expr>),
    FuncCall(PredefFunc, ArgList),
    CalcCall(usize, ArgList),
}

impl Narrate for Expr {
    fn narrate_buf(&self, symbols: &SymbolTable, buf: &mut String) -> std::fmt::Result {
        match self {
            Expr::Const(val) => write!(buf, "{val}"),
            Expr::Variable(var) => var.narrate_buf(symbols, buf),
            Expr::BiOperation(lhs, op, rhs) => {
                let _ = lhs.narrate_buf(symbols, buf);
                let _ = write!(buf, "{op}");
                rhs.narrate_buf(symbols, buf)
            }
            Expr::Negate(expr) => {
                let _ = write!(buf, "-");
                expr.narrate_buf(symbols, buf)
            }
            Expr::Absolute(expr) => {
                let _ = write!(buf, "|");
                let _ = expr.narrate_buf(symbols, buf);
                write!(buf, "|")
            }
            Expr::Bracket(expr) => {
                let _ = write!(buf, "(");
                let _ = expr.narrate_buf(symbols, buf);
                write!(buf, ")")
            }
            Expr::FuncCall(pdf, args) => {
                let _ = write!(buf, "{pdf}(");
                let _ = args.narrate_buf(symbols, buf);
                write!(buf, ")")
            }
            Expr::CalcCall(id, args) => {
                let _ = write!(
                    buf,
                    "{}(",
                    symbols
                        .get_index(*id)
                        .expect("missing calc in symbol table")
                        .0
                );
                let _ = args.narrate_buf(symbols, buf);
                write!(buf, ")")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BiOperator {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
}

impl BiOperator {
    pub fn calc(&self, val1: f64, val2: f64) -> f64 {
        match self {
            Self::Add => val1 + val2,
            Self::Sub => val1 - val2,
            Self::Mul => val1 * val2,
            Self::Div => {
                if val2 == 0.0 {
                    eprintln!("oh no: maths just broke");
                    0.0
                } else {
                    val1 / val2
                }
            }
            Self::Exp => val1.powf(val2),
        }
    }
}

impl Display for BiOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BiOperator::Add => write!(f, "+"),
            BiOperator::Sub => write!(f, "-"),
            BiOperator::Mul => write!(f, "*"),
            BiOperator::Div => write!(f, "/"),
            BiOperator::Exp => write!(f, "^"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Cond {
    Bracket(Box<Cond>),
    Cmp(Box<Expr>, CmpOperator, Box<Expr>),
    And(Box<Cond>, Box<Cond>),
    Or(Box<Cond>, Box<Cond>),
    Not(Box<Cond>),
}

impl Narrate for Cond {
    fn narrate_buf(&self, symbols: &SymbolTable, buf: &mut String) -> std::fmt::Result {
        match self {
            Cond::Bracket(cond) => {
                let _ = write!(buf, "(");
                let _ = cond.narrate_buf(symbols, buf);
                write!(buf, ")")
            }
            Cond::Cmp(lhs, op, rhs) => {
                let _ = lhs.narrate_buf(symbols, buf);
                let _ = write!(buf, " {op} ");
                rhs.narrate_buf(symbols, buf)
            }
            Cond::And(lhs, rhs) => {
                let _ = lhs.narrate_buf(symbols, buf);
                let _ = write!(buf, " && ");
                rhs.narrate_buf(symbols, buf)
            }
            Cond::Or(lhs, rhs) => {
                let _ = lhs.narrate_buf(symbols, buf);
                let _ = write!(buf, " || ");
                rhs.narrate_buf(symbols, buf)
            }
            Cond::Not(cond) => {
                let _ = write!(buf, "!");
                cond.narrate_buf(symbols, buf)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CmpOperator {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    UnEqual,
}

impl CmpOperator {
    pub fn compare(&self, lhs: f64, rhs: f64) -> bool {
        match self {
            CmpOperator::Less => lhs < rhs,
            CmpOperator::LessEqual => lhs <= rhs,
            CmpOperator::Greater => lhs > rhs,
            CmpOperator::GreaterEqual => lhs >= rhs,
            CmpOperator::Equal => lhs == rhs,
            CmpOperator::UnEqual => lhs != rhs,
        }
    }
}

impl Display for CmpOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CmpOperator::Less => write!(f, "<"),
            CmpOperator::LessEqual => write!(f, "<="),
            CmpOperator::Greater => write!(f, ">"),
            CmpOperator::GreaterEqual => write!(f, ">="),
            CmpOperator::Equal => write!(f, "=="),
            CmpOperator::UnEqual => write!(f, "!="),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PredefFunc {
    Sin,
    Cos,
    Tan,
    Sqrt,
    Rand,
}

impl PredefFunc {
    pub fn calc(&self, args: &[f64]) -> f64 {
        use std::f64::consts::PI;
        match self {
            PredefFunc::Sin => (args[0] * PI / 180.0).sin(),
            PredefFunc::Cos => (args[0] * PI / 180.0).cos(),
            PredefFunc::Tan => (args[0] * PI / 180.0).tan(),
            PredefFunc::Sqrt => args[0].sqrt(),
            PredefFunc::Rand => args[0] + (args[1] - args[0]) * rand::random::<f64>(),
        }
    }
}

impl Display for PredefFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredefFunc::Sin => write!(f, "sin"),
            PredefFunc::Cos => write!(f, "cos"),
            PredefFunc::Tan => write!(f, "tan"),
            PredefFunc::Sqrt => write!(f, "sqrt"),
            PredefFunc::Rand => write!(f, "rand"),
        }
    }
}
