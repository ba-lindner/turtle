use std::fmt::Display;

use crate::{Pos, StepVariant};

use self::predef_vars::PredefVar;

pub mod keywords;
pub mod predef_vars;

pub type ArgList = Vec<Expr>;
pub type ArgDefList = Vec<usize>;
pub type Statements = Vec<Pos<Statement>>;

#[derive(Debug)]
pub enum ParseToken {
    PathDef(usize, ArgDefList, Statements),
    CalcDef(usize, ArgDefList, Statements, Expr),
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
        body: Statements
    },
    WhileLoop(Cond, Statements),
    RepeatLoop(Cond, Statements),
}

impl Statement {
    pub fn kind(&self) -> StepVariant {
        match self {
            Self::MoveDist { draw: true, .. } |
            Self::MoveHome(true) |
            Self::MoveMark(true) => StepVariant::Draw,
            Self::Clear |
            Self::Direction(_) |
            Self::MoveDist { .. } |
            Self::MoveHome(_) |
            Self::MoveMark(_) |
            Self::Turn { .. } => StepVariant::Turtle,
            _ => StepVariant::Statement,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Variable {
    Local(usize),
    Global(usize),
    GlobalPreDef(PredefVar),
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
            Self::Div => val1 / val2,
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
            PredefFunc::Rand => write!(f, "__ttl_rand"),
        }
    }
}
