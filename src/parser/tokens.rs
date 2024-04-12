use std::fmt::Display;

pub type ArgList = Vec<Expr>;
pub type ArgDefList = Vec<usize>;
pub type Statements = Vec<Statement>;

#[derive(Debug)]
pub enum ParseToken {
    PathDef(usize, ArgDefList, Statements),
    CalcDef(usize, ArgDefList, Statements, Expr),
    StartBlock(Statements),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Walk(Expr),
    WalkBack(Expr),
    Jump(Expr),
    JumpBack(Expr),
    WalkHome,
    JumpHome,
    TurnLeft(Expr),
    TurnRight(Expr),
    Direction(Expr),
    Color(Expr, Expr, Expr),
    Clear,
    Stop,
    Finish,
    PathCall(usize, ArgList),
    Store(Expr, Variable),
    Add(Expr, Variable),
    Sub(Expr, Variable),
    Mul(Expr, Variable),
    Div(Expr, Variable),
    Mark,
    WalkMark,
    JumpMark,
    IfBranch(Cond, Statements),
    IfElseBranch(Cond, Statements, Statements),
    DoLoop(Expr, Statements),
    CounterLoop(Variable, Expr, bool, Expr, Option<Expr>, Statements),
    WhileLoop(Cond, Statements),
    RepeatLoop(Cond, Statements),
}

#[derive(Debug, PartialEq)]
pub enum Variable {
    Local(usize),
    Global(usize),
    GlobalPreDef(usize)
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Const(f64),
    Variable(Variable),
    BiOperation(Box<Expr>, BiOperator, Box<Expr>),
    UnOperation(UnOperator, Box<Expr>),
    Absolute(Box<Expr>),
    Bracket(Box<Expr>),
    FuncCall(PredefFunc, ArgList),
    CalcCall(usize, ArgList),
}

#[derive(Debug, PartialEq)]
pub enum BiOperator {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
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
pub enum UnOperator {
    Neg,
}

impl Display for UnOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOperator::Neg => write!(f, "-"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Cond {
    Bracket(Box<Cond>),
    Cmp(Box<Expr>, CmpOperator, Box<Expr>),
    And(Box<Cond>, Box<Cond>),
    Or(Box<Cond>, Box<Cond>),
    Not(Box<Cond>)
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