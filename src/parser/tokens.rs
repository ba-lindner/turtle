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
    FuncCall(usize, ArgList),
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

#[derive(Debug, PartialEq)]
pub enum UnOperator {
    Neg,
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