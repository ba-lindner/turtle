use crate::SymbolTable;

use super::{ArgList, BiOperator, Block, Expr, Narrate as _, Variable};

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
    Print(Expr),
    IfBranch(Expr, Block),
    IfElseBranch(Expr, Block, Block),
    DoLoop(Expr, Block),
    CounterLoop {
        counter: Variable,
        from: Expr,
        up: bool,
        to: Expr,
        step: Option<Expr>,
        body: Block,
    },
    WhileLoop(Expr, Block),
    RepeatLoop(Expr, Block),
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
                    op => unreachable!("cannot use {op} as statement"),
                }
            }
            Statement::Mark => println!("marked current location"),
            Statement::MoveMark(draw) => {
                println!("{} to last mark", if *draw { "walked" } else { "jumped" })
            }
            Statement::Print(txt) => println!("printed {}", txt.narrate(symbols)),
            Statement::IfBranch(_, _) => println!("finished if"),
            Statement::IfElseBranch(_, _, _) => println!("finished if-else"),
            Statement::DoLoop(_, _) => println!("finished do loop"),
            Statement::CounterLoop { .. } => println!("finished counter loop"),
            Statement::WhileLoop(_, _) => println!("finished while loop"),
            Statement::RepeatLoop(_, _) => println!("finished repeat loop"),
        }
    }
}
