use std::{future::Future, pin::pin, sync::Arc, task::{Context, Poll, Wake, Waker}};

use clap::ValueEnum;

use crate::{Cond, Expr, FilePos, Statement, Statements, TProgram};

use super::{turtle::Turtle, varlist::VarList, window::Window, FloatExt};

// have a cat

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Default)]
pub enum StepVariant {
    /// every statement
    #[default]
    Statement,
    /// every turtle action
    /// 
    /// includes turns and jumps
    Turtle,
    /// every drawn line
    Draw,
}

struct TurtleWaker;

impl Wake for TurtleWaker {
    fn wake(self: std::sync::Arc<Self>) {}
}

struct TurtleFuture(bool);

impl Future for TurtleFuture {
    type Output = ();

    fn poll(mut self: std::pin::Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Self::Output> {
        if self.0 {
            Poll::Ready(())
        } else {
            self.0 = true;
            Poll::Pending
        }
    }
}

pub struct Debugger<'p, 'w> {
    prog: &'p TProgram,
    window: &'w Window,
    turtle: Turtle<'w>,
    step: StepVariant,
    breakpoints: Vec<FilePos>,
}

impl<'p, 'w> Debugger<'p, 'w> {
    pub fn new(prog: &'p TProgram, window: &'w Window, args: &[String]) -> Self {
        Self {
            prog,
            window,
            turtle: Turtle::new(window, args),
            step: StepVariant::Statement,
            breakpoints: Vec::new(),
        }
    }

    pub fn step(&mut self, step: StepVariant) {
        self.step = step;
    }

    pub fn breakpoints(&mut self, breakpoints: &[String]) {
        fn parse_pos(bp: &str) -> Option<FilePos> {
            let (line, col) = bp.split_once(",")?;
            Some(FilePos::new(line.parse().ok()?, col.parse().ok()?))
        }

        for bp in breakpoints {
            if let Some(pos) = parse_pos(bp) {
                self.breakpoints.push(pos);
            }
        }
    }

    fn statement_finished(&self, kind: StepVariant, last_pos: FilePos, now_pos: FilePos) -> TurtleFuture {
        for &bp in &self.breakpoints {
            if last_pos < bp && bp <= now_pos {
                return TurtleFuture(false);
            }
        }
        TurtleFuture(self.step > kind)
    }

    pub fn run(&mut self) {
        let waker: Waker = Arc::new(TurtleWaker).into();
        let mut ctx = Context::from_waker(&waker);
        let window = self.window;
        let mut fut = pin!(self.dbg_stmts(&self.prog.main));
        while let Poll::Pending = fut.as_mut().poll(&mut ctx) {
            window.wait_space_pressed();
        }
    }

    async fn dbg_stmts(&mut self, stmts: &Statements) {
        let fut = async {
            let mut last_pos = FilePos::default();
            for stmt in stmts {
                self.dbg_stmt(stmt).await;
                let now_pos = stmt.get_pos();
                self.statement_finished(stmt.kind(), last_pos, now_pos).await;
                last_pos = now_pos;
            }
        };
        Box::pin(fut).await
    }

    async fn dbg_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::MoveDist { dist, draw, back } => {
                let dist = self.dbg_expr(dist).await;
                self.turtle.move_dist(dist, *back, *draw);
            }
            Statement::MoveHome(draw) => self.turtle.move_home(*draw),
            Statement::Turn { left, by } => {
                let angle = self.dbg_expr(by).await;
                self.turtle.set_dir(self.turtle.get_dir() + angle.neg(*left));
            }
            Statement::Direction(expr) => {
                let new_dir = self.dbg_expr(expr).await;
                self.turtle.set_dir(new_dir);
            }
            Statement::Color(ex1, ex2, ex3) => {
                let r = self.dbg_expr(ex1).await;
                let g = self.dbg_expr(ex2).await;
                let b = self.dbg_expr(ex3).await;
                self.turtle.set_col(r, g, b);
            }
            Statement::Clear => self.turtle.clear(),
            Statement::Stop => {
                // self.stopped = true;
                println!("halt and catch fire");
                self.turtle.wait_exit();
            }
            Statement::Finish => {/*self.stopped = true,*/}
            Statement::PathCall(id, args) => {
                let path = self.prog.get_path(*id).expect("should be caught by parser");
                let args = self.dbg_args(args).await;
                assert_eq!(path.args.len(), args.len());
                let mut vars = VarList::new();
                for (i, arg) in args.iter().enumerate() {
                    vars.set_var(path.args[i], *arg);
                }
                self.turtle.push_stack(vars);
                self.dbg_stmts(&path.body).await;
                self.turtle.pop_stack();
            }
            Statement::Store(expr, var) => {
                let val = self.dbg_expr(expr).await;
                self.turtle.set_var(var, val);
            }
            Statement::Calc { var, val, op } => {
                let lhs = self.turtle.get_var(var);
                let rhs = self.dbg_expr(val).await;
                self.turtle.set_var(var, op.calc(lhs, rhs));
            }
            Statement::Mark => self.turtle.new_mark(),
            Statement::MoveMark(draw) => self.turtle.move_mark(*draw),
            Statement::IfBranch(cond, stmts) => {
                if self.dbg_cond(cond).await {
                    self.dbg_stmts(stmts).await;
                }
            }
            Statement::IfElseBranch(cond, if_stmts, else_stmts) => {
                if self.dbg_cond(cond).await {
                    self.dbg_stmts(if_stmts).await;
                } else {
                    self.dbg_stmts(else_stmts).await;
                }
            }
            Statement::DoLoop(expr, stmts) => {
                let count = self.dbg_expr(expr).await.floor();
                for _ in 0..count as isize {
                    self.dbg_stmts(stmts).await;
                }
            }
            Statement::CounterLoop { counter, from, up, to, step, body } => {
                let init = self.dbg_expr(from).await;
                let end = self.dbg_expr(to).await;
                let step = match step {
                    Some(expr) => self.dbg_expr(expr).await,
                    None => 1.0,
                }.neg(!*up);
                self.turtle.set_var(counter, init);
                while *up != (self.turtle.get_var(counter) >= end) {
                    self.dbg_stmts(body).await;
                    let next_val = self.turtle.get_var(counter) + step;
                    self.turtle.set_var(counter, next_val);
                }
            }
            Statement::WhileLoop(cond, stmts) => {
                while self.dbg_cond(cond).await {
                    self.dbg_stmts(stmts).await;
                }
            }
            Statement::RepeatLoop(cond, stmts) => {
                self.dbg_stmts(stmts).await;
                while self.dbg_cond(cond).await {
                    self.dbg_stmts(stmts).await;
                }
            }
        }
    }

    async fn dbg_expr(&mut self, expr: &Expr) -> f64 {
        let fut = async {
            match expr {
                Expr::Const(val) => *val,
                Expr::Variable(var) => self.turtle.get_var(var),
                Expr::BiOperation(lhs, op, rhs) => {
                    let lhs = self.dbg_expr(lhs).await;
                    let rhs = self.dbg_expr(rhs).await;
                    op.calc(lhs, rhs)
                }
                Expr::Negate(expr) => {
                    -self.dbg_expr(expr).await
                }
                Expr::Absolute(expr) => self.dbg_expr(expr).await.abs(),
                Expr::Bracket(expr) => self.dbg_expr(expr).await,
                Expr::FuncCall(pdf, args) => {
                    let args = self.dbg_args(args).await;
                    pdf.calc(&args)
                }
                Expr::CalcCall(id, args) => {
                    let calc = self.prog.get_calc(*id).expect("should be checked by parser");
                    let args = self.dbg_args(args).await;
                    assert_eq!(calc.args.len(), args.len());
                    let mut vars = VarList::new();
                    for (i, arg) in args.iter().enumerate() {
                        vars.set_var(calc.args[i], *arg);
                    }
                    self.turtle.push_stack(vars);
                    self.dbg_stmts(&calc.body).await;
                    let res = self.dbg_expr(&calc.ret).await;
                    self.turtle.pop_stack();
                    res
                }
            }
        };
        Box::pin(fut).await
    }

    async fn dbg_cond(&mut self, cond: &Cond) -> bool {
        let fut = async {
            match cond {
                Cond::Bracket(bcond) => self.dbg_cond(bcond).await,
                Cond::Cmp(lhs, op, rhs) => {
                    let lhs = self.dbg_expr(lhs).await;
                    let rhs = self.dbg_expr(rhs).await;
                    op.compare(lhs, rhs)
                }
                Cond::And(lhs, rhs) => {
                    self.dbg_cond(lhs).await && self.dbg_cond(rhs).await
                }
                Cond::Or(lhs, rhs) => {
                    self.dbg_cond(lhs).await || self.dbg_cond(rhs).await
                }
                Cond::Not(sub) => !self.dbg_cond(sub).await,
            }
        };
        Box::pin(fut).await
    }

    async fn dbg_args(&mut self, args: &[Expr]) -> Vec<f64> {
        let mut res = Vec::with_capacity(args.len());
        for arg in args {
            res.push(self.dbg_expr(arg).await);
        }
        res
    }
}
