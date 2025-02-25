use std::{future::Future, rc::Rc, sync::mpsc::Sender, task::Poll};

use parking_lot::Mutex;

use crate::{debugger::varlist::VarList, pos::FilePos, tokens::{Block, Cond, Expr, Statement}, TProgram};

use super::{turtle::Turtle, DbgAction};

struct TurtleFuture(bool);

impl Future for TurtleFuture {
    type Output = ();

    fn poll(mut self: std::pin::Pin<&mut Self>, _: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        if self.0 {
            Poll::Ready(())
        } else {
            self.0 = true;
            Poll::Pending
        }
    }
}

pub struct DebugTask<'p> {
    prog: &'p TProgram,
    turtle: Rc<Mutex<Turtle>>,
    finished: Rc<Mutex<bool>>,
    action: Sender<(DbgAction<'p>, FilePos)>,
    debug: bool,
}

impl<'p> DebugTask<'p> {
    pub fn new(
        prog: &'p TProgram,
        turtle: Rc<Mutex<Turtle>>,
        finished: Rc<Mutex<bool>>,
        action: Sender<(DbgAction<'p>, FilePos)>,
        debug: bool,
    ) -> Self {
        Self {
            prog,
            turtle,
            finished,
            action,
            debug,
        }
    }

    pub async fn execute(mut self) {
        self.dbg_block(&self.prog.main).await
    }

    async fn dbg_block(&mut self, block: &'p Block) {
        let fut = async {
            if self.debug {
                let _ = self.action.send((DbgAction::BlockEntered, block.begin));
            }
            for stmt in &block.statements {
                // before
                if self.debug {
                    let _ = self.action.send((DbgAction::BeforeStmt, stmt.get_pos()));
                    TurtleFuture(false).await;
                }
                // execute
                self.dbg_stmt(stmt).await;
                // after
                if self.debug {
                    let _ = self.action.send((DbgAction::AfterStmt(stmt), stmt.get_pos()));
                    TurtleFuture(false).await;
                }
            }
        };
        Box::pin(fut).await
    }

    async fn dbg_stmt(&mut self, stmt: &'p Statement) {
        match stmt {
            Statement::MoveDist { dist, draw, back } => {
                let dist = self.dbg_expr(dist).await;
                self.turtle.lock().move_dist(dist, *back, *draw);
            }
            Statement::MoveHome(draw) => self.turtle.lock().move_home(*draw),
            Statement::Turn { left, by } => {
                let angle = self.dbg_expr(by).await;
                let new_dir = self.turtle.lock().dir + if *left { angle } else { -angle };
                self.turtle.lock().set_dir(new_dir);
            }
            Statement::Direction(expr) => {
                let new_dir = self.dbg_expr(expr).await;
                self.turtle.lock().set_dir(new_dir);
            }
            Statement::Color(ex1, ex2, ex3) => {
                let r = self.dbg_expr(ex1).await;
                let g = self.dbg_expr(ex2).await;
                let b = self.dbg_expr(ex3).await;
                self.turtle.lock().set_col(r, g, b);
            }
            Statement::Clear => self.turtle.lock().window.clear(),
            Statement::Stop => self.stop(true).await,
            Statement::Finish => self.stop(false).await,
            Statement::PathCall(id, args) => {
                let path = self.prog.get_path(*id).expect("should be caught by parser");
                let args = self.dbg_args(args).await;
                assert_eq!(path.args.len(), args.len());
                let mut vars = VarList::new();
                for (i, arg) in args.iter().enumerate() {
                    vars.set_var(path.args[i], *arg);
                }
                self.turtle.lock().stack.push((Some(*id), vars));
                self.dbg_block(&path.body).await;
                self.turtle.lock().stack.pop();
            }
            Statement::Store(expr, var) => {
                let val = self.dbg_expr(expr).await;
                self.turtle.lock().set_var(var, val);
            }
            Statement::Calc { var, val, op } => {
                let lhs = self.turtle.lock().get_var(var);
                let rhs = self.dbg_expr(val).await;
                self.turtle.lock().set_var(var, op.calc(lhs, rhs));
            }
            Statement::Mark => self.turtle.lock().new_mark(),
            Statement::MoveMark(draw) => self.turtle.lock().move_mark(*draw),
            Statement::IfBranch(cond, stmts) => {
                if self.dbg_cond(cond).await {
                    self.dbg_block(stmts).await;
                }
            }
            Statement::IfElseBranch(cond, if_stmts, else_stmts) => {
                if self.dbg_cond(cond).await {
                    self.dbg_block(if_stmts).await;
                } else {
                    self.dbg_block(else_stmts).await;
                }
            }
            Statement::DoLoop(expr, stmts) => {
                let count = self.dbg_expr(expr).await.floor();
                for _ in 0..count as isize {
                    self.dbg_block(stmts).await;
                }
            }
            Statement::CounterLoop {
                counter,
                from,
                up,
                to,
                step,
                body,
            } => {
                let init = self.dbg_expr(from).await;
                let end = self.dbg_expr(to).await;
                let step = if *up { 1.0 } else { -1.0 }
                    * match step {
                        Some(expr) => self.dbg_expr(expr).await,
                        None => 1.0,
                    };
                self.turtle.lock().set_var(counter, init);
                while *up != (self.turtle.lock().get_var(counter) >= end) {
                    self.dbg_block(body).await;
                    let next_val = self.turtle.lock().get_var(counter) + step;
                    self.turtle.lock().set_var(counter, next_val);
                }
            }
            Statement::WhileLoop(cond, stmts) => {
                while self.dbg_cond(cond).await {
                    self.dbg_block(stmts).await;
                }
            }
            Statement::RepeatLoop(cond, stmts) => {
                self.dbg_block(stmts).await;
                while !self.dbg_cond(cond).await {
                    self.dbg_block(stmts).await;
                }
            }
        }
    }

    async fn dbg_expr(&mut self, expr: &Expr) -> f64 {
        let fut = async {
            match expr {
                Expr::Const(val) => *val,
                Expr::Variable(var) => self.turtle.lock().get_var(var),
                Expr::BiOperation(lhs, op, rhs) => {
                    let lhs = self.dbg_expr(lhs).await;
                    let rhs = self.dbg_expr(rhs).await;
                    op.calc(lhs, rhs)
                }
                Expr::Negate(expr) => -self.dbg_expr(expr).await,
                Expr::Absolute(expr) => self.dbg_expr(expr).await.abs(),
                Expr::Bracket(expr) => self.dbg_expr(expr).await,
                Expr::FuncCall(pdf, args) => {
                    let args = self.dbg_args(args).await;
                    pdf.calc(&args)
                }
                Expr::CalcCall(id, args) => {
                    let calc = self
                        .prog
                        .get_calc(*id)
                        .expect("should be checked by parser");
                    let args = self.dbg_args(args).await;
                    assert_eq!(calc.args.len(), args.len());
                    let mut vars = VarList::new();
                    for (i, arg) in args.iter().enumerate() {
                        vars.set_var(calc.args[i], *arg);
                    }
                    self.turtle.lock().stack.push((Some(*id), vars));
                    self.dbg_block(&calc.body).await;
                    let res = self.dbg_expr(&calc.ret).await;
                    self.turtle.lock().stack.pop();
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
                Cond::And(lhs, rhs) => self.dbg_cond(lhs).await && self.dbg_cond(rhs).await,
                Cond::Or(lhs, rhs) => self.dbg_cond(lhs).await || self.dbg_cond(rhs).await,
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

    async fn stop(&mut self, wait: bool) {
        *self.finished.lock() = true;
        if wait {
            println!("halt and catch fire");
            while self.turtle.lock().window.keys_pressed().is_ok() {
                std::thread::sleep(std::time::Duration::from_millis(20));
            }
        }
        // yield control
        TurtleFuture(false).await;
    }
}
