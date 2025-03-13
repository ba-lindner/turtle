use std::{future::Future, rc::Rc, sync::mpsc::Sender, task::Poll};

use parking_lot::Mutex;

use crate::{
    debugger::varlist::VarList,
    pos::FilePos,
    tokens::{Block, Expr, ExprKind, Statement, Value},
    TProgram,
};

use super::{turtle::Turtle, DbgAction};

struct TurtleFuture(bool);

impl Future for TurtleFuture {
    type Output = ();

    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        _: &mut std::task::Context<'_>,
    ) -> Poll<Self::Output> {
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
                    let _ = self
                        .action
                        .send((DbgAction::AfterStmt(stmt), stmt.get_pos()));
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
                self.turtle.lock().move_dist(dist.num(), *back, *draw);
            }
            Statement::MoveHome(draw) => self.turtle.lock().move_home(*draw),
            Statement::Turn { left, by } => {
                let angle = self.dbg_expr(by).await;
                let new_dir =
                    self.turtle.lock().dir + if *left { angle.num() } else { -angle.num() };
                self.turtle.lock().set_dir(new_dir);
            }
            Statement::Direction(expr) => {
                let new_dir = self.dbg_expr(expr).await;
                self.turtle.lock().set_dir(new_dir.num());
            }
            Statement::Color(ex1, ex2, ex3) => {
                let r = self.dbg_expr(ex1).await.num();
                let g = self.dbg_expr(ex2).await.num();
                let b = self.dbg_expr(ex3).await.num();
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
                for (i, arg) in args.into_iter().enumerate() {
                    vars.set_var(path.args[i].0, arg);
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
                self.turtle.lock().set_var(var, op.eval(&lhs, &rhs));
            }
            Statement::Mark => self.turtle.lock().new_mark(),
            Statement::MoveMark(draw) => self.turtle.lock().move_mark(*draw),
            Statement::Print(expr) => {
                println!("Turtle says: {}", self.dbg_expr(expr).await.string())
            }
            Statement::IfBranch(cond, stmts) => {
                if self.dbg_expr(cond).await.bool() {
                    self.dbg_block(stmts).await;
                }
            }
            Statement::IfElseBranch(cond, if_stmts, else_stmts) => {
                if self.dbg_expr(cond).await.bool() {
                    self.dbg_block(if_stmts).await;
                } else {
                    self.dbg_block(else_stmts).await;
                }
            }
            Statement::DoLoop(expr, stmts) => {
                let count = self.dbg_expr(expr).await.num().floor();
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
                let init = self.dbg_expr(from).await.num();
                let end = self.dbg_expr(to).await.num();
                let step = if *up { 1.0 } else { -1.0 }
                    * match step {
                        Some(expr) => self.dbg_expr(expr).await.num(),
                        None => 1.0,
                    };
                self.turtle.lock().set_var(counter, Value::Number(init));
                while *up != (self.turtle.lock().get_var(counter).num() >= end) {
                    self.dbg_block(body).await;
                    let next_val = self.turtle.lock().get_var(counter).num() + step;
                    self.turtle.lock().set_var(counter, Value::Number(next_val));
                }
            }
            Statement::WhileLoop(cond, stmts) => {
                while self.dbg_expr(cond).await.bool() {
                    self.dbg_block(stmts).await;
                }
            }
            Statement::RepeatLoop(cond, stmts) => {
                self.dbg_block(stmts).await;
                while !self.dbg_expr(cond).await.bool() {
                    self.dbg_block(stmts).await;
                }
            }
        }
    }

    async fn dbg_expr(&mut self, expr: &Expr) -> Value {
        let fut = async {
            match &expr.kind {
                ExprKind::Const(val) => val.clone(),
                ExprKind::Variable(var) => self.turtle.lock().get_var(var),
                ExprKind::BiOperation(lhs, op, rhs) => {
                    let lhs = self.dbg_expr(lhs).await;
                    let rhs = self.dbg_expr(rhs).await;
                    op.eval(&lhs, &rhs)
                }
                ExprKind::UnOperation(op, expr) => {
                    let val = self.dbg_expr(expr).await;
                    op.eval(&val)
                }
                ExprKind::Absolute(expr) => Value::Number(self.dbg_expr(expr).await.num().abs()),
                ExprKind::Bracket(expr) => self.dbg_expr(expr).await,
                ExprKind::Convert(from, to) => self.dbg_expr(from).await.convert(*to),
                ExprKind::FuncCall(pdf, args) => pdf.eval(&self.dbg_args(args).await),
                ExprKind::CalcCall(id, args) => {
                    let calc = self
                        .prog
                        .get_calc(*id)
                        .expect("should be checked by parser");
                    let args = self.dbg_args(args).await;
                    assert_eq!(calc.args.len(), args.len());
                    let mut vars = VarList::new();
                    for (i, arg) in args.into_iter().enumerate() {
                        vars.set_var(calc.args[i].0, arg);
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

    async fn dbg_args(&mut self, args: &[Expr]) -> Vec<Value> {
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
