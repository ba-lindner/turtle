use std::{future::Future, rc::Rc, sync::mpsc::Sender, task::Poll};

use parking_lot::Mutex;

use crate::{
    debugger::varlist::VarList, pos::FilePos, prog::PathDef, tokens::{Block, Expr, ExprKind, Statement, Value}, TProgram
};

use super::{turtle::Turtle, window::Window, DbgAction, EventKind, GlobalCtx};

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

pub struct DebugTask<'p, W: Window> {
    prog: &'p TProgram,
    turtle: Rc<Mutex<Turtle>>,
    ctx: Rc<GlobalCtx<W>>,
    action: Sender<(DbgAction<'p>, FilePos)>,
    debug: bool,
    curr_pos: FilePos,
}

impl<'p, W: Window> DebugTask<'p, W> {
    pub fn new(
        prog: &'p TProgram,
        turtle: Rc<Mutex<Turtle>>,
        ctx: Rc<GlobalCtx<W>>,
        action: Sender<(DbgAction<'p>, FilePos)>,
        debug: bool,
    ) -> Self {
        Self {
            prog,
            turtle,
            ctx,
            action,
            debug,
            curr_pos: FilePos::default(),
        }
    }

    pub async fn execute(mut self) {
        self.dbg_block(&self.prog.main).await
    }

    pub async fn execute_path(mut self, name: usize, args: Vec<Value>) {
        let path = self.prog.get_path(name).expect("path should exist");
        self.exec_path_def(path, args).await;
    }

    pub async fn execute_event(mut self, kind: EventKind, args: Vec<Value>) {
        let evt = match kind {
            EventKind::Mouse => &self.prog.mouse_event,
            EventKind::Key => &self.prog.key_event,
        };
        if let Some(evt) = evt {
            self.exec_path_def(evt, args).await;
        }
    }

    async fn exec_path_def(&mut self, path: &'p PathDef, args: Vec<Value>) {
        assert_eq!(path.args.len(), args.len());
        let mut ttl = self.turtle.lock();
        let (func, vars) = ttl.stack.last_mut().unwrap();
        for (&(arg, _), val) in path.args.iter().zip(args.into_iter()) {
            vars.set_var(arg, val);
        }
        *func = Some(path.name);
        drop(ttl);
        self.dbg_block(&path.body).await;
    }

    async fn dbg_block(&mut self, block: &'p Block) {
        let fut = async {
            if self.debug {
                let _ = self.action.send((DbgAction::BlockEntered, block.begin));
            }
            for stmt in &block.statements {
                self.curr_pos = stmt.get_pos();
                // before
                self.ret(DbgAction::BeforeStmt, self.debug).await;
                // execute
                self.dbg_stmt(stmt).await;
                // after
                self.ret(DbgAction::AfterStmt(stmt), self.debug).await;
            }
        };
        Box::pin(fut).await
    }

    async fn dbg_stmt(&mut self, stmt: &'p Statement) {
        match stmt {
            Statement::MoveDist { dist, draw, back } => {
                let dist = self.dbg_expr(dist).await;
                self.turtle.lock().move_dist(&self.ctx, dist.num(), *back, *draw);
                self.ret(DbgAction::Sleep, *draw).await;
            }
            Statement::MoveHome(draw) => {
                self.turtle.lock().move_home(&self.ctx, *draw);
                self.ret(DbgAction::Sleep, *draw).await;
            }
            Statement::Turn { left, by } => {
                let angle = self.dbg_expr(by).await.num();
                let new_dir =
                    self.turtle.lock().dir + if *left { angle } else { -angle };
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
            Statement::Clear => {
                self.ctx.window.lock().clear();
                self.ret(DbgAction::Sleep, true).await;
            }
            Statement::Stop => self.ret(DbgAction::Finished(true), true).await,
            Statement::Finish => self.ret(DbgAction::Finished(false), true).await,
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
                self.turtle.lock().set_var(&self.ctx, var, val);
            }
            Statement::Calc { var, val, op } => {
                let lhs = self.turtle.lock().get_var(&self.ctx, var);
                let rhs = self.dbg_expr(val).await;
                self.turtle.lock().set_var(&self.ctx, var, op.eval(&lhs, &rhs));
            }
            Statement::Mark => self.turtle.lock().new_mark(),
            Statement::MoveMark(draw) => {
                self.turtle.lock().move_mark(&self.ctx, *draw);
                self.ret(DbgAction::Sleep, *draw).await;
            }
            Statement::Print(expr) => {
                println!("Turtle says: {}", self.dbg_expr(expr).await.string())
            }
            Statement::Split(id, args) => {
                let args = self.dbg_args(args).await;
                let _ = self.action.send((DbgAction::Split(*id, args), self.curr_pos));
            }
            Statement::Wait => self.ret(DbgAction::Sleep, true).await,
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
                self.turtle.lock().set_var(&self.ctx, counter, Value::Number(init));
                while *up != (self.turtle.lock().get_var(&self.ctx, counter).num() >= end) {
                    self.dbg_block(body).await;
                    let next_val = self.turtle.lock().get_var(&self.ctx, counter).num() + step;
                    self.turtle.lock().set_var(&self.ctx, counter, Value::Number(next_val));
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
                ExprKind::Variable(var) => self.turtle.lock().get_var(&self.ctx, var),
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

    async fn ret(&mut self, act: DbgAction<'p>, cond: bool) {
        if cond {
            let _ = self.action.send((act, self.curr_pos));
            // yield control
            TurtleFuture(false).await;
        }
    }
}
