use std::{
    cell::{Cell, RefCell}, future::Future, rc::Rc, sync::mpsc::{Receiver, Sender}, task::Poll
};

use crate::{
    debugger::{
        turtle::{FuncType, StackFrame},
        varlist::VarList,
    },
    pos::FilePos,
    prog::PathDef,
    tokens::{Block, Expr, ExprKind, Statement, Value},
    TProgram,
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

pub enum DbgCommand {
    Eval(Expr),
    Exec(Statement),
}

pub struct TurtleTask<'p, W: Window> {
    prog: &'p TProgram,
    turtle: Rc<RefCell<Turtle>>,
    ctx: Rc<GlobalCtx<W>>,
    action: Sender<(DbgAction, FilePos)>,
    cmds: Receiver<DbgCommand>,
    curr_pos: FilePos,
    narrate: Rc<Cell<bool>>,
}

impl<'p, W: Window> TurtleTask<'p, W> {
    pub fn new(
        prog: &'p TProgram,
        turtle: Rc<RefCell<Turtle>>,
        ctx: Rc<GlobalCtx<W>>,
        action: Sender<(DbgAction, FilePos)>,
        cmds: Receiver<DbgCommand>,
        narrate: Rc<Cell<bool>>,
    ) -> Self {
        Self {
            prog,
            turtle,
            ctx,
            action,
            cmds,
            curr_pos: FilePos::default(),
            narrate,
        }
    }

    pub async fn execute(mut self) {
        self.dbg_block(&self.prog.main).await
    }

    pub async fn execute_path(self, name: usize, args: Vec<Value>) {
        let path = self.prog.get_path(name).expect("path should exist");
        self.exec_path_def(path, args, FuncType::Path(name)).await;
    }

    /// panics if event doesn't exist
    pub async fn execute_event(self, kind: EventKind, args: Vec<Value>) {
        let evt = match kind {
            EventKind::Mouse => &self.prog.mouse_event,
            EventKind::Key => &self.prog.key_event,
        };
        self.exec_path_def(
            evt.as_ref().expect("should be caught earlier"),
            args,
            FuncType::Event(kind),
        )
        .await;
    }

    pub async fn exec_path_def(mut self, path: &'p PathDef, args: Vec<Value>, ft: FuncType) {
        assert_eq!(path.args.len(), args.len());
        {
            let mut ttl = self.turtle.borrow_mut();
            let frame = ttl.stack.last_mut().unwrap();
            for (&(arg, _), val) in path.args.iter().zip(args.into_iter()) {
                frame.vars.set_var(arg, val);
            }
            frame.func = ft;
        }
        self.dbg_block(&path.body).await;
    }

    async fn dbg_block(&mut self, block: &Block) {
        let fut = async {
            if self.ctx.debug {
                let _ = self.action.send((DbgAction::BlockEntered, block.begin));
            }
            self.check_cmds().await;
            for stmt in &block.statements {
                self.curr_pos = stmt.get_pos();
                self.turtle.borrow_mut().stack.last_mut().unwrap().curr_pos = self.curr_pos;
                // before
                self.ret(DbgAction::BeforeStmt, self.ctx.debug).await;
                self.check_cmds().await;
                // execute
                self.dbg_stmt(stmt).await;
                self.check_cmds().await;
                // after
                if self.narrate.get() {
                    stmt.narrate(&self.prog.symbols);
                }
                self.ret(DbgAction::AfterStmt(stmt.kind()), self.ctx.debug)
                    .await;
            }
        };
        Box::pin(fut).await
    }

    async fn check_cmds(&mut self) {
        while let Ok(cmd) = self.cmds.try_recv() {
            match cmd {
                DbgCommand::Eval(expr) => {
                    let res = self.dbg_expr(&expr).await;
                    self.ret(DbgAction::CmdResult(Some(res)), true).await;
                }
                DbgCommand::Exec(stmt) => {
                    self.dbg_stmt(&stmt).await;
                    self.ret(DbgAction::CmdResult(None), true).await;
                }
            }
        }
    }

    async fn dbg_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::MoveDist { dist, draw, back } => {
                let dist = self.dbg_expr(dist).await;
                self.turtle
                    .borrow_mut()
                    .move_dist(&self.ctx, dist.num(), *back, *draw);
                self.ret(DbgAction::Sleep, *draw).await;
            }
            Statement::MoveHome(draw) => {
                self.turtle.borrow_mut().move_home(&self.ctx, *draw);
                self.ret(DbgAction::Sleep, *draw).await;
            }
            Statement::Turn { left, by } => {
                let angle = self.dbg_expr(by).await.num();
                let new_dir = self.turtle.borrow().dir + if *left { angle } else { -angle };
                self.turtle.borrow_mut().set_dir(new_dir);
            }
            Statement::Direction(expr) => {
                let new_dir = self.dbg_expr(expr).await;
                self.turtle.borrow_mut().set_dir(new_dir.num());
            }
            Statement::Color(ex1, ex2, ex3) => {
                let r = self.dbg_expr(ex1).await.num();
                let g = self.dbg_expr(ex2).await.num();
                let b = self.dbg_expr(ex3).await.num();
                self.turtle.borrow_mut().set_col(r, g, b);
            }
            Statement::Clear => {
                self.ctx.window.borrow_mut().clear();
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
                let frame = StackFrame {
                    vars,
                    func: FuncType::Path(*id),
                    curr_pos: path.body.begin,
                };
                self.turtle.borrow_mut().stack.push(frame);
                self.dbg_block(&path.body).await;
                self.turtle.borrow_mut().stack.pop();
            }
            Statement::Store(expr, var) => {
                let val = self.dbg_expr(expr).await;
                self.turtle.borrow_mut().set_var(&self.ctx, var, val);
            }
            Statement::Calc { var, val, op } => {
                let lhs = self.turtle.borrow_mut().get_var(&self.ctx, var);
                let rhs = self.dbg_expr(val).await;
                self.turtle
                    .borrow_mut()
                    .set_var(&self.ctx, var, op.eval(&lhs, &rhs));
            }
            Statement::Mark => self.turtle.borrow_mut().new_mark(),
            Statement::MoveMark(draw) => {
                self.turtle.borrow_mut().move_mark(&self.ctx, *draw);
                self.ret(DbgAction::Sleep, *draw).await;
            }
            Statement::Print(expr) => {
                let msg = self.dbg_expr(expr).await.string();
                self.ctx.window.borrow_mut().print(&msg);
            }
            Statement::Split(id, args) => {
                let args = self.dbg_args(args).await;
                let _ = self.action.send((
                    DbgAction::Split(*id, args, Box::new(self.turtle.borrow().split())),
                    self.curr_pos,
                ));
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
                self.turtle
                    .borrow_mut()
                    .set_var(&self.ctx, counter, Value::Number(init));
                while *up != (self.turtle.borrow_mut().get_var(&self.ctx, counter).num() >= end) {
                    self.dbg_block(body).await;
                    let next_val = self.turtle.borrow_mut().get_var(&self.ctx, counter).num() + step;
                    self.turtle
                        .borrow_mut()
                        .set_var(&self.ctx, counter, Value::Number(next_val));
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

    pub(super) async fn dbg_expr(&mut self, expr: &Expr) -> Value {
        let fut = async {
            match &expr.kind {
                ExprKind::Const(val) => val.clone(),
                ExprKind::Variable(var) => self.turtle.borrow_mut().get_var(&self.ctx, var),
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
                    let frame = StackFrame {
                        vars,
                        func: FuncType::Calc(*id),
                        curr_pos: calc.body.begin,
                    };
                    self.turtle.borrow_mut().stack.push(frame);
                    self.dbg_block(&calc.body).await;
                    let res = self.dbg_expr(&calc.ret).await;
                    self.turtle.borrow_mut().stack.pop();
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

    async fn ret(&mut self, act: DbgAction, cond: bool) {
        if cond {
            if let DbgAction::Finished(wait) = act {
                self.ctx.wait_end.set(wait);
            }
            let _ = self.action.send((act, self.curr_pos));
            // yield control
            TurtleFuture(false).await;
        }
    }
}
