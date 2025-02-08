use std::{
    future::Future,
    pin::pin,
    rc::Rc,
    sync::Arc,
    task::{Context, Poll, Wake, Waker},
};

use parking_lot::Mutex;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;

use crate::{
    pos::Pos,
    tokens::{Cond, Expr, Statement, Statements, StmtKind},
    FilePos, TProgram,
};
use turtle::Turtle;
use varlist::VarList;

mod turtle;
mod varlist;
mod window;

// have a cat

const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;
const START_COLOR: Color = Color::YELLOW;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DbgAction {
    BeforeStmt,
    AfterStmt,
}

pub struct Debugger<'p> {
    prog: &'p TProgram,
    turtle: Rc<Mutex<Turtle>>,
    finished: Rc<Mutex<bool>>,
    data: Rc<Mutex<DebugData<'p>>>,
}

struct DebugData<'s> {
    last_pos: FilePos,
    curr_pos: FilePos,
    curr_stmt: Option<&'s Statement>,
    action: DbgAction,
}

impl<'s> DebugData<'s> {
    fn new() -> Self {
        Self {
            last_pos: FilePos::default(),
            curr_pos: FilePos::default(),
            curr_stmt: None,
            action: DbgAction::BeforeStmt,
        }
    }

    fn update(&mut self, stmt: &'s Pos<Statement>) {
        self.last_pos = self.curr_pos;
        self.curr_pos = stmt.get_pos();
        self.curr_stmt = Some(&**stmt);
    }
}

impl<'p> Debugger<'p> {
    pub fn new(prog: &'p TProgram, args: &[String], title: &str) -> Self {
        Self {
            prog,
            turtle: Rc::new(Mutex::new(Turtle::new(title, args))),
            finished: Rc::new(Mutex::new(false)),
            data: Rc::new(Mutex::new(DebugData::new())),
        }
    }

    pub fn interpret(&mut self) {
        let waker: Waker = Arc::new(TurtleWaker).into();
        let mut ctx = Context::from_waker(&waker);
        let mut fut = pin!(self.dbg_stmts(&self.prog.main));
        while fut.as_mut().poll(&mut ctx).is_pending() {}
    }

    pub fn run(&mut self, bp: &[String]) {
        // greet user
        println!("turtle debugger v{}", env!("CARGO_PKG_VERSION"));
        println!("press space to run, press h for help");

        // inner items
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum DbgState {
            Idle,
            Running,
            Step,
            StepOver(usize),
            SkipTurtle,
            SkipDraw,
        }

        fn parse_pos(bp: &str) -> Option<FilePos> {
            let (line, col) = bp.split_once(",")?;
            Some(FilePos::new(line.parse().ok()?, col.parse().ok()?))
        }

        // clone `Rc`s
        let turtle = Rc::clone(&self.turtle);
        let finished = Rc::clone(&self.finished);
        let data = Rc::clone(&self.data);
        let prog = self.prog;

        // init debug state
        let breakpoints: Vec<_> = bp.iter().filter_map(|bp| parse_pos(bp)).collect();
        let mut state = DbgState::Idle;
        let mut narrator = false;

        // async stuff
        let waker: Waker = Arc::new(TurtleWaker).into();
        let mut ctx = Context::from_waker(&waker);
        let mut fut = pin!(self.dbg_stmts(&self.prog.main));
        while fut.as_mut().poll(&mut ctx).is_pending() {
            // main debug loop (the `while` above)
            // executed before and after each statement

            if *finished.lock() {
                return;
            }

            let action = data.lock().action;

            if action == DbgAction::AfterStmt && narrator {
                data.lock().curr_stmt.unwrap().narrate(&prog.symbols);
            }

            // while used to wait for user interaction if required
            let mut waiting_input = true;
            while waiting_input {
                let keys = match turtle.lock().window.keys_pressed() {
                    Ok(codes) => codes,
                    Err(window::ExitPressed) => return,
                };

                for key in &keys {
                    match *key {
                        Keycode::SPACE => {
                            if state == DbgState::Idle {
                                state = DbgState::Running;
                            } else if state == DbgState::Running {
                                state = DbgState::Idle;
                            }
                        }
                        Keycode::S if state == DbgState::Idle => {
                            state = DbgState::Step;
                        }
                        Keycode::O if state == DbgState::Idle => {
                            state = DbgState::StepOver(turtle.lock().stack.len());
                        }
                        Keycode::T if state == DbgState::Idle => {
                            state = DbgState::SkipTurtle;
                        }
                        Keycode::D if state == DbgState::Idle => {
                            state = DbgState::SkipDraw;
                        }
                        Keycode::N => {
                            narrator = !narrator;
                        }
                        Keycode::V => {
                            turtle.lock().dump_vars(&prog.symbols);
                        }
                        Keycode::P => {
                            let func = turtle
                                .lock()
                                .stack
                                .last()
                                .unwrap()
                                .0
                                .map(|id| {
                                    prog.symbols
                                        .get_index(id)
                                        .expect("missing function name")
                                        .0
                                        .as_str()
                                })
                                .unwrap_or("main");
                            println!("in function {func} at {}", data.lock().curr_pos);
                        }
                        Keycode::H => {
                            println!("available commands:");
                            println!("  SPACE - run / stop debugger");
                            println!("  S     - execute single statement");
                            println!("  O     - step over path / calc call");
                            println!("  T     - skip until next turtle action");
                            println!("  D     - skip until next drawn line");
                            println!("  N     - toggle narrator");
                            println!("  V     - dump variables");
                            println!("  P     - print current position");
                            println!("  H     - show this help");
                        }
                        _ => {}
                    }
                }

                // handle breakpoints
                if state == DbgState::Running
                    && action == DbgAction::BeforeStmt
                    && !breakpoints.is_empty()
                {
                    let (prev, curr) = {
                        let data = data.lock();
                        (data.last_pos, data.curr_pos)
                    };
                    for &bp in &breakpoints {
                        if prev < bp && bp <= curr {
                            state = DbgState::Idle;
                            break;
                        }
                    }
                }

                // better default
                waiting_input = false;
                match state {
                    DbgState::Idle => {
                        if action == DbgAction::BeforeStmt {
                            tick();
                            waiting_input = true;
                        }
                    }
                    DbgState::Running => {}
                    DbgState::Step => {
                        if action == DbgAction::AfterStmt {
                            state = DbgState::Idle;
                        }
                    }
                    DbgState::StepOver(depth) => {
                        if action == DbgAction::AfterStmt && turtle.lock().stack.len() == depth {
                            state = DbgState::Idle;
                        }
                    }
                    DbgState::SkipTurtle => {
                        if action == DbgAction::AfterStmt
                            && data.lock().curr_stmt.unwrap().kind() >= StmtKind::Turtle
                        {
                            state = DbgState::Idle;
                        }
                    }
                    DbgState::SkipDraw => {
                        if action == DbgAction::AfterStmt
                            && data.lock().curr_stmt.unwrap().kind() == StmtKind::Draw
                        {
                            state = DbgState::Idle;
                        }
                    }
                }
            }
        }
    }

    async fn dbg_stmts(&mut self, stmts: &'p Statements) {
        let fut = async {
            for stmt in stmts {
                // before
                {
                    let mut data = self.data.lock();
                    data.update(stmt);
                    data.action = DbgAction::BeforeStmt;
                }
                TurtleFuture(false).await;
                // execute
                self.dbg_stmt(stmt).await;
                // after
                self.data.lock().action = DbgAction::AfterStmt;
                TurtleFuture(false).await;
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
                let new_dir = self.turtle.lock().dir + if *left { -angle } else { angle };
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
                self.dbg_stmts(&path.body).await;
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
                    self.dbg_stmts(body).await;
                    let next_val = self.turtle.lock().get_var(counter) + step;
                    self.turtle.lock().set_var(counter, next_val);
                }
            }
            Statement::WhileLoop(cond, stmts) => {
                while self.dbg_cond(cond).await {
                    self.dbg_stmts(stmts).await;
                }
            }
            Statement::RepeatLoop(cond, stmts) => {
                self.dbg_stmts(stmts).await;
                while !self.dbg_cond(cond).await {
                    self.dbg_stmts(stmts).await;
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
                    self.dbg_stmts(&calc.body).await;
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
                tick();
            }
        }
        // yield control
        TurtleFuture(false).await;
    }
}

fn tick() {
    std::thread::sleep(std::time::Duration::from_millis(20));
}
