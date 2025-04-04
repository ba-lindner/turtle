use std::{
    future::Future,
    pin::Pin,
    rc::Rc,
    sync::mpsc::{self, Receiver, TryRecvError},
};

use parking_lot::Mutex;

use crate::{
    pos::{FilePos, Pos, Positionable},
    tokens::{StmtKind, Value},
    TProgram,
};

use super::{
    task::DebugTask,
    turtle::{FuncType, Turtle},
    window::Window,
    DbgAction, EventKind, GlobalCtx, TurtleWaker,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StepResult {
    Exec(StmtKind),
    Sync,
    Breakpoint,
    Finished,
}

pub struct DebugRunner<'p, W> {
    prog: &'p TProgram,
    turtle: Rc<Mutex<Turtle>>,
    pub finished: bool,
    actions: Receiver<(DbgAction<'p>, FilePos)>,
    pub narrate: bool,
    last_pos: FilePos,
    ctx: Rc<GlobalCtx<W>>,
    split_queue: Vec<(usize, Vec<Value>, Box<Turtle>)>,
    fut: Pin<Box<dyn Future<Output = ()> + 'p>>,
}

impl<'p, W: Window + 'p> DebugRunner<'p, W> {
    pub fn new(prog: &'p TProgram, ctx: Rc<GlobalCtx<W>>) -> DebugRunner<'p, W> {
        Self::construct(prog, ctx, Turtle::new(), |task| Box::pin(task.execute()))
    }

    pub fn for_event(
        prog: &'p TProgram,
        ctx: Rc<GlobalCtx<W>>,
        kind: EventKind,
        args: Vec<Value>,
    ) -> DebugRunner<'p, W> {
        Self::construct(prog, ctx, Turtle::new(), |task| {
            Box::pin(task.execute_event(kind, args))
        })
    }

    fn construct(
        prog: &'p TProgram,
        ctx: Rc<GlobalCtx<W>>,
        turtle: Turtle,
        f: impl FnOnce(DebugTask<'p, W>) -> Pin<Box<dyn Future<Output = ()> + 'p>>,
    ) -> Self {
        let turtle = Rc::new(Mutex::new(turtle));
        let (tx, rx) = mpsc::channel();
        let task = DebugTask::new(prog, turtle.clone(), ctx.clone(), tx);
        Self {
            prog,
            turtle,
            finished: false,
            actions: rx,
            narrate: false,
            last_pos: prog.main.begin,
            ctx,
            split_queue: Vec::new(),
            fut: f(task),
        }
    }

    fn split(&self, path: usize, args: Vec<Value>, turtle: Turtle) -> DebugRunner<'p, W> {
        Self::construct(self.prog, self.ctx.clone(), turtle, |task| {
            Box::pin(task.execute_path(path, args))
        })
    }

    pub fn poll_splits(&mut self) -> Vec<DebugRunner<'p, W>> {
        std::mem::take(&mut self.split_queue)
            .into_iter()
            .map(|(path, args, turtle)| self.split(path, args, *turtle))
            .collect()
    }

    pub fn stack_size(&self) -> usize {
        self.turtle.lock().stack.len()
    }

    // pub fn run(&mut self) {
    //     // greet user
    //     println!("turtle debugger v{}", env!("CARGO_PKG_VERSION"));
    //     println!("press space to run, press h for help");

    //     // async stuff
    //     let waker = TurtleWaker::get_waker();
    //     let mut ctx = std::task::Context::from_waker(&waker);
    //     while self.fut.as_mut().poll(&mut ctx).is_pending() {
    //         // main debug loop (the `while` above)
    //         // executed before and after each statement

    //         if self.finished {
    //             return;
    //         }

    //         let action = self
    //             .get_action()
    //             .expect("task should always deliver action").0;

    //         if self.narrate {
    //             if let DbgAction::AfterStmt(stmt) = *action {
    //                 stmt.narrate(&self.prog.symbols);
    //             }
    //         }

    //         // while used to wait for user interaction if required
    //         let mut waiting_input = true;
    //         while waiting_input {
    //             let keys = match self.turtle.lock().window.keys_pressed() {
    //                 Ok(codes) => codes,
    //                 Err(super::window::ExitPressed) => return,
    //             };

    //             for key in &keys {
    //                 match *key {
    //                     Keycode::SPACE => {
    //                         if self.state == DbgState::Idle {
    //                             self.state = DbgState::Running;
    //                         } else if self.state == DbgState::Running {
    //                             self.state = DbgState::Idle;
    //                         }
    //                     }
    //                     Keycode::S if self.state == DbgState::Idle => {
    //                         self.state = DbgState::Step;
    //                     }
    //                     Keycode::O if self.state == DbgState::Idle => {
    //                         self.state = DbgState::StepOver(self.turtle.lock().stack.len());
    //                     }
    //                     Keycode::T if self.state == DbgState::Idle => {
    //                         self.state = DbgState::SkipTurtle;
    //                     }
    //                     Keycode::D if self.state == DbgState::Idle => {
    //                         self.state = DbgState::SkipDraw;
    //                     }
    //                     Keycode::N => {
    //                         self.narrate = !self.narrate;
    //                         println!("narrator is {}", if self.narrate { "ON" } else { "OFF" });
    //                     }
    //                     Keycode::V => {
    //                         self.turtle.lock().dump_vars(&self.prog.symbols);
    //                     }
    //                     Keycode::P => {
    //                         let func = self
    //                             .turtle
    //                             .lock()
    //                             .stack
    //                             .last()
    //                             .unwrap()
    //                             .0
    //                             .map(|id| {
    //                                 self.prog
    //                                     .symbols
    //                                     .get_index(id)
    //                                     .expect("missing function name")
    //                                     .0
    //                                     .as_str()
    //                             })
    //                             .unwrap_or("main");
    //                         println!("in function {func} at {}", action.get_pos());
    //                     }
    //                     Keycode::H => {
    //                         println!("{DEBUG_HELP}");
    //                     }
    //                     k => {
    //                         eprintln!("unknown key {k}");
    //                     }
    //                 }
    //             }

    //             self.handle_breakpoints(action);

    //             // better default
    //             waiting_input = false;
    //             match self.state {
    //                 DbgState::Idle => {
    //                     if *action == DbgAction::BeforeStmt {
    //                         std::thread::sleep(std::time::Duration::from_millis(20));
    //                         waiting_input = true;
    //                     }
    //                 }
    //                 DbgState::Running => {}
    //                 DbgState::Step => {
    //                     if action.is_after() {
    //                         self.state = DbgState::Idle;
    //                     }
    //                 }
    //                 DbgState::StepOver(depth) => {
    //                     if action.is_after() && self.turtle.lock().stack.len() == depth {
    //                         self.state = DbgState::Idle;
    //                     }
    //                 }
    //                 DbgState::SkipTurtle => {
    //                     if let DbgAction::AfterStmt(stmt) = *action {
    //                         if stmt.kind() >= StmtKind::Turtle {
    //                             self.state = DbgState::Idle;
    //                         }
    //                     }
    //                 }
    //                 DbgState::SkipDraw => {
    //                     if let DbgAction::AfterStmt(stmt) = *action {
    //                         if stmt.kind() == StmtKind::Draw {
    //                             self.state = DbgState::Idle;
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }

    fn get_action(&mut self) -> Result<Pos<DbgAction<'p>>, TryRecvError> {
        let mut act = None;
        loop {
            match self.actions.try_recv() {
                Ok((DbgAction::BlockEntered, pos)) => self.last_pos = pos,
                Ok((DbgAction::AfterStmt(stmt), pos)) => {
                    act = Some(DbgAction::AfterStmt(stmt).attach_pos(pos));
                    self.last_pos = pos;
                }
                Ok((DbgAction::Split(path, args, ttl), _)) => {
                    self.split_queue.push((path, args, ttl))
                }
                Ok((a, pos)) => act = Some(a.attach_pos(pos)),
                Err(TryRecvError::Disconnected) => return Err(TryRecvError::Disconnected),
                Err(TryRecvError::Empty) => return act.ok_or(TryRecvError::Empty),
            }
        }
    }

    pub fn run_sleep(&mut self) {
        if self.finished {
            return;
        }
        let waker = TurtleWaker::get_waker();
        let mut cx = std::task::Context::from_waker(&waker);
        while self.fut.as_mut().poll(&mut cx).is_pending() {
            match *self.get_action().unwrap() {
                DbgAction::AfterStmt(stmt) if self.narrate => {
                    stmt.narrate(&self.prog.symbols);
                }
                DbgAction::Sleep => return,
                DbgAction::Finished(wait) => {
                    *self.ctx.wait_end.lock() = wait;
                    break;
                }
                _ => {}
            }
        }
        self.finished = true;
    }

    pub fn step_single(&mut self) -> StepResult {
        if self.finished {
            return StepResult::Finished;
        }
        let mut res = None;
        let waker = TurtleWaker::get_waker();
        let mut cx = std::task::Context::from_waker(&waker);
        while self.fut.as_mut().poll(&mut cx).is_pending() {
            let act = self
                .get_action()
                .expect("future should only return if some action was sent");
            match *act {
                DbgAction::AfterStmt(stmt) => {
                    if self.narrate {
                        stmt.narrate(&self.prog.symbols);
                    }
                    // use first kind
                    // all following will be end of control structures
                    res = res.or(Some(stmt.kind()));
                }
                DbgAction::Sleep => return StepResult::Sync,
                DbgAction::Finished(wait) => {
                    *self.ctx.wait_end.lock() = wait;
                    break;
                }
                DbgAction::BeforeStmt => {
                    for bp in &*self.ctx.breakpoints.lock() {
                        if self.last_pos < *bp && *bp < act.get_pos() {
                            return StepResult::Breakpoint;
                        }
                    }
                    if let Some(kind) = res {
                        return StepResult::Exec(kind);
                    }
                }
                _ => unreachable!(),
            }
        }
        self.finished = true;
        if let Some(kind) = res {
            StepResult::Exec(kind)
        } else {
            StepResult::Finished
        }
    }

    pub fn run_breakpoints(&mut self) -> StepResult {
        let waker = TurtleWaker::get_waker();
        let mut cx = std::task::Context::from_waker(&waker);
        while self.fut.as_mut().poll(&mut cx).is_pending() {
            let act = self
                .get_action()
                .expect("future should only return if some action was sent");
            match *act {
                DbgAction::BeforeStmt => {
                    for bp in &*self.ctx.breakpoints.lock() {
                        if self.last_pos < *bp && *bp < act.get_pos() {
                            return StepResult::Breakpoint;
                        }
                    }
                }
                DbgAction::AfterStmt(stmt) if self.narrate => {
                    stmt.narrate(&self.prog.symbols);
                }
                DbgAction::Sleep => return StepResult::Sync,
                DbgAction::Finished(wait) => {
                    *self.ctx.wait_end.lock() = wait;
                    break;
                }
                _ => {}
            }
        }
        self.finished = true;
        StepResult::Finished
    }

    // fn handle_breakpoints(&mut self, action: Pos<DbgAction<'p>>) {
    //     if self.state == DbgState::Running
    //         && *action == DbgAction::BeforeStmt
    //         && !self.breakpoints.is_empty()
    //     {
    //         for &bp in &self.breakpoints {
    //             if self.last_pos < bp && bp <= action.get_pos() {
    //                 self.state = DbgState::Idle;
    //                 break;
    //             }
    //         }
    //         self.last_pos = action.get_pos();
    //     }
    // }

    pub fn dump_vars(&self) {
        self.turtle.lock().dump_vars(&self.ctx, &self.prog.symbols);
    }

    pub fn curr_func(&self) -> FuncType {
        self.turtle
            .lock()
            .stack
            .last()
            .expect("should never be empty")
            .0
    }

    pub fn print_pos(&self) {
        let name = |id| self.prog.symbols.get_index(id).unwrap().0;
        match self.curr_func() {
            FuncType::Main => println!("in main block at {}", self.last_pos),
            FuncType::Path(id) => println!("in path {} at {}", name(id), self.last_pos),
            FuncType::Calc(id) => println!("in calculation {} at {}", name(id), self.last_pos),
            FuncType::Event(kind) => println!("in {kind} event handler at {}", self.last_pos),
        }
    }
}
