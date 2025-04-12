use std::{
    future::Future,
    pin::Pin,
    rc::Rc,
    sync::mpsc::{self, Receiver, Sender, TryRecvError},
};

use parking_lot::Mutex;

use crate::{
    pos::{FilePos, Pos, Positionable},
    tokens::{Expr, PredefVar, Statement, StmtKind, Value, VariableKind},
    TProgram,
};

use super::{
    task::{DbgCommand, TurtleTask},
    turtle::{FuncType, Turtle},
    window::Window,
    DbgAction, DebugErr, EventKind, FrameInfo, GlobalCtx, TurtleWaker, VarDump,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StepResult {
    Exec(StmtKind),
    Sync,
    Breakpoint(usize),
    Finished,
}

pub struct TurtleRunner<'p, W> {
    prog: &'p TProgram,
    turtle: Rc<Mutex<Turtle>>,
    pub finished: bool,
    actions: Receiver<(DbgAction, FilePos)>,
    cmds: Sender<DbgCommand>,
    narrate: Rc<Mutex<bool>>,
    last_pos: FilePos,
    ctx: Rc<GlobalCtx<W>>,
    split_queue: Vec<(usize, Vec<Value>, Box<Turtle>)>,
    pub start_task: (FuncType, Vec<Value>),
    fut: Pin<Box<dyn Future<Output = ()> + 'p>>,
}

impl<'p, W: Window + 'p> TurtleRunner<'p, W> {
    pub fn new(prog: &'p TProgram, ctx: Rc<GlobalCtx<W>>) -> TurtleRunner<'p, W> {
        Self::construct(
            prog,
            ctx,
            Turtle::new(),
            (FuncType::Main, Vec::new()),
            |task| Box::pin(task.execute()),
        )
    }

    pub fn for_event(
        prog: &'p TProgram,
        ctx: Rc<GlobalCtx<W>>,
        kind: EventKind,
        args: Vec<Value>,
    ) -> TurtleRunner<'p, W> {
        Self::construct(
            prog,
            ctx,
            Turtle::new(),
            (FuncType::Event(kind), args.clone()),
            |task| Box::pin(task.execute_event(kind, args)),
        )
    }

    fn construct(
        prog: &'p TProgram,
        ctx: Rc<GlobalCtx<W>>,
        turtle: Turtle,
        start_task: (FuncType, Vec<Value>),
        f: impl FnOnce(TurtleTask<'p, W>) -> Pin<Box<dyn Future<Output = ()> + 'p>>,
    ) -> Self {
        let turtle = Rc::new(Mutex::new(turtle));
        let narrate = Rc::new(Mutex::new(false));
        let (act_tx, act_rx) = mpsc::channel();
        let (cmd_tx, cmd_rx) = mpsc::channel();
        let task = TurtleTask::new(
            prog,
            turtle.clone(),
            ctx.clone(),
            act_tx,
            cmd_rx,
            narrate.clone(),
        );
        Self {
            prog,
            turtle,
            finished: false,
            actions: act_rx,
            cmds: cmd_tx,
            narrate,
            last_pos: prog.main.begin,
            ctx,
            split_queue: Vec::new(),
            start_task,
            fut: f(task),
        }
    }

    fn split(&self, path: usize, args: Vec<Value>, turtle: Turtle) -> TurtleRunner<'p, W> {
        Self::construct(
            self.prog,
            self.ctx.clone(),
            turtle,
            (FuncType::Path(path), args.clone()),
            |task| Box::pin(task.execute_path(path, args)),
        )
    }

    pub fn poll_splits(&mut self) -> Vec<TurtleRunner<'p, W>> {
        std::mem::take(&mut self.split_queue)
            .into_iter()
            .map(|(path, args, turtle)| self.split(path, args, *turtle))
            .collect()
    }

    pub fn stack_size(&self) -> usize {
        self.turtle.lock().stack.len()
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //   execution
    //
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    fn get_action(&mut self) -> Result<Pos<DbgAction>, TryRecvError> {
        let mut act = None;
        loop {
            match self.actions.try_recv() {
                Ok((DbgAction::BlockEntered, pos)) => self.last_pos = pos,
                Ok((a @ DbgAction::AfterStmt(_), pos)) => {
                    act = Some(a.attach_pos(pos));
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
                DbgAction::AfterStmt(kind) => {
                    // use first kind
                    // all following will be end of control structures
                    res.get_or_insert(kind);
                }
                DbgAction::Sleep => return StepResult::Sync,
                DbgAction::Finished(wait) => {
                    *self.ctx.wait_end.lock() = wait;
                    break;
                }
                DbgAction::BeforeStmt => {
                    if let Some(idx) = self.ctx.breakpoint_hit(self.last_pos, act.get_pos()) {
                        return StepResult::Breakpoint(idx);
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
                    if let Some(idx) = self.ctx.breakpoint_hit(self.last_pos, act.get_pos()) {
                        return StepResult::Breakpoint(idx);
                    }
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

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //   debug information
    //
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    pub fn curr_pos(&self) -> (FuncType, FilePos) {
        let ttl = self.turtle.lock();
        let frame = ttl.stack.last().expect("should never be empty");
        (frame.func, frame.curr_pos)
    }

    pub fn vardump(&self, frame: Option<usize>) -> Result<VarDump, DebugErr> {
        let mut ttl = self.turtle.lock();
        let index = frame.unwrap_or(ttl.stack.len() - 1);
        let locals = &ttl
            .stack
            .get(index)
            .ok_or(DebugErr::FrameNotFound(index))?
            .vars;
        let name = |id| {
            self.prog
                .symbols
                .get_index(id)
                .expect("should always exist")
                .0
        };
        Ok(VarDump {
            locals: locals
                .iter()
                .map(|(&id, val)| (name(id).clone(), val.clone()))
                .collect(),
            globals: self
                .ctx
                .vars
                .lock()
                .iter()
                .map(|(&id, val)| (format!("@{}", name(id)), val.clone()))
                .collect(),
            predef: PredefVar::get_all()
                .into_iter()
                .map(|pdv| {
                    let var = VariableKind::GlobalPreDef(pdv).at(FilePos::default());
                    (pdv, ttl.get_var(&self.ctx, &var))
                })
                .collect(),
        })
    }

    pub fn stacktrace(&self) -> Vec<FrameInfo> {
        let mut res = Vec::new();
        let ttl = self.turtle.lock();
        for i in (0..ttl.stack.len()).rev() {
            let frame = &ttl.stack[i];
            res.push(FrameInfo {
                index: i,
                func: frame.func,
                pos: frame.curr_pos,
            })
        }
        res
    }

    pub fn get_narrate(&self) -> bool {
        *self.narrate.lock()
    }

    pub fn set_narrate(&self, n: bool) {
        *self.narrate.lock() = n;
    }

    pub fn eval(&mut self, expr: Expr, frame: Option<usize>) -> Value {
        _ = self.cmds.send(DbgCommand::Eval(expr));
        let mut frames = if let Some(frame) = frame {
            self.turtle.lock().stack.drain(frame + 1..).collect()
        } else {
            Vec::new()
        };
        let res = self.run_cmd();
        self.turtle.lock().stack.append(&mut frames);
        res.expect("expr should always return value")
    }

    pub fn exec(&mut self, stmt: Statement) -> bool {
        _ = self.cmds.send(DbgCommand::Exec(stmt));
        self.run_cmd();
        self.finished
    }

    fn run_cmd(&mut self) -> Option<Value> {
        let waker = TurtleWaker::get_waker();
        let mut cx = std::task::Context::from_waker(&waker);
        while self.fut.as_mut().poll(&mut cx).is_pending() {
            for (act, _) in self.actions.try_iter() {
                match act {
                    DbgAction::Finished(wait) => {
                        self.finished = true;
                        *self.ctx.wait_end.lock() = wait;
                        return None;
                    }
                    DbgAction::CmdResult(res) => return res,
                    _ => {}
                }
            }
        }
        self.finished = true;
        None
    }
}
