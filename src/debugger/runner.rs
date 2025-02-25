use std::{
    future::Future,
    pin::Pin,
    rc::Rc,
    sync::mpsc::{self, Receiver, TryRecvError},
};

use parking_lot::Mutex;
use sdl2::keyboard::Keycode;

use crate::{
    pos::{FilePos, Pos, Positionable},
    tokens::StmtKind, TProgram,
};

use super::{task::DebugTask, turtle::Turtle, DbgAction, TurtleWaker};

const DEBUG_HELP: &str = "available commands:
  SPACE - run / stop debugger
  S     - execute single statement
  O     - step over path / calc call
  T     - skip until next turtle action
  D     - skip until next drawn line
  N     - toggle narrator
  V     - dump variables
  P     - print current position
  H     - show this help";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DbgState {
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

pub struct DebugRunner<'p, F> {
    prog: &'p TProgram,
    turtle: Rc<Mutex<Turtle>>,
    finished: Rc<Mutex<bool>>,
    actions: Receiver<(DbgAction<'p>, FilePos)>,
    breakpoints: Vec<FilePos>,
    state: DbgState,
    pub narrate: bool,
    last_pos: FilePos,
    fut: Pin<Box<F>>,
}

impl<'p> DebugRunner<'p, std::future::Ready<()>> {
    pub fn new(
        prog: &'p TProgram,
        args: &[String],
        title: &str,
    ) -> DebugRunner<'p, impl Future<Output = ()> + 'p> {
        let turtle = Turtle::new(title, args);
        let turtle = Rc::new(Mutex::new(turtle));
        let finished = Rc::new(Mutex::new(false));
        let (tx, rx) = mpsc::channel::<(DbgAction<'p>, FilePos)>();
        let task = DebugTask::new(prog, turtle.clone(), finished.clone(), tx, true);
        DebugRunner {
            prog,
            turtle,
            finished,
            actions: rx,
            breakpoints: Vec::new(),
            state: DbgState::Idle,
            narrate: false,
            last_pos: prog.main.begin,
            fut: Box::pin(task.execute()),
        }
    }
}

impl<'p, F: Future<Output = ()>> DebugRunner<'p, F> {
    pub fn set_breakpoints(&mut self, breakpoints: &[String]) {
        self.breakpoints.clear();
        for bp in breakpoints {
            if let Some(pos) = parse_pos(bp) {
                self.breakpoints.push(pos);
            } else {
                eprintln!("invalid breakpoint '{bp}'");
            }
        }
    }

    pub fn run(&mut self) {
        // greet user
        println!("turtle debugger v{}", env!("CARGO_PKG_VERSION"));
        println!("press space to run, press h for help");

        // async stuff
        let waker = TurtleWaker::get_waker();
        let mut ctx = std::task::Context::from_waker(&waker);
        while self.fut.as_mut().poll(&mut ctx).is_pending() {
            // main debug loop (the `while` above)
            // executed before and after each statement

            if *self.finished.lock() {
                return;
            }

            let action = self
                .get_action()
                .expect("task should always deliver action");

            if self.narrate {
                if let DbgAction::AfterStmt(stmt) = *action {
                    stmt.narrate(&self.prog.symbols);
                }
            }

            // while used to wait for user interaction if required
            let mut waiting_input = true;
            while waiting_input {
                let keys = match self.turtle.lock().window.keys_pressed() {
                    Ok(codes) => codes,
                    Err(super::window::ExitPressed) => return,
                };

                for key in &keys {
                    match *key {
                        Keycode::SPACE => {
                            if self.state == DbgState::Idle {
                                self.state = DbgState::Running;
                            } else if self.state == DbgState::Running {
                                self.state = DbgState::Idle;
                            }
                        }
                        Keycode::S if self.state == DbgState::Idle => {
                            self.state = DbgState::Step;
                        }
                        Keycode::O if self.state == DbgState::Idle => {
                            self.state = DbgState::StepOver(self.turtle.lock().stack.len());
                        }
                        Keycode::T if self.state == DbgState::Idle => {
                            self.state = DbgState::SkipTurtle;
                        }
                        Keycode::D if self.state == DbgState::Idle => {
                            self.state = DbgState::SkipDraw;
                        }
                        Keycode::N => {
                            self.narrate = !self.narrate;
                            println!("narrator is {}", if self.narrate { "ON" } else { "OFF" });
                        }
                        Keycode::V => {
                            self.turtle.lock().dump_vars(&self.prog.symbols);
                        }
                        Keycode::P => {
                            let func = self
                                .turtle
                                .lock()
                                .stack
                                .last()
                                .unwrap()
                                .0
                                .map(|id| {
                                    self.prog
                                        .symbols
                                        .get_index(id)
                                        .expect("missing function name")
                                        .0
                                        .as_str()
                                })
                                .unwrap_or("main");
                            println!("in function {func} at {}", action.get_pos());
                        }
                        Keycode::H => {
                            println!("{DEBUG_HELP}");
                        }
                        k => {
                            eprintln!("unknown key {k}");
                        }
                    }
                }

                self.handle_breakpoints(action);

                // better default
                waiting_input = false;
                match self.state {
                    DbgState::Idle => {
                        if *action == DbgAction::BeforeStmt {
                            std::thread::sleep(std::time::Duration::from_millis(20));
                            waiting_input = true;
                        }
                    }
                    DbgState::Running => {}
                    DbgState::Step => {
                        if action.is_after() {
                            self.state = DbgState::Idle;
                        }
                    }
                    DbgState::StepOver(depth) => {
                        if action.is_after() && self.turtle.lock().stack.len() == depth {
                            self.state = DbgState::Idle;
                        }
                    }
                    DbgState::SkipTurtle => {
                        if let DbgAction::AfterStmt(stmt) = *action {
                            if stmt.kind() >= StmtKind::Turtle {
                                self.state = DbgState::Idle;
                            }
                        }
                    }
                    DbgState::SkipDraw => {
                        if let DbgAction::AfterStmt(stmt) = *action {
                            if stmt.kind() == StmtKind::Draw {
                                self.state = DbgState::Idle;
                            }
                        }
                    }
                }
            }
        }
    }

    fn get_action(&mut self) -> Result<Pos<DbgAction<'p>>, GetActionError> {
        let mut act = None;
        loop {
            match self.actions.try_recv() {
                Ok((DbgAction::BlockEntered, pos)) => self.last_pos = pos,
                Ok((DbgAction::AfterStmt(stmt), pos)) => {
                    act = Some(DbgAction::AfterStmt(stmt).attach_pos(pos));
                    self.last_pos = pos;
                }
                Ok((DbgAction::BeforeStmt, pos)) => {
                    act = Some(DbgAction::BeforeStmt.attach_pos(pos))
                }
                Err(TryRecvError::Disconnected) => return Err(GetActionError::SenderDropped),
                Err(TryRecvError::Empty) => return act.ok_or(GetActionError::NoAction),
            }
        }
    }

    fn handle_breakpoints(&mut self, action: Pos<DbgAction<'p>>) {
        if self.state == DbgState::Running
            && *action == DbgAction::BeforeStmt
            && !self.breakpoints.is_empty()
        {
            for &bp in &self.breakpoints {
                if self.last_pos < bp && bp <= action.get_pos() {
                    self.state = DbgState::Idle;
                    break;
                }
            }
            self.last_pos = action.get_pos();
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum GetActionError {
    #[error("no action sent by task")]
    NoAction,
    #[error("sender dropped by task")]
    SenderDropped,
}
