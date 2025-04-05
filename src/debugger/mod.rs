use std::{
    rc::Rc,
    sync::Arc,
    task::{Wake, Waker},
    thread,
    time::Duration,
};

use commands::DbgCommand;
use parking_lot::Mutex;
use turtle::Turtle;
use varlist::VarList;
use window::{Window, WindowEvent};

use crate::{
    features::{Feature, FeatureState},
    pos::FilePos,
    tokens::{EventKind, PredefVar, Statement, StmtKind, Value},
    TProgram,
};
use runner::{DebugRunner, StepResult};

pub mod commands;
mod runner;
mod task;
mod turtle;
mod varlist;
pub mod window;

// have a cat

/// a point in the turtle coordinate system
type TCoord = (f64, f64);
/// a color in the turtle color space
type TColor = (f64, f64, f64);

const START_COLOR: TColor = (100.0, 100.0, 0.0);

#[derive(Debug, PartialEq, Clone)]
enum DbgAction<'s> {
    BlockEntered,
    BeforeStmt,
    AfterStmt(&'s Statement),
    Sleep,
    Finished(/*should wait*/ bool),
    Split(usize, Vec<Value>, Box<Turtle>),
}

struct TurtleWaker;

impl TurtleWaker {
    fn get_waker() -> Waker {
        Arc::new(Self).into()
    }
}

impl Wake for TurtleWaker {
    fn wake(self: std::sync::Arc<Self>) {}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProgEnd {
    WindowExited,
    AllTurtlesFinished,
}

struct GlobalCtx<W> {
    vars: Mutex<VarList>,
    args: [Value; 9],
    delay: Mutex<f64>,
    wait_end: Mutex<bool>,
    window: Mutex<W>,
    debug: bool,
    breakpoints: Mutex<Vec<FilePos>>,
}

impl<W: Window> GlobalCtx<W> {
    pub fn get_var(&self, var: PredefVar) -> Value {
        match var {
            PredefVar::Arg(i) => self.args[i - 1].clone(),
            PredefVar::MaxX => Value::Number(self.window.lock().get_max_x()),
            PredefVar::MaxY => Value::Number(self.window.lock().get_max_y()),
            PredefVar::Delay => Value::Number(*self.delay.lock()),
            _ => unreachable!(),
        }
    }

    pub fn set_var(&self, var: PredefVar, val: Value) {
        match var {
            PredefVar::MaxX => self.window.lock().set_max_x(val.num()),
            PredefVar::MaxY => self.window.lock().set_max_y(val.num()),
            PredefVar::Delay => *self.delay.lock() = val.num(),
            _ => unreachable!(),
        }
    }
}

pub struct DebugRun<'p, W> {
    prog: &'p TProgram,
    ctx: Rc<GlobalCtx<W>>,
    turtles: Vec<DebugRunner<'p, W>>,
}

impl<'p, W: Window + 'p> DebugRun<'p, W> {
    pub fn new(
        prog: &'p TProgram,
        args: &[String],
        window: W,
        debug: bool,
        breakpoints: Vec<FilePos>,
    ) -> Self {
        let args = std::array::from_fn(|idx| {
            let arg = args.get(idx).map(String::as_str).unwrap_or_default();
            if prog.features[Feature::Types] == FeatureState::Enabled {
                Value::String(arg.to_string())
            } else {
                Value::Number(arg.parse().unwrap_or_default())
            }
        });
        let ctx = Rc::new(GlobalCtx {
            vars: Mutex::new(VarList::new()),
            args,
            delay: Mutex::new(1.0),
            wait_end: Mutex::new(false),
            window: Mutex::new(window),
            debug,
            breakpoints: Mutex::new(breakpoints),
        });
        let runner = DebugRunner::new(prog, ctx.clone());
        Self {
            prog,
            ctx,
            turtles: vec![runner],
        }
    }

    pub fn run(&mut self) {
        while !self.turtles.is_empty() {
            self.turtles[0].run_sleep();
            match self.sync_turtles() {
                Ok(()) => {}
                Err(ProgEnd::AllTurtlesFinished) => break,
                Err(ProgEnd::WindowExited) => return,
            }
        }
        self.finished();
    }

    /// synchronize turtles
    ///
    /// run after main turtle has drawn a line
    ///
    /// this will execute all *other* turtles until the next line
    /// draw and then remove finished turtles
    ///
    /// afterwards, @delay is applied and events are handled
    fn sync_turtles(&mut self) -> Result<(), ProgEnd> {
        fn run_part<'p, W: Window + 'p>(
            turtles: &mut [DebugRunner<'p, W>],
        ) -> Vec<DebugRunner<'p, W>> {
            let mut res = Vec::new();
            for ttl in turtles {
                ttl.run_sleep();
                res.append(&mut ttl.poll_splits());
            }
            res
        }
        let mut splits = self.turtles[0].poll_splits();
        splits.append(&mut run_part(&mut self.turtles[1..]));
        while !splits.is_empty() {
            let new_splits = run_part(&mut splits);
            self.turtles.append(&mut splits);
            splits = new_splits;
        }
        self.turtles.retain(|ttl| !ttl.finished);
        if self.turtles.is_empty() {
            return Err(ProgEnd::AllTurtlesFinished);
        }
        thread::sleep(Duration::from_millis(*self.ctx.delay.lock() as u64));
        let eventing = self.prog.features[Feature::Events] == FeatureState::Enabled;
        for evt in self.ctx.window.lock().events() {
            match evt {
                WindowEvent::WindowExited => return Err(ProgEnd::WindowExited),
                WindowEvent::KeyPressed(c) => {
                    if eventing && self.prog.key_event.is_some() {
                        self.turtles.push(DebugRunner::for_event(
                            self.prog,
                            self.ctx.clone(),
                            EventKind::Key,
                            vec![Value::String(c.to_string())],
                        ));
                    }
                }
                WindowEvent::MouseClicked(coord, btn) => {
                    if eventing && self.prog.mouse_event.is_some() {
                        self.turtles.push(DebugRunner::for_event(
                            self.prog,
                            self.ctx.clone(),
                            EventKind::Mouse,
                            vec![
                                Value::Number(coord.0),
                                Value::Number(coord.1),
                                Value::Boolean(btn),
                            ],
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    pub fn run_debug(&mut self) {
        println!("turtle debugger v{}", env!("CARGO_PKG_VERSION"));
        println!("enter 'help' to view available commands");
        loop {
            let Some(cmd) = commands::get_command() else {
                return;
            };
            match self.exec_cmd(cmd) {
                Ok(()) => {}
                Err(ProgEnd::AllTurtlesFinished) => {
                    self.finished();
                    return;
                }
                Err(ProgEnd::WindowExited) => return,
            }
        }
    }

    pub fn exec_cmd(&mut self, cmd: DbgCommand) -> Result<(), ProgEnd> {
        match cmd {
            DbgCommand::StepSingle(count) => {
                for _ in 0..count {
                    self.step_single()?;
                }
            }
            DbgCommand::StepOver(count) => {
                for _ in 0..count {
                    self.step_over()?;
                }
            }
            DbgCommand::StepTurtle => self.step_kind(StmtKind::Turtle)?,
            DbgCommand::StepDraw => self.step_kind(StmtKind::Draw)?,
            DbgCommand::StepSync => self.step_sync()?,
            DbgCommand::Run => self.run_breakpoints()?,
            DbgCommand::ToggleNarrate => self.turtles[0].narrate = !self.turtles[0].narrate,
            DbgCommand::Vardump => self.turtles[0].dump_vars(),
            DbgCommand::CurrPos => self.turtles[0].print_pos(),
        }
        Ok(())
    }

    fn step_single(&mut self) -> Result<Option<StmtKind>, ProgEnd> {
        match self.turtles[0].step_single() {
            StepResult::Exec(kind) => Ok(Some(kind)),
            StepResult::Sync => {
                self.sync_turtles()?;
                // sync only _during_ walk / clear / wait
                // so next call to DebugRunner::step_single will return StepResult::Exec
                // thus this is not really recursion
                self.step_single()
            }
            StepResult::Finished => {
                self.sync_turtles()?;
                Ok(None)
            }
            StepResult::Breakpoint => Ok(None),
        }
    }

    fn step_over(&mut self) -> Result<(), ProgEnd> {
        let stack_size = self.turtles[0].stack_size();
        while self.step_single()?.is_some() && self.turtles[0].stack_size() > stack_size {}
        Ok(())
    }

    fn step_kind(&mut self, kind: StmtKind) -> Result<(), ProgEnd> {
        while self.step_single()?.is_some_and(|k| k < kind) {}
        Ok(())
    }

    fn step_sync(&mut self) -> Result<(), ProgEnd> {
        if self.turtles[0].run_breakpoints() != StepResult::Breakpoint {
            self.sync_turtles()?;
        }
        Ok(())
    }

    fn run_breakpoints(&mut self) -> Result<(), ProgEnd> {
        loop {
            match self.turtles[0].run_breakpoints() {
                StepResult::Exec(_) => unreachable!(),
                StepResult::Sync => {
                    self.sync_turtles()?;
                }
                StepResult::Breakpoint => return Ok(()),
                StepResult::Finished => {
                    self.sync_turtles()?;
                    return Ok(());
                }
            }
        }
    }

    fn finished(&self) {
        if *self.ctx.wait_end.lock() {
            println!("halt and catch fire");
            while !self
                .ctx
                .window
                .lock()
                .events()
                .contains(&WindowEvent::WindowExited)
            {
                thread::sleep(Duration::from_millis(200));
            }
        }
    }
}
