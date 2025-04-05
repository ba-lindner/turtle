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

struct Breakpoint {
    id: usize,
    enabled: bool,
    pos: FilePos,
}

struct GlobalCtx<W> {
    vars: Mutex<VarList>,
    args: [Value; 9],
    delay: Mutex<f64>,
    wait_end: Mutex<bool>,
    window: Mutex<W>,
    debug: bool,
    breakpoints: Mutex<Vec<Breakpoint>>,
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

    pub fn breakpoint_hit(&self, last_pos: FilePos, curr_pos: FilePos) -> Option<usize> {
        for bp in &*self.breakpoints.lock() {
            if bp.enabled && last_pos < bp.pos && bp.pos < curr_pos {
                return Some(bp.id);
            }
        }
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DbgEvent {
    TurtleFinished(usize),
    BreakpointHit(usize),
}

pub struct DebugRun<'p, W> {
    prog: &'p TProgram,
    ctx: Rc<GlobalCtx<W>>,
    turtles: Vec<(usize, DebugRunner<'p, W>)>,
    turtle_id: usize,
    breakpoint_id: usize,
    active_turtle: usize,
    stmt_count: usize,
    events: Vec<DbgEvent>,
    is_sync: bool,
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
        let breakpoint_id = breakpoints.len();
        let breakpoints = breakpoints
            .into_iter()
            .enumerate()
            .map(|(id, pos)| Breakpoint {
                id,
                enabled: true,
                pos,
            })
            .collect();
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
            turtles: vec![(0, runner)],
            turtle_id: 1,
            breakpoint_id,
            active_turtle: 0,
            stmt_count: 0,
            events: Vec::new(),
            is_sync: true,
        }
    }

    fn active(&mut self) -> &mut DebugRunner<'p, W> {
        &mut self.turtles[self.active_turtle].1
    }

    fn active_id(&self) -> usize {
        self.turtles[self.active_turtle].0
    }

    pub fn run(&mut self) {
        while !self.turtles.is_empty() {
            self.active().run_sleep();
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
            turtles: &mut [(usize, DebugRunner<'p, W>)],
            id: &mut usize,
        ) -> Vec<(usize, DebugRunner<'p, W>)> {
            let mut res = Vec::new();
            for (_, ttl) in turtles {
                ttl.run_sleep();
                res.append(&mut ttl.poll_splits(id));
            }
            res
        }
        let mut splits = Vec::new();
        for idx in 0..=self.active_turtle {
            splits.append(&mut self.turtles[idx].1.poll_splits(&mut self.turtle_id));
        }
        splits.append(&mut run_part(
            &mut self.turtles[self.active_turtle + 1..],
            &mut self.turtle_id,
        ));
        while !splits.is_empty() {
            let new_splits = run_part(&mut splits, &mut self.turtle_id);
            self.turtles.append(&mut splits);
            splits = new_splits;
        }
        let active_id = self.active_id();
        self.turtles.retain(|(_, ttl)| !ttl.finished);
        if self.active_turtle >= self.turtles.len() || self.active_id() != active_id {
            self.active_turtle = self
                .turtles
                .iter()
                .position(|(idx, _)| *idx == active_id)
                .unwrap_or_default();
        }
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
                        let id = self.turtle_id;
                        self.turtle_id += 1;
                        self.turtles.push((
                            id,
                            DebugRunner::for_event(
                                self.prog,
                                self.ctx.clone(),
                                EventKind::Key,
                                vec![Value::String(c.to_string())],
                            ),
                        ));
                    }
                }
                WindowEvent::MouseClicked(coord, btn) => {
                    if eventing && self.prog.mouse_event.is_some() {
                        let id = self.turtle_id;
                        self.turtle_id += 1;
                        self.turtles.push((
                            id,
                            DebugRunner::for_event(
                                self.prog,
                                self.ctx.clone(),
                                EventKind::Mouse,
                                vec![
                                    Value::Number(coord.0),
                                    Value::Number(coord.1),
                                    Value::Boolean(btn),
                                ],
                            ),
                        ));
                    }
                }
            }
        }
        self.is_sync = true;
        Ok(())
    }

    fn after_sync(&mut self) {
        if self.is_sync {
            for idx in 0..self.active_turtle {
                self.turtles[idx].1.run_sleep();
            }
            self.is_sync = false;
        }
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
            DbgCommand::ToggleNarrate => {
                self.active().narrate = !self.active().narrate;
                println!("narrator is {}", if self.active().narrate { "ON" } else { "OFF" });
            }
            DbgCommand::Vardump => self.active().dump_vars(),
            DbgCommand::CurrPos => {
                let pos = self.active().curr_pos();
                println!(
                    "turtle #{} is currently in {} at {}",
                    self.active_id(),
                    pos.0.disp(&self.prog.symbols),
                    pos.1
                );
            }
            DbgCommand::ListTurtles => {
                println!("turtles are {}in sync", if self.is_sync { "" } else { "NOT " });
                println!("active turtle: #{}", self.active_id());
                for (id, ttl) in &self.turtles {
                    let func = ttl.start_task.0.disp(&self.prog.symbols);
                    print!("#{id:<3} {func}");
                    for val in &ttl.start_task.1 {
                        print!(" {val}");
                    }
                    println!()
                }
            }
            DbgCommand::SelectTurtle(id) => {
                if self.is_sync {
                    if let Some(idx) = self.turtles.iter().position(|(tid, _)| *tid == id) {
                        self.active_turtle = idx;
                        println!("turtle #{id} is now active");
                    } else {
                        eprintln!("no such turtle");
                    }
                } else {
                    eprintln!("active turtle can only be selected when turtles are synchronized");
                }
            }
            DbgCommand::ListBreakpoints => {
                for bp in &*self.ctx.breakpoints.lock() {
                    println!("#{:<3} {} {}", bp.id, if bp.enabled { "enabled " } else { "disabled" }, bp.pos);
                }
            }
            DbgCommand::AddBreakpoint(pos) => {
                let id = self.breakpoint_id;
                self.breakpoint_id += 1;
                self.ctx.breakpoints.lock().push(Breakpoint { id, enabled: true, pos });
            }
            DbgCommand::DeleteBreakpoint(id) => {
                self.ctx.breakpoints.lock().retain(|b| b.id != id);
            }
            DbgCommand::EnableBreakpoint(id, active) => {
                if let Some(bp) = self.ctx.breakpoints.lock().iter_mut().find(|b| b.id == id) {
                    bp.enabled = active;
                } else {
                    eprintln!("no breakpoint #{id}");
                }
            }
        }
        if self.stmt_count > 0 {
            println!("executed {} statements", self.stmt_count);
            self.stmt_count = 0;
        }
        for evt in self.events.drain(..) {
            match evt {
                DbgEvent::TurtleFinished(id) => println!("turtle #{id} finished"),
                DbgEvent::BreakpointHit(id) => println!("breakpoint #{id} hit"),
            }
        }
        Ok(())
    }

    fn collect_events(&mut self, res: StepResult) {
        match res {
            StepResult::Exec(_) => self.stmt_count += 1,
            StepResult::Sync => {}
            StepResult::Breakpoint(idx) => self.events.push(DbgEvent::BreakpointHit(idx)),
            StepResult::Finished => self
                .events
                .push(DbgEvent::TurtleFinished(self.turtles[self.active_turtle].0)),
        }
    }

    fn step_single(&mut self) -> Result<Option<StmtKind>, ProgEnd> {
        self.after_sync();
        let res = self.active().step_single();
        self.collect_events(res);
        match res {
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
            StepResult::Breakpoint(_) => Ok(None),
        }
    }

    fn step_over(&mut self) -> Result<(), ProgEnd> {
        let stack_size = self.active().stack_size();
        while self.step_single()?.is_some() && self.active().stack_size() > stack_size {}
        Ok(())
    }

    fn step_kind(&mut self, kind: StmtKind) -> Result<(), ProgEnd> {
        while self.step_single()?.is_some_and(|k| k < kind) {}
        Ok(())
    }

    fn step_sync(&mut self) -> Result<(), ProgEnd> {
        self.after_sync();
        let res = self.active().run_breakpoints();
        self.collect_events(res);
        if !matches!(res, StepResult::Breakpoint(_)) {
            self.sync_turtles()?;
        }
        Ok(())
    }

    fn run_breakpoints(&mut self) -> Result<(), ProgEnd> {
        loop {
            self.after_sync();
            let res = self.active().run_breakpoints();
            self.collect_events(res);
            match res {
                StepResult::Exec(_) => unreachable!(),
                StepResult::Sync | StepResult::Finished => {
                    self.sync_turtles()?;
                }
                StepResult::Breakpoint(_) => return Ok(()),
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
