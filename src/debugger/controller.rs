use std::rc::Rc;

use parking_lot::Mutex;

use crate::{features::{Feature, FeatureState}, pos::FilePos, tokens::{EventKind, StmtKind, Value}, TProgram};

use super::{interface::DbgInterface, runner::{StepResult, TurtleRunner}, turtle::FuncType, varlist::VarList, window::{Window, WindowEvent}, Breakpoint, DbgEvent, DebugErr, FrameInfo, GlobalCtx, ProgEnd, TurtleInfo, VarDump};

pub struct DebugController<'p, W> {
    pub prog: &'p TProgram,
    ctx: Rc<GlobalCtx<W>>,
    turtles: Vec<(usize, TurtleRunner<'p, W>)>,
    turtle_id: usize,
    breakpoint_id: usize,
    active_turtle: usize,
    stmt_count: usize,
    events: Vec<DbgEvent>,
    is_sync: bool,
}

impl<'p, W: Window + 'p> DebugController<'p, W> {
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
        let runner = TurtleRunner::new(prog, ctx.clone());
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

    pub fn debug_in(&mut self, mut interf: impl DbgInterface) {
        self.ctx.window.lock().init(20.0, 15.0);
        if interf.exec(self) == ProgEnd::AllTurtlesFinished {
            self.finished();
        };
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //   helper functions
    //
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    fn active(&mut self) -> &mut TurtleRunner<'p, W> {
        &mut self.turtles[self.active_turtle].1
    }

    pub fn active_id(&self) -> usize {
        self.turtles[self.active_turtle].0
    }

    fn add_turtle(&mut self, turtle: TurtleRunner<'p, W>) {
        let id = self.turtle_id;
        self.turtle_id += 1;
        self.turtles.push((id, turtle));
    }

    fn find_turtle(&self, id: usize) -> Option<usize> {
        self.turtles.iter().position(|(tid, _)| *tid == id)
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //   multithreading
    //
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /// synchronize turtles
    ///
    /// run after main turtle has drawn a line
    ///
    /// this will execute all *other* turtles until the next line
    /// draw and then remove finished turtles
    ///
    /// afterwards, @delay is applied and events are handled
    fn sync_turtles(&mut self) -> Result<(), ProgEnd> {
        let (finished, remaining) = self.turtles.split_at_mut(self.active_turtle + 1);
        let mut splits: Vec<_> = finished
            .iter_mut()
            .flat_map(|ttl| ttl.1.poll_splits())
            .chain(remaining.iter_mut().flat_map(|(_, ttl)| {
                ttl.run_sleep();
                ttl.poll_splits()
            }))
            .collect();
        while !splits.is_empty() {
            let mut new_splits = Vec::new();
            for mut ttl in splits {
                ttl.run_sleep();
                new_splits.append(&mut ttl.poll_splits());
                self.add_turtle(ttl);
            }
            splits = new_splits;
        }
        let active_id = self.active_id();
        self.turtles.retain(|(_, ttl)| !ttl.finished);
        if self.active_turtle >= self.turtles.len() || self.active_id() != active_id {
            self.active_turtle = self.find_turtle(active_id).unwrap_or_default();
        }
        if self.turtles.is_empty() {
            return Err(ProgEnd::AllTurtlesFinished);
        }
        std::thread::sleep(std::time::Duration::from_millis(*self.ctx.delay.lock() as u64));
        let events = self.ctx.window.lock().events();
        for evt in events {
            match evt {
                WindowEvent::WindowExited => return Err(ProgEnd::WindowExited),
                WindowEvent::KeyPressed(c) => {
                    if self.prog.key_event.is_some() {
                        self.add_turtle(TurtleRunner::for_event(
                            self.prog,
                            self.ctx.clone(),
                            EventKind::Key,
                            vec![Value::String(c.to_string())],
                        ));
                    }
                }
                WindowEvent::MouseClicked(coord, btn) => {
                    if self.prog.mouse_event.is_some() {
                        self.add_turtle(TurtleRunner::for_event(
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

    pub fn finished(&self) {
        if *self.ctx.wait_end.lock() {
            println!("halt and catch fire");
            while !self
                .ctx
                .window
                .lock()
                .events()
                .contains(&WindowEvent::WindowExited)
            {
                std::thread::sleep(std::time::Duration::from_millis(200));
            }
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //   interpreter
    //
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //   debug commands - execution
    //
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    fn collect_events(&mut self, res: StepResult) {
        match res {
            StepResult::Exec(_) => self.stmt_count += 1,
            StepResult::Sync => {}
            StepResult::Breakpoint(idx) => self.events.push(DbgEvent::BreakpointHit(idx)),
            StepResult::Finished => self.events.push(DbgEvent::TurtleFinished(self.active_id())),
        }
    }

    pub fn step_single(&mut self) -> Result<Option<StmtKind>, ProgEnd> {
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

    pub fn step_over(&mut self) -> Result<(), ProgEnd> {
        let stack_size = self.active().stack_size();
        while self.step_single()?.is_some() && self.active().stack_size() > stack_size {}
        Ok(())
    }

    pub fn step_out(&mut self) -> Result<(), ProgEnd> {
        let stack_size = self.active().stack_size();
        while self.step_single()?.is_some() && self.active().stack_size() >= stack_size {}
        Ok(())
    }

    pub fn step_kind(&mut self, kind: StmtKind) -> Result<(), ProgEnd> {
        while self.step_single()?.is_some_and(|k| k < kind) {}
        Ok(())
    }

    pub fn step_sync(&mut self) -> Result<(), ProgEnd> {
        self.after_sync();
        let res = self.active().run_breakpoints();
        self.collect_events(res);
        if !matches!(res, StepResult::Breakpoint(_)) {
            self.sync_turtles()?;
        }
        Ok(())
    }

    pub fn run_breakpoints(&mut self) -> Result<(), ProgEnd> {
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

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //   debug commands - configuration
    //
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    pub fn toggle_narrate(&mut self) -> bool {
        let narrator = !self.active().narrate;
        self.active().narrate = narrator;
        narrator
    }

    pub fn select_turtle(&mut self, id: usize) -> Result<(), DebugErr> {
        if !self.is_sync {
            return Err(DebugErr::TurtleSwitchNoSync);
        }
        let idx = self.find_turtle(id).ok_or(DebugErr::TurtleNotFound(id))?;
        self.active_turtle = idx;
        Ok(())
    }

    pub fn add_breakpoint(&mut self, pos: FilePos) -> usize {
        let id = self.breakpoint_id;
        self.breakpoint_id += 1;
        self.ctx.breakpoints.lock().push(Breakpoint {
            id,
            enabled: true,
            pos,
        });
        id
    }

    pub fn delete_breakpoint(&mut self, id: usize) {
        self.ctx.breakpoints.lock().retain(|bp| bp.id != id);
    }

    pub fn enable_breakpoint(&mut self, id: usize, active: bool) -> Result<(), DebugErr> {
        self.ctx
            .breakpoints
            .lock()
            .iter_mut()
            .find(|bp| bp.id == id)
            .ok_or(DebugErr::BreakpointNotFound(id))?
            .enabled = active;
        Ok(())
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //   debug commands - information
    //
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    fn active_ref(&self) -> &TurtleRunner<'p, W> {
        &self.turtles[self.active_turtle].1
    }

    pub fn vardump(&self, frame: Option<usize>) -> Result<VarDump, DebugErr> {
        self.active_ref().vardump(frame)
    }

    pub fn curr_pos(&self) -> (FuncType, FilePos) {
        self.active_ref().curr_pos()
    }

    pub fn list_turtles(&self) -> (bool, Vec<TurtleInfo>) {
        let mut ttls = Vec::new();
        for (id, ttl) in &self.turtles {
            ttls.push(TurtleInfo {
                id: *id,
                is_active: *id == self.active_id(),
                start_task: ttl.start_task.clone(),
            });
        }
        (self.is_sync, ttls)
    }

    pub fn stacktrace(&self) -> Vec<FrameInfo> {
        self.active_ref().stacktrace()
    }

    pub fn list_breakpoints(&self) -> Vec<Breakpoint> {
        self.ctx.breakpoints.lock().clone()
    }

    pub fn events(&mut self) -> (usize, Vec<DbgEvent>) {
        let res = (self.stmt_count, std::mem::take(&mut self.events));
        self.stmt_count = 0;
        res
    }
}