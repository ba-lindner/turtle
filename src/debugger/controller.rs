use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use crate::{
    features::{Feature, FeatureState},
    pos::{FilePos, Positionable},
    prog::semcheck::{self, Vars},
    tokens::{EventKind, StmtKind, ValType, Value, VariableKind},
    TProgram, TurtleError,
};

use super::{
    interface::DbgInterface,
    runner::{StepResult, TurtleRunner},
    turtle::FuncType,
    varlist::VarList,
    window::{Window, WindowEvent},
    Breakpoint, DbgEvent, DebugErr, FrameInfo, GlobalCtx, ProgEnd, TurtleInfo, VarDump,
};

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
            vars: RefCell::new(VarList::new()),
            args,
            delay: Cell::new(1.0),
            wait_end: Cell::new(false),
            window: RefCell::new(window),
            debug,
            breakpoints: RefCell::new(breakpoints),
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
        self.ctx.window.borrow_mut().init_with(20.0, 15.0);
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
    /// run after active turtle has drawn a line
    ///
    /// this will execute all *following* turtles until the next line
    /// draw and then remove finished turtles
    ///
    /// afterwards, `@delay` is applied and events are handled
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
        let delay = self.ctx.delay.get() as u64;
        if delay > 0 {
            std::thread::sleep(std::time::Duration::from_millis(delay));
        }
        let events = self
            .ctx
            .window
            .borrow_mut()
            .events()
            .into_iter()
            .map(|evt| match evt {
                WindowEvent::WindowExited => Err(ProgEnd::WindowExited),
                WindowEvent::KeyPressed(c) => {
                    Ok((EventKind::Key, vec![Value::String(c.to_string())]))
                }
                WindowEvent::MouseClicked(coord, btn) => Ok((
                    EventKind::Mouse,
                    vec![
                        Value::Number(coord.0),
                        Value::Number(coord.1),
                        Value::Boolean(btn),
                    ],
                )),
            })
            .collect::<Result<Vec<_>, _>>()?;
        for (kind, args) in events {
            if self.prog.get_event(kind).is_some() {
                self.add_turtle(TurtleRunner::for_event(
                    self.prog,
                    self.ctx.clone(),
                    kind,
                    args,
                ));
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
        if self.ctx.wait_end.get() {
            println!("halt and catch fire");
            while !self
                .ctx
                .window
                .borrow_mut()
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
        self.ctx.window.borrow_mut().init_with(20.0, 15.0);
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

    /// Execute a single statement.
    ///
    /// This function returns:
    /// * `Err(end)` if the program finished
    /// * `Ok(None)` if the turtle finished or a breakpoint was hit
    /// * `Ok(Some(kind))` if a statement was executed
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

    /// Execute a single statement or function call.
    pub fn step_over(&mut self) -> Result<(), ProgEnd> {
        let stack_size = self.active().stack_size();
        while self.step_single()?.is_some() && self.active().stack_size() > stack_size {}
        Ok(())
    }

    /// Leave the current function.
    ///
    /// This will execute all remaining statements in the
    /// current function.
    pub fn step_out(&mut self) -> Result<(), ProgEnd> {
        let stack_size = self.active().stack_size();
        while self.step_single()?.is_some() && self.active().stack_size() >= stack_size {}
        Ok(())
    }

    /// Executes statements until the first statement with the given kind.
    ///
    /// See [`StmtKind`] for details on which statement is considered which kind.
    pub fn step_kind(&mut self, kind: StmtKind) -> Result<(), ProgEnd> {
        while self.step_single()?.is_some_and(|k| k < kind) {}
        Ok(())
    }

    /// Executes statements until all turtles are synced.
    ///
    /// This is _almost_ identical to [`step_kind`](Self::step_kind()) with [`StmtKind::Draw`].
    /// However, while `step_kind` stops _after_ the statement, this will stop
    /// _during_ the statement and thus return in a state where all
    /// turtles are synced, allowing the user to [switch turtles](Self::select_turtle).
    /// This is **not** the case for `step_kind`.
    pub fn step_sync(&mut self) -> Result<(), ProgEnd> {
        self.after_sync();
        let res = self.active().run_breakpoints();
        self.collect_events(res);
        if !matches!(res, StepResult::Breakpoint(_)) {
            self.sync_turtles()?;
        }
        Ok(())
    }

    /// Run the program until a breakpoint is hit.
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
        let narrator = !self.active().get_narrate();
        self.active().set_narrate(narrator);
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
        self.ctx.breakpoints.borrow_mut().push(Breakpoint {
            id,
            enabled: true,
            pos,
        });
        id
    }

    pub fn delete_breakpoint(&mut self, id: usize) {
        self.ctx.breakpoints.borrow_mut().retain(|bp| bp.id != id);
    }

    pub fn enable_breakpoint(&mut self, id: usize, active: bool) -> Result<(), DebugErr> {
        self.ctx
            .breakpoints
            .borrow_mut()
            .iter_mut()
            .find(|bp| bp.id == id)
            .ok_or(DebugErr::BreakpointNotFound(id))?
            .enabled = active;
        Ok(())
    }

    fn check_undef(&self, vars: Vars, variables: Vec<VariableKind>) -> Result<(), TurtleError> {
        if !vars.locals() {
            Err(TurtleError::UndefLocals(
                FuncType::Main,
                semcheck::collect_undef(variables, true),
            ))
        } else if !vars.globals() {
            Err(TurtleError::UndefGlobals(semcheck::collect_undef(
                variables, false,
            )))
        } else {
            Ok(())
        }
    }

    pub fn eval_expr(&mut self, frame: Option<usize>, expr: &str) -> Result<Value, DebugErr> {
        let mut expr = self.prog.with_parser(expr, |p| Ok(p.parse_expr()?))?;
        if expr.side_effects(self.prog, &mut Vec::new()) {
            return Err(DebugErr::ExprSideEffects);
        }
        let mut ctx = self.prog.get_context();
        let (_, vars) = expr.val_type(&mut ctx)?;
        self.check_undef(vars, expr.collect_variables())?;
        Ok(self.active().eval(expr, frame))
    }

    /// returns true if turtle finished
    pub fn exec_stmt(&mut self, stmt: &str) -> Result<bool, DebugErr> {
        let mut stmt = self.prog.with_parser(stmt, |p| Ok(p.parse_stm()?))?;
        let mut ctx = self.prog.get_context();
        let vars = stmt.semantic_check(&mut ctx)?;
        self.check_undef(vars, stmt.collect_variables())?;
        Ok(self.active().exec(stmt.into_inner()))
    }

    pub fn add_func(&mut self, func: &str) -> Result<(), DebugErr> {
        let func = self.prog.with_parser(func, |p| {
            Ok(p.parse_next().ok_or(TurtleError::ParseError(
                crate::prog::parser::ParseError::UnexpectedEnd.attach_pos(FilePos::default()),
            ))??)
        })?;
        let mut ctx = self.prog.get_context();
        match func {
            crate::tokens::ParseToken::PathDef(mut path) => {
                if !path.semantic_check(&mut ctx)? {
                    let vars = path.body.collect_variables();
                    let undef = semcheck::collect_undef(vars, false);
                    Err(TurtleError::UndefGlobals(undef))?;
                }
                if let Some(proto) = ctx.protos.get(&path.name) {
                    if proto.args != path.args {
                        return Err(DebugErr::IncompatibleArgs(FuncType::Path(path.name)));
                    }
                    self.prog
                        .extensions
                        .paths
                        .borrow_mut()
                        .retain(|p| p.name != path.name);
                }
                self.prog.extensions.paths.borrow_mut().push(path);
            }
            crate::tokens::ParseToken::CalcDef(mut calc) => {
                if !calc.semantic_check(&mut ctx)? {
                    let vars = calc.body.collect_variables();
                    let undef = semcheck::collect_undef(vars, false);
                    Err(TurtleError::UndefGlobals(undef))?;
                }
                if let Some(proto) = ctx.protos.get(&calc.name) {
                    if proto.args != calc.args {
                        return Err(DebugErr::IncompatibleArgs(FuncType::Calc(calc.name)));
                    }
                    self.prog
                        .extensions
                        .calcs
                        .borrow_mut()
                        .retain(|c| c.name != calc.name);
                }
                self.prog.extensions.calcs.borrow_mut().push(calc);
            }
            crate::tokens::ParseToken::EventHandler(kind, mut path) => {
                if !path.semantic_check(&mut ctx)? {
                    let vars = path.body.collect_variables();
                    let undef = semcheck::collect_undef(vars, false);
                    Err(TurtleError::UndefGlobals(undef))?;
                }
                let args: &[_] = match kind {
                    EventKind::Mouse => &[ValType::Number, ValType::Number, ValType::Boolean],
                    EventKind::Key => &[ValType::String],
                };
                semcheck::compare_args(kind, &path.args, args)?;
                match kind {
                    EventKind::Mouse => *self.prog.extensions.mouse_event.borrow_mut() = Some(path),
                    EventKind::Key => *self.prog.extensions.key_event.borrow_mut() = Some(path),
                }
            }
            crate::tokens::ParseToken::StartBlock(_) => return Err(DebugErr::MainBlock),
        }
        Ok(())
    }

    pub fn undef_func(&mut self, func: Option<FuncType>) -> Result<(), DebugErr> {
        if let Some(func) = func {
            match func {
                FuncType::Main => return Err(DebugErr::MainBlock),
                FuncType::Path(id) => self
                    .prog
                    .extensions
                    .paths
                    .borrow_mut()
                    .retain(|p| p.name != id),
                FuncType::Calc(id) => self
                    .prog
                    .extensions
                    .calcs
                    .borrow_mut()
                    .retain(|c| c.name != id),
                FuncType::Event(kind) => match kind {
                    EventKind::Mouse => *self.prog.extensions.mouse_event.borrow_mut() = None,
                    EventKind::Key => *self.prog.extensions.key_event.borrow_mut() = None,
                },
            }
        } else {
            self.prog.extensions.paths.borrow_mut().clear();
            self.prog.extensions.calcs.borrow_mut().clear();
            *self.prog.extensions.mouse_event.borrow_mut() = None;
            *self.prog.extensions.key_event.borrow_mut() = None;
        }
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
        self.ctx.breakpoints.borrow().clone()
    }

    pub fn events(&mut self) -> (usize, Vec<DbgEvent>) {
        let res = (self.stmt_count, std::mem::take(&mut self.events));
        self.stmt_count = 0;
        res
    }
}
