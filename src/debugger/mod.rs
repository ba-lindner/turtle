use std::{
    rc::Rc, sync::Arc, task::{Wake, Waker}, thread, time::Duration
};

use parking_lot::Mutex;
use varlist::VarList;
use window::{Window, WindowEvent};

use crate::{features::{Feature, FeatureState}, pos::FilePos, tokens::{PredefVar, Statement, StmtKind, Value}, TProgram};
use runner::DebugRunner;

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
    Split(usize, Vec<Value>),
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

enum EventKind {
    Mouse,
    Key,
}

struct GlobalCtx<W> {
    vars: Mutex<VarList>,
    args: Mutex<[Value; 9]>,
    delay: Mutex<f64>,
    wait_end: Mutex<bool>,
    window: Mutex<W>,
}

impl<W: Window> GlobalCtx<W> {
    pub fn get_var(&self, var: PredefVar) -> Value {
        match var {
            PredefVar::Arg(i) => self.args.lock()[i].clone(),
            PredefVar::MaxX => Value::Number(self.window.lock().get_max_x()),
            PredefVar::MaxY => Value::Number(self.window.lock().get_max_y()),
            PredefVar::Delay => Value::Number(*self.delay.lock()),
            _ => unreachable!()
        }
    }

    pub fn set_var(&self, var: PredefVar, val: Value) {
        match var {
            PredefVar::MaxX => self.window.lock().set_max_x(val.num()),
            PredefVar::MaxY => self.window.lock().set_max_y(val.num()),
            PredefVar::Delay => *self.delay.lock() = val.num(),
            _ => unreachable!()
        }
    }
}

pub struct DebugRun<'p, W> {
    prog: &'p TProgram,
    ctx: Rc<GlobalCtx<W>>,
    turtles: Vec<DebugRunner<'p, W>>
}

impl<'p, W: Window + 'p> DebugRun<'p, W> {
    pub fn new(prog: &'p TProgram, args: &[String], window: W, debug: bool, breakpoints: Vec<FilePos>) -> Self {
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
            args: Mutex::new(args),
            delay: Mutex::new(1.0),
            wait_end: Mutex::new(false),
            window: Mutex::new(window),
        });
        let runner = DebugRunner::new(prog, ctx.clone(), debug);
        Self {
            prog,
            ctx,
            turtles: vec![runner]
        }
    }

    pub fn run(&mut self) {
        while !self.turtles.is_empty() {
            self.turtles[0].run_sleep();
            self.sync_turtles();
            thread::sleep(Duration::from_millis(*self.ctx.delay.lock() as u64));
            for evt in self.ctx.window.lock().events() {
                match evt {
                    WindowEvent::WindowExited => return,
                    WindowEvent::KeyPressed(_) => todo!(),
                    WindowEvent::MouseClicked(_, _) => todo!(),
                }
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
    fn sync_turtles(&mut self) {
        fn run_part<'p, W: Window + 'p>(turtles: &mut [DebugRunner<'p, W>]) -> Vec<DebugRunner<'p, W>> {
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
    }

    fn after_draw(&mut self) -> bool {
        thread::sleep(Duration::from_millis(*self.ctx.delay.lock() as u64));
        if self.turtles.is_empty() {
            return true;
        }
        let eventing = self.prog.features[Feature::Events] == FeatureState::Enabled;
        for evt in self.ctx.window.lock().events() {
            match evt {
                WindowEvent::WindowExited => return true,
                WindowEvent::KeyPressed(_) => todo!(),
                WindowEvent::MouseClicked(_, _) => todo!(),
            }
        }
        // check window events
        // start event turtles
        // set 'main thread' values (narrator, breakpoints)
        false
    }

    pub fn run_debug(&mut self) {
        println!("turtle debugger v{}", env!("CARGO_PKG_VERSION"));
        println!("enter 'help' to view available commands");

        #[derive(Debug, PartialEq, Eq, Clone, Copy)]
        enum DbgCommand {
            StepSingle(usize),
            StepOver(usize),
            StepTurtle,
            StepDraw,
            Run,
            ToggleNarrate,
            Vardump,
            CurrPos,
        }

        fn get_command() -> Option<DbgCommand> {
            const DEBUG_HELP: &str = "available commands:
  run                 - run / stop debugger
  step [count]        - execute single statement
  step over [count]   - step over path / calc call
  step turtle         - skip until next turtle action
  step draw           - skip until next drawn line
  narrator            - toggle narrator
  variables           - dump variables
  position            - print current position
  help                - show this help";

            loop {
                let line = std::io::stdin().lines().next()?.ok()?;
                if line.is_empty() {
                    return None;
                }
                let mut words = line.split_whitespace();
                let cmd = words.next().unwrap();
                if "step".starts_with(cmd) {
                    match words.next() {
                        Some(over) if "over".starts_with(over) => {
                            match words.next().map(str::parse) {
                                Some(Ok(count)) => return Some(DbgCommand::StepOver(count)),
                                Some(Err(why)) => eprintln!("not a number: {why}"),
                                None => return Some(DbgCommand::StepOver(1)),
                            }
                        }
                        Some(turtle) if "turtle".starts_with(turtle) => return Some(DbgCommand::StepTurtle),
                        Some(draw) if "draw".starts_with(draw) => return Some(DbgCommand::StepDraw),
                        Some(count) => {
                            match count.parse() {
                                Ok(count) => return Some(DbgCommand::StepSingle(count)),
                                Err(why) => eprintln!("not a number: {why}"),
                            }
                        }
                        None => return Some(DbgCommand::StepSingle(1)),
                    }
                } else if "run".starts_with(cmd) {
                    return Some(DbgCommand::Run);
                } else if "narrator".starts_with(cmd) {
                    return Some(DbgCommand::ToggleNarrate);
                } else if "variables".starts_with(cmd) {
                    return Some(DbgCommand::Vardump);
                } else if "position".starts_with(cmd) {
                    return Some(DbgCommand::CurrPos);
                } else if "help".starts_with(cmd) {
                    println!("{DEBUG_HELP}");
                } else if "quit".starts_with(cmd) {
                    return None;
                } else {
                    eprintln!("unknown command {cmd}");
                }
            }
        }

        let Some(cmd) = get_command() else {
            return;
        };
        match cmd {
            DbgCommand::StepSingle(count) => for _ in 0..count { self.step_single(); }
            DbgCommand::StepOver(count) => for _ in 0..count { self.step_over(); }
            DbgCommand::StepTurtle => self.step_turtle(),
            DbgCommand::StepDraw => self.step_draw(),
            DbgCommand::Run => self.run_breakpoints(),
            DbgCommand::ToggleNarrate => self.turtles[0].narrate = !self.turtles[0].narrate,
            DbgCommand::Vardump => self.turtles[0].dump_vars(),
            DbgCommand::CurrPos => self.turtles[0].print_pos(),
        }
    }

    fn step_single(&mut self) -> StmtKind {
        let kind = self.turtles[0].step_single();
        if kind == StmtKind::Draw {
            self.sync_turtles();
        }
        kind
    }

    fn step_over(&mut self) {
        let stack_size = self.turtles[0].stack_size();
        self.step_single();
        while self.turtles[0].stack_size() > stack_size {
            self.step_single();
        }
    }

    fn step_turtle(&mut self) {

    }

    fn step_draw(&mut self) {

    }

    fn run_breakpoints(&mut self) {

    }

    fn finished(&self) {
        if *self.ctx.wait_end.lock() {
            println!("halt and catch fire");
            while !self.ctx.window.lock().events().contains(&WindowEvent::WindowExited){
                thread::sleep(Duration::from_millis(200));
            }
        }
    }
}