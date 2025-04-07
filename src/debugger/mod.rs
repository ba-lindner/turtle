use std::{
    collections::HashMap,
    sync::Arc,
    task::{Wake, Waker},
};

use parking_lot::Mutex;
use turtle::{FuncType, Turtle};
use varlist::VarList;
use window::Window;

use crate::{
    pos::FilePos,
    tokens::{EventKind, PredefVar, Statement, Value},
};

pub use controller::DebugController as Debugger;

pub mod config;
mod controller;
pub mod interface;
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

#[derive(Debug, Clone, Copy)]
pub struct Breakpoint {
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
pub enum DbgEvent {
    TurtleFinished(usize),
    BreakpointHit(usize),
}

pub struct VarDump {
    pub locals: HashMap<String, Value>,
    pub globals: HashMap<String, Value>,
    pub predef: HashMap<PredefVar, Value>,
}

pub struct TurtleInfo {
    pub id: usize,
    pub is_active: bool,
    pub start_task: (FuncType, Vec<Value>),
}

pub struct FrameInfo {
    pub index: usize,
    pub func: FuncType,
    pub pos: FilePos,
}

#[derive(Debug, thiserror::Error)]
pub enum DebugErr {
    #[error("no turtle #{0}")]
    TurtleNotFound(usize),
    #[error("no breakpoint #{0}")]
    BreakpointNotFound(usize),
    #[error("active turtle cannot be switched when not synced")]
    TurtleSwitchNoSync,
    #[error("no frame #{0}")]
    FrameNotFound(usize),
}
