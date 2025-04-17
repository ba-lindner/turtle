use std::{
    cell::{Cell, RefCell},
    fmt::{Display, Write as _},
    sync::Arc,
    task::{Wake, Waker},
};

use indexmap::IndexMap;
use turtle::Turtle;
use varlist::VarList;
use window::Window;

use crate::{
    pos::FilePos,
    tokens::{EventKind, PredefVar, StmtKind, Value},
    SymbolTable, TurtleError,
};

pub use controller::DebugController as Debugger;
pub use turtle::FuncType;

pub mod config;
mod controller;
pub mod interface;
mod runner;
mod task;
#[cfg(test)]
mod test;
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
enum DbgAction {
    BlockEntered,
    BeforeStmt,
    AfterStmt(StmtKind),
    Sleep,
    Finished(/*should wait*/ bool),
    Split(usize, Vec<Value>, Box<Turtle>),
    CmdResult(Option<Value>),
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

impl Display for Breakpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#{:<3} {} {}",
            self.id,
            if self.enabled { "enabled " } else { "disabled" },
            self.pos
        )
    }
}

struct GlobalCtx<W> {
    vars: RefCell<VarList>,
    args: [Value; 9],
    delay: Cell<f64>,
    wait_end: Cell<bool>,
    window: RefCell<W>,
    debug: bool,
    breakpoints: RefCell<Vec<Breakpoint>>,
}

impl<W: Window> GlobalCtx<W> {
    pub fn get_var(&self, var: PredefVar) -> Value {
        match var {
            PredefVar::Arg(i) => self.args[i - 1].clone(),
            PredefVar::MaxX => Value::Number(self.window.borrow().get_max_coords().0),
            PredefVar::MaxY => Value::Number(self.window.borrow().get_max_coords().1),
            PredefVar::Delay => Value::Number(self.delay.get()),
            _ => unreachable!(),
        }
    }

    pub fn set_var(&self, var: PredefVar, val: Value) {
        match var {
            PredefVar::MaxX => self.window.borrow_mut().set_max_x(val.num()),
            PredefVar::MaxY => self.window.borrow_mut().set_max_y(val.num()),
            PredefVar::Delay => self.delay.set(val.num()),
            _ => unreachable!(),
        }
    }

    pub fn breakpoint_hit(&self, last_pos: FilePos, curr_pos: FilePos) -> Option<usize> {
        for bp in &*self.breakpoints.borrow() {
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
    pub locals: IndexMap<String, Value>,
    pub globals: IndexMap<String, Value>,
    pub predef: IndexMap<PredefVar, Value>,
}

pub struct TurtleInfo {
    pub id: usize,
    pub is_active: bool,
    pub start_task: (FuncType, Vec<Value>),
}

impl TurtleInfo {
    pub fn disp(&self, symbols: &SymbolTable) -> String {
        let func = self.start_task.0.disp(symbols);
        let mut res = format!(
            "#{:<3} {} {func}",
            self.id,
            if self.is_active { "x" } else { " " }
        );
        for val in &self.start_task.1 {
            write!(&mut res, " {val}").unwrap();
        }
        res
    }
}

pub struct FrameInfo {
    pub index: usize,
    pub func: FuncType,
    pub pos: FilePos,
}

impl FrameInfo {
    pub fn disp(&self, symbols: &SymbolTable) -> String {
        format!("{:<3} {} {}", self.index, self.func.disp(symbols), self.pos)
    }
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
    #[error("{0}")]
    TurtleError(#[from] TurtleError),
    #[error("expression has side effects")]
    ExprSideEffects,
    #[error("cannot modify main block")]
    MainBlock,
    #[error(
        "cannot replace definition for {0} as another already exists with different arguments"
    )]
    IncompatibleArgs(FuncType),
}
