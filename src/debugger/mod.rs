use std::{
    future::Future,
    pin::pin,
    rc::Rc,
    sync::{mpsc, Arc},
    task::{Context, Wake, Waker},
};

use parking_lot::lock_api::Mutex;
use sdl2::pixels::Color;
use task::DebugTask;
use turtle::Turtle;

use crate::{tokens::Statement, TProgram};

pub mod runner;
mod task;
mod turtle;
mod varlist;
mod window;

// have a cat

const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;
const START_COLOR: Color = Color::YELLOW;

#[derive(Debug, PartialEq, Clone, Copy)]
enum DbgAction<'s> {
    BlockEntered,
    BeforeStmt,
    AfterStmt(&'s Statement),
}

impl DbgAction<'_> {
    fn is_after(&self) -> bool {
        matches!(self, Self::AfterStmt(_))
    }
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

pub struct ItpRunner<'p>(DebugTask<'p>);

impl<'p> ItpRunner<'p> {
    pub fn new(prog: &'p TProgram, args: &[String], title: &str) -> Self {
        let (action, _) = mpsc::channel();
        Self(DebugTask::new(
            prog,
            Rc::new(Mutex::new(Turtle::new(title, args))),
            Rc::new(Mutex::new(false)),
            action,
            false,
        ))
    }

    pub fn run(self) {
        let waker = TurtleWaker::get_waker();
        let mut ctx = Context::from_waker(&waker);
        let mut fut = pin!(self.0.execute());
        while fut.as_mut().poll(&mut ctx).is_pending() {}
    }
}
