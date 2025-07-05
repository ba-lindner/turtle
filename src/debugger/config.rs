use crate::{TProgram, pos::FilePos};

use super::{
    Debugger, Window,
    interface::{DbgInterface, Terminal},
};

#[cfg(not(feature = "sdl"))]
use super::window::VoidWindow;
#[cfg(feature = "sdl")]
use super::window::{ChannelWindow, SdlWindow};

pub struct RunConfig<'a, W, I> {
    args: &'a [String],
    window: W,
    kind: RunKind<I>,
}

pub enum RunKind<I> {
    Interpret,
    Debug(I, Vec<FilePos>),
}

#[cfg(not(feature = "sdl"))]
impl<'a> RunConfig<'a, VoidWindow, Terminal> {
    pub fn new(args: &'a [String]) -> Self {
        Self {
            args,
            window: (0.0, 0.0),
            kind: RunKind::Interpret,
        }
    }
}

#[cfg(feature = "sdl")]
impl<'a> RunConfig<'a, ChannelWindow, Terminal> {
    pub fn new(args: &'a [String]) -> Self {
        Self {
            args,
            window: SdlWindow::create("Turtle Graphics".to_string()),
            kind: RunKind::Interpret,
        }
    }
}

impl<'a, W: Window, I: DbgInterface> RunConfig<'a, W, I> {
    pub fn window<W2: Window>(self, window: W2) -> RunConfig<'a, W2, I> {
        RunConfig {
            args: self.args,
            window,
            kind: self.kind,
        }
    }

    pub fn debug_in<I2: DbgInterface>(self, interface: I2) -> RunConfig<'a, W, I2> {
        let bp = if let RunKind::Debug(_, bp) = self.kind {
            bp
        } else {
            Vec::new()
        };
        RunConfig {
            args: self.args,
            window: self.window,
            kind: RunKind::Debug(interface, bp),
        }
    }

    pub fn breakpoints(mut self, breakpoints: Vec<FilePos>) -> Self {
        if let RunKind::Debug(_, bp) = &mut self.kind {
            *bp = breakpoints;
        }
        self
    }

    pub fn exec(self, prog: &TProgram) {
        match self.kind {
            RunKind::Interpret => {
                Debugger::new(prog, self.args, self.window, false, Vec::new()).run();
            }
            RunKind::Debug(interf, breakpoints) => {
                Debugger::new(prog, self.args, self.window, true, breakpoints).debug_in(interf);
            }
        }
    }
}
