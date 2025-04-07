mod commands;
mod terminal;
mod vscode;

use super::{window::Window, DebugRun};

pub use terminal::Terminal;

pub trait DbgInterface {
    fn exec<'p, W: Window + 'p>(&mut self, run: DebugRun<'p, W>);
}