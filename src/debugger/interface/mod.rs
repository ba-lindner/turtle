mod commands;
mod terminal;
mod vscode;

use super::{window::Window, Debugger, ProgEnd};

pub use terminal::Terminal;
pub use vscode::VSCode;

pub trait DbgInterface {
    fn exec<'p, W: Window + 'p>(&mut self, run: &mut Debugger<'p, W>) -> ProgEnd;
}