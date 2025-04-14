macro_rules! match_extended {
    ($inp:expr => {
        $none:ident => $empty:expr,
        $($key:literal => $res:expr,)*
        $default:ident => $err:expr $(,)?
    }) => {
        match $inp {
            Some(inp) => {
                let extended = crate::debugger::interface::commands::extend_str(inp, &[$($key),*]);
                match extended {
                    $($key => $res,)*
                    $default => $err,
                }
            }
            $none => $empty,
        }
    };
}

mod commands;
mod shell;
mod terminal;
mod vscode;

use super::{window::Window, Debugger, ProgEnd};

pub use shell::Shell;
pub use terminal::Terminal;
pub use vscode::VSCode;

pub trait DbgInterface {
    fn exec<'p, W: Window + 'p>(&mut self, run: &mut Debugger<'p, W>) -> ProgEnd;
}

trait CommonInterface {
    type Command;

    fn greeting(&self) -> Option<&str> {
        None
    }

    fn get_command(&mut self) -> Option<Self::Command>;

    fn exec_cmd<'p, W: Window + 'p>(
        &mut self,
        run: &mut Debugger<'p, W>,
        cmd: Self::Command,
    ) -> Result<(), ProgEnd>;
}

impl<I: CommonInterface> DbgInterface for I {
    fn exec<'p, W: Window + 'p>(&mut self, run: &mut Debugger<'p, W>) -> ProgEnd {
        if let Some(greeting) = self.greeting() {
            println!("{greeting}");
        }
        while let Some(cmd) = self.get_command() {
            if let Err(end) = self.exec_cmd(run, cmd) {
                return end;
            }
        }
        ProgEnd::WindowExited
    }
}
