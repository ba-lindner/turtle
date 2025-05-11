//! Debugger Interface
//! 
//! This module contains the trait for debug interfaces, [`DbgInterface`],
//! aswell as several implementations:
//! * [`Terminal`] is the default interface.
//! * [`VSCode`] is intended to be used as part of a vs code extension
//! * [`Shell`] is a turtle shell to test out ideas
//! * [`Strings`] has the default interfaces, but works on strings instead of stdin/stdout

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
mod strings;
mod terminal;
mod vscode;

use super::{window::Window, Debugger, ProgEnd};

pub use shell::Shell;
pub use strings::Strings;
pub use terminal::Terminal;
pub use vscode::VSCode;

/// A user interface for debugging purposes.
/// 
/// Every type implementing this interface is expected
/// to have some form of user interaction with which it
/// can control the debugger passed to [`exec()`](DbgInterface::exec()).
pub trait DbgInterface {
    /// Run a debugging session.
    /// 
    /// This function is supposed to contain the 'main loop'
    /// of the turtle debugger, meaning that it should run for
    /// as long as the user inputs any commands (and the program
    /// didn't finish yet). To process user input, the [`Debugger`]
    /// passed to this function contains a number of functions
    /// for running, manipulation and interrogation purposes.
    /// 
    /// The return value indicates whether the program exited
    /// regularly ([`ProgEnd::AllTurtlesFinished`]) or was
    /// terminated by the user ([`ProgEnd::WindowExited`]).
    /// In the former case, the window might be kept open until
    /// closed by the user (if `stop` was executed).
    /// 
    /// Any errors that happen are supposed to be communicated
    /// to the user directly. 
    fn exec<'p, W: Window + 'p>(&mut self, run: &mut Debugger<'p, W>) -> ProgEnd;
}

/// A common [`DbgInterface`].
/// 
/// By implementing this trait, types can get a
/// common implementation of the `DbgInterface`, where
/// repeatedly a command is first obtained from user
/// input and then executed.
trait CommonInterface {
    type Command;

    /// An optional greeting.
    /// 
    /// If a value is returned, it is printed
    /// at the beginning of a debugging session.
    fn greeting(&self) -> Option<&str> {
        None
    }

    /// Obtain the next command to execute.
    /// 
    /// [`None`] can be returned to indicate that
    /// the user ended the debugger. In this case,
    /// [`DbgInterface::exec()`] will return [`ProgEnd::WindowExited`].
    fn get_command(&mut self) -> Option<Self::Command>;

    /// Execute a command.
    /// 
    /// Do whatever it is that you have to do.
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
