use std::{
    fmt::Display,
    sync::mpsc::{self, Receiver, Sender},
};

use crate::{
    Disp,
    debugger::{Debugger, ProgEnd, window::Window},
};

use super::{
    CommonInterface,
    commands::{self, DbgCommand, NoCmdReason},
};

/// The "output" type of the [`Strings`] debug interface.
type StringRes = Result<String, String>;

/// Generic [`DbgInterface`](super::DbgInterface) with default commands.
///
/// This type offers the same user interface as the default
/// [`Terminal`](super::Terminal) one, but uses an [`Iterator`] for input
/// and a [`mpsc::channel`] for output instead of stdin/stdout.
/// All parsing from strings and formatting into strings is also
/// handled by this interface.
///
/// Since [`Receiver`] also implements [`IntoIterator`], this
/// interface can also be used with a channel for both input
/// and output.
///
/// [`DbgInterface`](super::DbgInterface) is implemented via [`CommonInterface`].
pub struct Strings<I> {
    /// Iterator to draw user input from
    inputs: I,
    /// Sender to dump output into
    outputs: Sender<StringRes>,
}

impl Strings<std::iter::Empty<String>> {
    /// Construct a new [`Strings`] instance together with the output channel.
    pub fn construct<I: IntoIterator<Item = String>>(
        inp: I,
    ) -> (Strings<I::IntoIter>, Receiver<StringRes>) {
        let (tx, rx) = mpsc::channel();
        (Strings::new(inp.into_iter(), tx), rx)
    }
}

impl<I: Iterator<Item = String>> Strings<I> {
    /// Construct a new [`Strings`] instance from an already existing channel for output.
    pub fn new(inputs: I, outputs: Sender<StringRes>) -> Self {
        Self { inputs, outputs }
    }

    fn send(&self, msg: Result<String, String>) -> Result<(), ProgEnd> {
        self.outputs.send(msg).map_err(|_| ProgEnd::WindowExited)
    }

    fn ok<T: Display>(&self, t: T) -> Result<(), ProgEnd> {
        self.send(Ok(t.to_string()))
    }

    fn err<T: Display>(&self, t: T) -> Result<(), ProgEnd> {
        self.send(Err(t.to_string()))
    }
}

impl<I: Iterator<Item = String>> CommonInterface for Strings<I> {
    type Command = DbgCommand;

    fn get_command(&mut self) -> Option<Self::Command> {
        loop {
            match commands::parse_command(&self.inputs.next()?) {
                Ok(cmd) => return Some(cmd),
                Err(NoCmdReason::Quit) => return None,
                Err(NoCmdReason::Empty) => {}
                Err(NoCmdReason::Help(help)) => _ = self.ok(help),
                Err(NoCmdReason::Err(why)) => _ = self.err(why),
            }
        }
    }

    fn exec_cmd<'p, W: Window + 'p>(
        &mut self,
        run: &mut Debugger<'p, W>,
        cmd: Self::Command,
    ) -> Result<(), ProgEnd> {
        let res = cmd.exec(run)?;
        if let Some(out) = res.output {
            self.ok(out.with_symbols(&run.prog.symbols))?;
        }
        if let Some(why) = res.error {
            self.err(why)?;
        }
        if res.stmt_count > 0 {
            self.ok(format!("executed {} statements", res.stmt_count))?;
        }
        for evt in res.run_events {
            self.ok(evt)?;
        }
        Ok(())
    }
}
