use std::sync::mpsc::{self, Receiver, Sender};

use crate::debugger::{Debugger, ProgEnd, interface::commands::CmdResult, window::Window};

use super::{CommonInterface, commands::DbgCommand};

pub struct ChannelInterface<I> {
    inputs: I,
    outputs: Sender<CmdResult>,
}

impl ChannelInterface<std::iter::Empty<DbgCommand>> {
    pub fn construct<I: IntoIterator<Item = DbgCommand>>(
        inp: I,
    ) -> (ChannelInterface<I::IntoIter>, Receiver<CmdResult>) {
        let (tx, rx) = mpsc::channel();
        (ChannelInterface::new(inp.into_iter(), tx), rx)
    }
}

impl<I: Iterator<Item = DbgCommand>> ChannelInterface<I> {
    pub fn new(inputs: I, outputs: Sender<CmdResult>) -> Self {
        Self { inputs, outputs }
    }
}

impl<I: Iterator<Item = DbgCommand>> CommonInterface for ChannelInterface<I> {
    type Command = DbgCommand;

    fn get_command(&mut self) -> Option<Self::Command> {
        self.inputs.next()
    }

    fn exec_cmd<'p, W: Window + 'p>(
        &mut self,
        run: &mut Debugger<'p, W>,
        cmd: Self::Command,
    ) -> Result<(), ProgEnd> {
        let (res, end) = cmd.exec(run);
        self.outputs.send(res).map_err(|_| ProgEnd::WindowExited)?;
        if let Some(end) = end {
            return Err(end)
        }
        Ok(())
    }
}
