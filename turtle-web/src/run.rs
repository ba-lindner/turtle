use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Receiver, Sender},
    },
    thread,
    time::{Duration, Instant},
};

use anyhow::Result;
use tokio::sync::mpsc::{Sender as TSender, Receiver as TReceiver};
use turtle::{
    debugger::{
        config::RunConfig,
        interface::{
            commands::{CmdResult, DbgCommand}, CommonInterface,
        },
        window::{ChannelWindow, WindowCmd, WindowEvent}, ProgEnd,
    }, TProgram
};

struct AsyncInterface {
    inputs: Receiver<DbgCommand>,
    outputs: TSender<(CmdResult, Option<ProgEnd>)>,
}

impl AsyncInterface {
    fn construct() -> (Self, Sender<DbgCommand>, TReceiver<(CmdResult, Option<ProgEnd>)>) {
        let (in_tx, in_rx) = mpsc::channel();
        let (out_tx, out_rx) = tokio::sync::mpsc::channel(100);
        (Self {
            inputs: in_rx,
            outputs: out_tx,
        }, in_tx, out_rx)
    }
}

impl CommonInterface for AsyncInterface {
    type Command = DbgCommand;

    fn get_command(&mut self) -> Option<Self::Command> {
        self.inputs.recv().ok()
    }

    fn exec_cmd<'p, W: turtle::debugger::window::Window + 'p>(
        &mut self,
        run: &mut turtle::debugger::Debugger<'p, W>,
        cmd: Self::Command,
    ) -> std::result::Result<(), turtle::debugger::ProgEnd> {
        let res = cmd.exec(run);
        let end = res.1;
        futures::executor::block_on(self.outputs.send(res)).map_err(|_| ProgEnd::WindowExited)?;
        if let Some(end) = end {
            return Err(end)
        }
        Ok(())
    }
}

pub struct Run {
    pub prog: Arc<TProgram>,
    commands: Receiver<WindowCmd>,
    events: Sender<WindowEvent>,
    inputs: Sender<DbgCommand>,
    outputs: TReceiver<(CmdResult, Option<ProgEnd>)>,
    last_change: Instant,
    window: Vec<WindowCmd>,
    finished: Arc<AtomicBool>,
}

impl Run {
    pub fn start(prog: TProgram, args: Vec<String>) -> Self {
        let prog = Arc::new(prog);
        let t_prog = Arc::clone(&prog);
        let (window, commands, events) = ChannelWindow::construct();
        let (interface, inputs, outputs) = AsyncInterface::construct();
        let finished = Arc::new(AtomicBool::new(false));
        let t_finished = Arc::clone(&finished);
        thread::spawn(move || {
            RunConfig::new(&args)
                .window(window)
                .debug_in(interface)
                .exec(&t_prog);
            t_finished.store(true, Ordering::Relaxed);
        });
        Self {
            prog,
            commands,
            events,
            inputs,
            outputs,
            last_change: Instant::now(),
            window: Vec::new(),
            finished,
        }
    }

    pub fn stop(&self) {
        _ = self.events.send(WindowEvent::WindowExited);
        self.finished.store(true, Ordering::Relaxed);
    }

    pub async fn exec_cmd(&mut self, cmd: DbgCommand) -> Result<(CmdResult, Option<ProgEnd>)> {
        self.last_change = Instant::now();
        self.inputs.send(cmd)?;
        Ok(self.outputs.recv().await.unwrap_or_default())
    }

    pub fn report_event(&mut self, event: WindowEvent) -> Result<()> {
        self.last_change = Instant::now();
        self.events.send(event)?;
        Ok(())
    }

    pub fn get_window_output(&mut self) -> &[WindowCmd] {
        self.last_change = Instant::now();
        self.window.extend(self.commands.try_iter());
        &self.window
    }

    pub fn gc(&self, max_idle: Duration) {
        if self.last_change.elapsed() > max_idle {
            _ = self.events.send(WindowEvent::WindowExited);
        }
    }

    pub fn finished(&self) -> bool {
        self.finished.load(Ordering::Relaxed)
    }
}
