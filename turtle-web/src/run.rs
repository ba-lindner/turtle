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
use turtle::{
    TProgram,
    debugger::{
        config::RunConfig,
        interface::{
            ChannelInterface,
            commands::{CmdResult, DbgCommand},
        },
        window::{ChannelWindow, WindowCmd, WindowEvent},
    },
};

pub struct Run {
    pub prog: Arc<TProgram>,
    commands: Receiver<WindowCmd>,
    events: Sender<WindowEvent>,
    inputs: Sender<DbgCommand>,
    outputs: Receiver<CmdResult>,
    last_change: Instant,
    window: Vec<WindowCmd>,
    finished: Arc<AtomicBool>,
}

impl Run {
    pub fn start(prog: TProgram, args: Vec<String>) -> Self {
        let prog = Arc::new(prog);
        let t_prog = Arc::clone(&prog);
        let (window, commands, events) = ChannelWindow::construct();
        let (in_tx, in_rx) = mpsc::channel();
        let (interface, outputs) = ChannelInterface::construct(in_rx);
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
            inputs: in_tx,
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

    pub fn exec_cmd(&mut self, cmd: DbgCommand) -> Result<CmdResult> {
        self.last_change = Instant::now();
        self.inputs.send(cmd)?;
        Ok(self.outputs.recv()?)
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
