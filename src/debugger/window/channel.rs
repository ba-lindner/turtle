use std::sync::mpsc::{self, Receiver, Sender};

use crate::debugger::{TColor, TCoord};

use super::{Window, WindowCmd, WindowEvent};

type InitFn = Box<dyn FnOnce()>;

pub struct ChannelWindow {
    max_coord: (f64, f64),
    pub(super) init: InitFn,
    commands: Sender<WindowCmd>,
    events: Receiver<WindowEvent>,
}

impl ChannelWindow {
    pub fn new(commands: Sender<WindowCmd>, events: Receiver<WindowEvent>, init: InitFn) -> Self {
        Self {
            max_coord: (0.0, 0.0),
            init,
            commands,
            events,
        }
    }

    pub fn construct() -> (Self, Receiver<WindowCmd>, Sender<WindowEvent>) {
        let (cmd_tx, cmd_rx) = mpsc::channel();
        let (evt_tx, evt_rx) = mpsc::channel();
        (Self::new(cmd_tx, evt_rx, Box::new(|| ())), cmd_rx, evt_tx)
    }
}

impl Window for ChannelWindow {
    fn init(&mut self, max_x: f64, max_y: f64) {
        self.max_coord = (max_x, max_y);
        let init = std::mem::replace(&mut self.init, Box::new(|| ()));
        init()
    }

    fn max_coords(&mut self) -> &mut (f64, f64) {
        &mut self.max_coord
    }

    fn draw(&mut self, from: TCoord, to: TCoord, col: TColor) {
        let from = (from.0 / self.max_coord.0, from.1 / self.max_coord.1);
        let to = (to.0 / self.max_coord.0, to.1 / self.max_coord.1);
        self.commands.send(WindowCmd::Draw(from, to, col)).unwrap();
    }

    fn clear(&mut self) {
        self.commands.send(WindowCmd::Clear).unwrap();
    }

    fn print(&mut self, msg: &str) {
        self.commands.send(WindowCmd::Print(msg.to_string())).unwrap();
    }

    fn events(&mut self) -> Vec<WindowEvent> {
        self.events.try_iter().map(|mut evt| {
            if let WindowEvent::MouseClicked(pos, _) = &mut evt {
                pos.0 *= self.max_coord.0;
                pos.1 *= self.max_coord.1;
            }
            evt
        }).collect()
    }
}