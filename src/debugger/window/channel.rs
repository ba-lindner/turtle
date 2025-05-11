use std::sync::mpsc::{self, Receiver, Sender};

use crate::debugger::{TColor, TCoord};

use super::{Window, WindowCmd, WindowEvent};

type InitFn = Box<dyn FnOnce() + Send>;

/// A generic channel-based window.
/// 
/// This window doesn't do much on its own,
/// but simply receives event from a channel
/// and sends actions to another one. Coordinates
/// are normalized to be in the range (-1 .. 1).
/// 
/// While this in itself is not too useful, it allows
/// for some nice things:
/// * Testing is simplified as events can be read from
///   a vec, outputs written to a vec and afterwards
///   be compared with the expected outcome.
/// * [`SdlWindow`](super::SdlWindow) uses this
///   for significant performance improvements
/// * The turtle web server also is built on this
/// 
/// The key benefit of using a [`ChannelWindow`] lies
/// in the possibility to handle window events somewhere
/// else instead of deep within the debugger.
pub struct ChannelWindow {
    max_coord: (f64, f64),
    /// Init function
    /// 
    /// This exists mainly for [`SdlWindow`](super::SdlWindow).
    pub(super) init: InitFn,
    commands: Sender<WindowCmd>,
    events: Receiver<WindowEvent>,
}

impl ChannelWindow {
    /// Create a new [`ChannelWindow`] instance from two given channels.
    pub fn new(commands: Sender<WindowCmd>, events: Receiver<WindowEvent>, init: InitFn) -> Self {
        Self {
            max_coord: (0.0, 0.0),
            init,
            commands,
            events,
        }
    }

    /// Create a new [`ChannelWindow`] instance together with two channels.
    pub fn construct() -> (Self, Receiver<WindowCmd>, Sender<WindowEvent>) {
        let (cmd_tx, cmd_rx) = mpsc::channel();
        let (evt_tx, evt_rx) = mpsc::channel();
        (Self::new(cmd_tx, evt_rx, Box::new(|| ())), cmd_rx, evt_tx)
    }
}

impl Window for ChannelWindow {
    fn init(&mut self) {
        // self.init is `impl FnOnce()`, so executing it moves it
        // thus we need to replace it with empty closure
        // it probably shouldn't be executed twice anyway
        std::mem::replace(&mut self.init, Box::new(|| ()))();
    }

    fn get_max_coords(&self) -> TCoord {
        self.max_coord
    }

    fn set_max_x(&mut self, max_x: f64) {
        self.max_coord.0 = max_x;
    }

    fn set_max_y(&mut self, max_y: f64) {
        self.max_coord.1 = max_y;
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
        self.commands
            .send(WindowCmd::Print(msg.to_string()))
            .unwrap();
    }

    fn events(&mut self) -> Vec<WindowEvent> {
        self.events
            .try_iter()
            .map(|mut evt| {
                if let WindowEvent::MouseClicked(pos, _) = &mut evt {
                    pos.0 *= self.max_coord.0;
                    pos.1 *= self.max_coord.1;
                }
                evt
            })
            .collect()
    }
}
