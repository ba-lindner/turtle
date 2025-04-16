use crate::debugger::{TColor, TCoord};

use super::Window;

pub struct BufferedWindow<W> {
    inner: W,
    buffer: Vec<(TCoord, TCoord, TColor)>
}

impl<W: Window> BufferedWindow<W> {
    pub fn new(inner: W) -> Self {
        Self {
            inner,
            buffer: Vec::new(),
        }
    }

    pub fn repaint(&mut self) {
        self.inner.clear();
        for &(from, to, col) in &self.buffer {
            self.inner.draw(from, to, col);
        }
    }
}

impl<W: Window> Window for BufferedWindow<W> {
    fn init(&mut self) {
        self.inner.init();
    }

    fn get_max_coords(&self) -> TCoord {
        self.inner.get_max_coords()
    }

    fn set_max_x(&mut self, max_x: f64) {
        self.inner.set_max_x(max_x);
        self.repaint();
    }

    fn set_max_y(&mut self, max_y: f64) {
        self.inner.set_max_y(max_y);
        self.repaint();
    }

    fn draw(&mut self, from: TCoord, to: TCoord, col: TColor) {
        self.buffer.push((from, to, col));
        self.inner.draw(from, to, col);
    }

    fn clear(&mut self) {
        self.buffer.clear();
        self.inner.clear();
    }

    fn print(&mut self, msg: &str) {
        self.inner.print(msg);
    }

    fn events(&mut self) -> Vec<super::WindowEvent> {
        self.inner.events()
    }

    fn buffered(self) -> BufferedWindow<impl Window> where Self: Sized {
        self
    }
}