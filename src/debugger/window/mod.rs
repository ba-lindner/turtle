use super::{TColor, TCoord};

mod sdl;

pub use sdl::SdlWindow;

pub trait Window {
    fn init(&mut self, max_x: f64, max_y: f64) {
        *self.max_coords() = (max_x, max_y);
    }

    fn max_coords(&mut self) -> &mut (f64, f64);

    fn draw(&mut self, _: TCoord, _: TCoord, _: TColor) {}
    fn clear(&mut self) {}

    fn print(&mut self, _: &str) {}

    fn events(&mut self) -> Vec<WindowEvent> {
        Vec::new()
    }
}

impl Window for Box<dyn Window> {
    fn init(&mut self, max_x: f64, max_y: f64) {
        (**self).init(max_x, max_y);
    }

    fn max_coords(&mut self) -> &mut (f64, f64) {
        (**self).max_coords()
    }

    fn draw(&mut self, from: TCoord, to: TCoord, col: TColor) {
        (**self).draw(from, to, col);
    }

    fn clear(&mut self) {
        (**self).clear();
    }

    fn print(&mut self, msg: &str) {
        (**self).print(msg);
    }

    fn events(&mut self) -> Vec<WindowEvent> {
        (**self).events()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WindowEvent {
    WindowExited,
    KeyPressed(char),
    MouseClicked(TCoord, bool),
}

impl Window for (f64, f64) {
    fn max_coords(&mut self) -> &mut (f64, f64) {
        self
    }
}
