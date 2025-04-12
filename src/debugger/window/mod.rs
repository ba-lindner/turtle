use super::{TColor, TCoord};

mod sdl;

pub use sdl::SdlWindow;

pub trait Window {
    fn init(&mut self, max_x: f64, max_y: f64) {
        self.set_max_x(max_x);
        self.set_max_y(max_y);
    }

    fn max_coords(&mut self) -> &mut (f64, f64);

    fn get_max_x(&mut self) -> f64 {
        self.max_coords().0
    }

    fn get_max_y(&mut self) -> f64 {
        self.max_coords().1
    }

    fn set_max_x(&mut self, max_x: f64) {
        self.max_coords().0 = max_x;
    }

    fn set_max_y(&mut self, max_y: f64) {
        self.max_coords().1 = max_y;
    }

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
