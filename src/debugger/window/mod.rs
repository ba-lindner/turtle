use super::{TColor, TCoord};

mod buffered;
mod channel;
mod sdl;

pub use buffered::BufferedWindow;
pub use channel::ChannelWindow;
pub use sdl::SdlWindow;
pub type VoidWindow = (f64, f64);

pub trait Window {
    fn init(&mut self);
    fn init_with(&mut self, max_x: f64, max_y: f64) {
        self.set_max_x(max_x);
        self.set_max_y(max_y);
        self.init();
    }

    fn get_max_coords(&self) -> TCoord;
    fn set_max_x(&mut self, max_x: f64);
    fn set_max_y(&mut self, max_y: f64);

    fn draw(&mut self, from: TCoord, to: TCoord, col: TColor);
    fn clear(&mut self);

    fn print(&mut self, msg: &str);

    fn events(&mut self) -> Vec<WindowEvent>;

    fn buffered(self) -> BufferedWindow<impl Window> where Self: Sized {
        BufferedWindow::new(self)
    }
}

impl Window for Box<dyn Window> {
    fn init(&mut self) {
        (**self).init();
    }

    fn get_max_coords(&self) -> TCoord {
        (**self).get_max_coords()
    }

    fn set_max_x(&mut self, max_x: f64) {
        (**self).set_max_x(max_x);
    }

    fn set_max_y(&mut self, max_y: f64) {
        (**self).set_max_y(max_y);
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

#[derive(Debug, PartialEq)]
pub enum WindowCmd {
    Draw(TCoord, TCoord, TColor),
    Clear,
    Print(String)
}

impl Window for VoidWindow {
    fn init(&mut self) {}

    fn get_max_coords(&self) -> TCoord {
        *self
    }

    fn set_max_x(&mut self, max_x: f64) {
        self.0 = max_x;
    }

    fn set_max_y(&mut self, max_y: f64) {
        self.1 = max_y
    }

    fn draw(&mut self, _: TCoord, _: TCoord, _: TColor) {}
    fn clear(&mut self) {}

    fn print(&mut self, _: &str) {}

    fn events(&mut self) -> Vec<WindowEvent> {
        Vec::new()
    }
}
