use super::{TColor, TCoord};

mod buffered;
mod channel;
#[cfg(feature = "sdl")]
mod sdl;

pub use buffered::BufferedWindow;
pub use channel::ChannelWindow;
#[cfg(feature = "sdl")]
pub use sdl::SdlWindow;

/// The `/dev/null`-window.
pub type VoidWindow = (f64, f64);

/// A window displaying the turtle drawings.
pub trait Window {
    /// Initialize the window.
    fn init(&mut self);

    /// Initialize the window with max coordinates.
    ///
    /// This should not be implemented directly.
    /// Instead, put your code in [`init`](Window::init()).
    fn init_with(&mut self, max_x: f64, max_y: f64) {
        self.set_max_x(max_x);
        self.set_max_y(max_y);
        self.init();
    }

    /// Get `@max_x` and `@max_y`.
    fn get_max_coords(&self) -> TCoord;

    /// Set `@max_x`.
    fn set_max_x(&mut self, max_x: f64);

    /// Set `@max_y`.
    fn set_max_y(&mut self, max_y: f64);

    /// Draw a line.
    ///
    /// This is called for each `walk ...` statement.
    fn draw(&mut self, from: TCoord, to: TCoord, col: TColor);

    /// Clear the canvas.
    ///
    /// This is called for each `clear` statement.
    fn clear(&mut self);

    /// Print some message.
    ///
    /// This is called for each `print` statement.
    fn print(&mut self, msg: &str);

    /// Query for User input.
    ///
    /// This should return a [`Vec`] with all [`WindowEvent`]s
    /// that occurred since the last call to this function.
    fn events(&mut self) -> Vec<WindowEvent>;

    /// Create a buffered version of `self`.
    fn buffered(self) -> BufferedWindow<impl Window>
    where
        Self: Sized,
    {
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

/// Events that can occur during execution.
///
/// These events represent user input during
/// program execution and are limited to the
/// `events` feature.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WindowEvent {
    /// The window was closed.
    ///
    /// This is not really an event for
    /// user input, but still handled as such.
    WindowExited,
    /// A key was pressed.
    ///
    /// The [`char`] should represent the pressed key
    /// as closely as possible.
    KeyPressed(char),
    /// A mouse button was clicked.
    ///
    /// The [`bool`] should be set to true if
    /// the left mouse button was used, and
    /// false otherwise.
    MouseClicked(TCoord, bool),
}

#[derive(Debug, PartialEq)]
pub enum WindowCmd {
    Draw(TCoord, TCoord, TColor),
    Clear,
    Print(String),
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

    fn print(&mut self, msg: &str) {
        println!("{msg}")
    }

    fn events(&mut self) -> Vec<WindowEvent> {
        Vec::new()
    }
}
