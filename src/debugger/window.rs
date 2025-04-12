use sdl2::{event::Event, pixels::Color, rect::Point, render::Canvas, EventPump};

use super::{TColor, TCoord};

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

pub struct SdlWindow {
    state: SdlState,
    max_coord: (f64, f64),
}

enum SdlState {
    Uninit(String),
    Init {
        canvas: Canvas<sdl2::video::Window>,
        event_pump: EventPump,
    },
}

const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;

impl SdlWindow {
    pub fn new(title: &str) -> Self {
        Self {
            state: SdlState::Uninit(title.to_string()),
            max_coord: (0.0, 0.0),
        }
    }

    fn map_coords(&self, point: TCoord) -> Point {
        Point::new(
            (WIDTH as f64 / 2.0 * (1.0 + point.0 / self.max_coord.0)) as i32,
            (HEIGHT as f64 / 2.0 * (1.0 - point.1 / self.max_coord.1)) as i32,
        )
    }
}

impl Window for SdlWindow {
    fn init(&mut self, max_x: f64, max_y: f64) {
        self.max_coord = (max_x, max_y);
        let SdlState::Uninit(title) = &self.state else {
            panic!("already initialized");
        };
        let sdl_context = sdl2::init().unwrap();
        let video_sub = sdl_context.video().unwrap();
        let window = video_sub
            .window(title, WIDTH, HEIGHT)
            .position_centered()
            .build()
            .unwrap();
        let canvas = window.into_canvas().build().unwrap();
        let event_pump = sdl_context.event_pump().unwrap();
        self.state = SdlState::Init { canvas, event_pump };
        self.clear();
    }

    fn max_coords(&mut self) -> &mut (f64, f64) {
        &mut self.max_coord
    }

    fn draw(&mut self, from: TCoord, to: TCoord, col: TColor) {
        let start = self.map_coords(from);
        let end = self.map_coords(to);
        let SdlState::Init { canvas, .. } = &mut self.state else {
            panic!("uninitialized");
        };
        canvas.set_draw_color(Color::RGB(
            (col.0 * 2.55) as u8,
            (col.1 * 2.55) as u8,
            (col.2 * 2.55) as u8,
        ));
        canvas.draw_line(start, end).unwrap();
        canvas.present();
    }

    fn clear(&mut self) {
        let SdlState::Init { canvas, .. } = &mut self.state else {
            panic!("uninitialized");
        };
        canvas.set_draw_color(Color::BLACK);
        canvas.clear();
        canvas.present();
    }

    fn print(&mut self, msg: &str) {
        println!("Turtle says: {msg}");
    }

    fn events(&mut self) -> Vec<WindowEvent> {
        let SdlState::Init { event_pump, .. } = &mut self.state else {
            panic!("uninitialized");
        };
        event_pump
            .poll_iter()
            .flat_map(|evt| match evt {
                Event::KeyDown {
                    keycode: Some(kc), ..
                } => kc
                    .into_i32()
                    .try_into()
                    .ok()
                    .and_then(char::from_u32)
                    .map(WindowEvent::KeyPressed),
                Event::Quit { .. } => Some(WindowEvent::WindowExited),
                Event::MouseButtonDown {
                    x, y, mouse_btn, ..
                } => {
                    let x = (x as f64 * 2.0 / WIDTH as f64 - 1.0) * self.max_coord.0;
                    let y = (1.0 - y as f64 * 2.0 / HEIGHT as f64) * self.max_coord.1;
                    Some(WindowEvent::MouseClicked(
                        (x, y),
                        mouse_btn == sdl2::mouse::MouseButton::Left,
                    ))
                }
                _ => None,
            })
            .collect()
    }
}

impl Window for (f64, f64) {
    fn max_coords(&mut self) -> &mut (f64, f64) {
        self
    }
}
