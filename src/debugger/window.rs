use sdl2::{event::Event, pixels::Color, rect::Point, render::Canvas, EventPump};

use super::{TColor, TCoord};

pub trait Window {
    fn get_max_x(&self) -> f64;
    fn get_max_y(&self) -> f64;
    fn set_max_x(&mut self, max_x: f64);
    fn set_max_y(&mut self, max_y: f64);

    fn draw(&mut self, from: TCoord, to: TCoord, col: TColor);
    fn clear(&mut self);

    fn print(&mut self, msg: &str);

    fn events(&mut self) -> Vec<WindowEvent>;
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WindowEvent {
    WindowExited,
    KeyPressed(char),
    MouseClicked(TCoord, bool),
}

pub struct SdlWindow {
    canvas: Canvas<sdl2::video::Window>,
    event_pump: EventPump,
    max_coord: (f64, f64),
}

const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;

impl SdlWindow {
    pub fn new(title: &str) -> Self {
        let sdl_context = sdl2::init().unwrap();
        let video_sub = sdl_context.video().unwrap();
        let window = video_sub
            .window(title, WIDTH, HEIGHT)
            .position_centered()
            .build()
            .unwrap();
        let canvas = window.into_canvas().build().unwrap();
        let event_pump = sdl_context.event_pump().unwrap();
        let mut this = Self {
            canvas,
            event_pump,
            max_coord: (20.0, 15.0),
        };
        this.clear();
        this
    }

    fn map_coords(&self, point: TCoord) -> Point {
        Point::new(
            (WIDTH as f64 / 2.0 * (1.0 + point.0 / self.max_coord.0)) as i32,
            (HEIGHT as f64 / 2.0 * (1.0 - point.1 / self.max_coord.1)) as i32,
        )
    }
}

impl Window for SdlWindow {
    fn get_max_x(&self) -> f64 {
        self.max_coord.0
    }

    fn get_max_y(&self) -> f64 {
        self.max_coord.1
    }

    fn set_max_x(&mut self, max_x: f64) {
        self.max_coord.0 = max_x;
    }

    fn set_max_y(&mut self, max_y: f64) {
        self.max_coord.1 = max_y;
    }

    fn draw(&mut self, from: TCoord, to: TCoord, col: TColor) {
        self.canvas.set_draw_color(Color::RGB(
            (col.0 * 2.55) as u8,
            (col.1 * 2.55) as u8,
            (col.2 * 2.55) as u8,
        ));
        let start = self.map_coords(from);
        let end = self.map_coords(to);
        self.canvas.draw_line(start, end).unwrap();
        self.canvas.present();
    }

    fn clear(&mut self) {
        self.canvas.set_draw_color(Color::BLACK);
        self.canvas.clear();
        self.canvas.present();
    }

    fn print(&mut self, msg: &str) {
        println!("Turtle says: {msg}");
    }

    fn events(&mut self) -> Vec<WindowEvent> {
        self.event_pump
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
