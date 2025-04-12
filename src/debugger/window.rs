use std::{sync::mpsc::{self, Receiver, Sender}, thread, time::Duration};

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

enum WindowCmd {
    Draw(Color, Point, Point),
    Clear,
}

pub struct SdlWindow {
    title: String,
    max_coord: (f64, f64),
    cmds: Sender<WindowCmd>,
    events: Receiver<WindowEvent>,
}

struct SdlImpl {
    canvas: Canvas<sdl2::video::Window>,
    event_pump: EventPump,
    cmds: Receiver<WindowCmd>,
    events: Sender<WindowEvent>,
    wait_exit: Option<usize>
}

const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;

impl SdlWindow {
    pub fn new(title: &str) -> Self {
        Self {
            title: title.to_string(),
            max_coord: (0.0, 0.0),
            cmds: mpsc::channel().0,
            events: mpsc::channel().1,
        }
    }

    fn map_coords(&self, point: TCoord) -> Point {
        Point::new(
            (WIDTH as f64 / 2.0 * (1.0 + point.0 / self.max_coord.0)) as i32,
            (HEIGHT as f64 / 2.0 * (1.0 - point.1 / self.max_coord.1)) as i32,
        )
    }
}

impl SdlImpl {
    fn spawn(title: String, cmds: Receiver<WindowCmd>, events: Sender<WindowEvent>) {
        thread::spawn(move || {
            let sdl_context = sdl2::init().unwrap();
            let video_sub = sdl_context.video().unwrap();
            let window = video_sub
                .window(&title, WIDTH, HEIGHT)
                .position_centered()
                .build()
                .unwrap();
            let mut canvas = window.into_canvas().build().unwrap();
            canvas.clear();
            let event_pump = sdl_context.event_pump().unwrap();
            let this = SdlImpl {
                canvas,
                event_pump,
                cmds,
                events,
                wait_exit: None,
            };
            this.run();
        });
    }

    fn run(mut self) {
        loop {
            for cmd in self.cmds.try_iter() {
                match cmd {
                    WindowCmd::Clear => {
                        self.canvas.set_draw_color(Color::BLACK);
                        self.canvas.clear();
                    }
                    WindowCmd::Draw(color, start, end) => {
                        self.canvas.set_draw_color(color);
                        self.canvas.draw_line(start, end).unwrap();
                    }
                }
            }
            self.canvas.present();
            thread::sleep(Duration::from_millis(16));
            if let Some(rem) = &mut self.wait_exit {
                *rem -= 1;
                if *rem == 0 {
                    std::process::exit(0);
                }
            }
            self.collect_events();
        }
    }

    fn collect_events(&mut self) {
        let events = self.event_pump
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
                Event::Quit { .. } => {
                    self.wait_exit.get_or_insert(5);
                    Some(WindowEvent::WindowExited)
                }
                Event::MouseButtonDown {
                    x, y, mouse_btn, ..
                } => {
                    Some(WindowEvent::MouseClicked(
                        (x as f64, y as f64),
                        mouse_btn == sdl2::mouse::MouseButton::Left,
                    ))
                }
                _ => None,
            });
        for evt in events {
            self.events.send(evt).unwrap();
        }
    }
}

impl Window for SdlWindow {
    fn init(&mut self, max_x: f64, max_y: f64) {
        self.max_coord = (max_x, max_y);
        let (cmd_tx, cmd_rx) = mpsc::channel();
        let (evt_tx, evt_rx) = mpsc::channel();
        let title = std::mem::take(&mut self.title);
        SdlImpl::spawn(title, cmd_rx, evt_tx);
        self.cmds = cmd_tx;
        self.events = evt_rx;
    }

    fn max_coords(&mut self) -> &mut (f64, f64) {
        &mut self.max_coord
    }

    fn draw(&mut self, from: TCoord, to: TCoord, col: TColor) {
        let col = Color::RGB(
            (col.0 * 2.55) as u8,
            (col.1 * 2.55) as u8,
            (col.2 * 2.55) as u8,
        );
        let start = self.map_coords(from);
        let end = self.map_coords(to);
        self.cmds.send(WindowCmd::Draw(col, start, end)).unwrap();
        // canvas.set_draw_color(Color::RGB(
        //     (col.0 * 2.55) as u8,
        //     (col.1 * 2.55) as u8,
        //     (col.2 * 2.55) as u8,
        // ));
        // canvas.draw_line(start, end).unwrap();
        // canvas.present();
    }

    fn clear(&mut self) {
        self.cmds.send(WindowCmd::Clear).unwrap();
        // canvas.set_draw_color(Color::BLACK);
        // canvas.clear();
        // canvas.present();
    }

    fn print(&mut self, msg: &str) {
        println!("Turtle says: {msg}");
    }

    fn events(&mut self) -> Vec<WindowEvent> {
        self.events.try_iter().map(|mut e| {
            if let WindowEvent::MouseClicked((x, y), _) = &mut e {
                *x = (*x as f64 * 2.0 / WIDTH as f64 - 1.0) * self.max_coord.0;
                *y = (1.0 - *y as f64 * 2.0 / HEIGHT as f64) * self.max_coord.1;
            }
            e
        }).collect()
    }
}

impl Window for (f64, f64) {
    fn max_coords(&mut self) -> &mut (f64, f64) {
        self
    }
}
