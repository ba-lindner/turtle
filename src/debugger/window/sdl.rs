use std::{
    sync::mpsc::{Receiver, Sender},
    thread,
    time::Duration,
};

use sdl2::{event::Event, pixels::Color, rect::Point, render::Canvas, EventPump};

use super::{ChannelWindow, WindowCmd, WindowEvent};

pub struct SdlWindow {
    canvas: Canvas<sdl2::video::Window>,
    event_pump: EventPump,
    cmds: Receiver<WindowCmd>,
    events: Sender<WindowEvent>,
    wait_exit: Option<usize>,
}

const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;

impl SdlWindow {
    pub fn create(title: String) -> ChannelWindow {
        let (mut window, cmds, events) = ChannelWindow::construct();
        window.init = Box::new(|| SdlWindow::spawn(title, cmds, events));
        window
    }

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
            let this = SdlWindow {
                canvas,
                event_pump,
                cmds,
                events,
                wait_exit: None,
            };
            this.run();
        });
    }

    fn map_coords(&self, coord: (f64, f64)) -> Point {
        Point::new(
            (WIDTH as f64 / 2.0 * (1.0 + coord.0)) as i32,
            (HEIGHT as f64 / 2.0 * (1.0 - coord.1)) as i32,
        )
    }

    fn run(mut self) {
        loop {
            for cmd in self.cmds.try_iter() {
                match cmd {
                    WindowCmd::Clear => {
                        self.canvas.set_draw_color(Color::BLACK);
                        self.canvas.clear();
                    }
                    WindowCmd::Draw(from, to, col) => {
                        let color = Color::RGB(
                            (col.0 * 2.55) as u8,
                            (col.1 * 2.55) as u8,
                            (col.2 * 2.55) as u8,
                        );
                        let start = self.map_coords(from);
                        let end = self.map_coords(to);
                        self.canvas.set_draw_color(color);
                        self.canvas.draw_line(start, end).unwrap();
                    }
                    WindowCmd::Print(msg) => println!("{msg}"),
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
        let events = self.event_pump.poll_iter().flat_map(|evt| match evt {
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
            } => Some(WindowEvent::MouseClicked(
                (2.0 * x as f64 / WIDTH as f64 - 1.0, 1.0 - y as f64 * 2.0 / HEIGHT as f64),
                mouse_btn == sdl2::mouse::MouseButton::Left,
            )),
            _ => None,
        });
        for evt in events {
            self.events.send(evt).unwrap();
        }
    }
}
