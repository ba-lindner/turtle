use std::sync::Mutex;

use sdl2::{event::Event, keyboard::Keycode, pixels::Color, rect::Point, render::Canvas, EventPump};

pub struct Window {
    canvas: Mutex<Canvas<sdl2::video::Window>>,
    events: Mutex<EventPump>,
}

impl Window {
    pub fn new(title: &str) -> Self {
        let sdl_context = sdl2::init().unwrap();
        let video_sub = sdl_context.video().unwrap();
        let window = video_sub
            .window(title, super::WIDTH, super::HEIGHT)
            .position_centered()
            .build()
            .unwrap();
        let mut canvas = window.into_canvas().build().unwrap();
        let events = sdl_context.event_pump().unwrap();
        canvas.set_draw_color(super::START_COLOR);
        let res = Self {
            canvas: Mutex::new(canvas),
            events: Mutex::new(events),
        };
        res.clear();
        res
    }

    pub fn clear(&self) {
        let mut canvas = self.canvas.lock().unwrap();
        let col = canvas.draw_color();
        canvas.set_draw_color(Color::BLACK);
        canvas.clear();
        canvas.present();
        canvas.set_draw_color(col);
    }

    pub fn set_col(&self, col: Color) {
        self.canvas.lock().unwrap().set_draw_color(col);
    }

    pub fn draw_line(&self, start: Point, end: Point) {
        let mut canvas = self.canvas.lock().unwrap();
        canvas.draw_line(start, end).unwrap();
        canvas.present();
    }

    pub fn exit_pressed(&self) -> bool {
        let mut events = self.events.lock().unwrap();
        while let Some(evt) = events.poll_event() {
            if let Event::Quit { timestamp: _ } = evt {
                return true;
            }
        }
        false
    }

    pub fn wait_space_pressed(&self) -> bool {
        let mut events = self.events.lock().unwrap();
        loop {
            while let Some(evt) = events.poll_event() {
                match evt {
                    Event::KeyDown { keycode: Some(Keycode::SPACE), .. } => {
                        return true;
                    }
                    Event::Quit { .. } => {
                        return false;
                    }
                    _ => {}
                }
            }
        }
    }
}
