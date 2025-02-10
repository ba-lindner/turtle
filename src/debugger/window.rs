use sdl2::{
    event::Event, keyboard::Keycode, pixels::Color, rect::Point, render::Canvas, EventPump,
};

pub struct Window {
    canvas: Canvas<sdl2::video::Window>,
    key_queue: Vec<Keycode>,
    events: EventPump,
}

pub struct ExitPressed;

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
        let mut res = Self {
            canvas,
            key_queue: Vec::new(),
            events,
        };
        res.clear();
        res
    }

    pub fn clear(&mut self) {
        let col = self.canvas.draw_color();
        self.canvas.set_draw_color(Color::BLACK);
        self.canvas.clear();
        self.canvas.present();
        self.canvas.set_draw_color(col);
    }

    pub fn set_col(&mut self, col: Color) {
        self.canvas.set_draw_color(col);
    }

    pub fn draw_line(&mut self, start: Point, end: Point) {
        self.canvas.draw_line(start, end).unwrap();
        self.canvas.present();
    }

    pub fn keys_pressed(&mut self) -> Result<Vec<Keycode>, ExitPressed> {
        if self.exit_pressed() {
            Err(ExitPressed)
        } else {
            Ok(std::mem::take(&mut self.key_queue))
        }
    }

    pub fn exit_pressed(&mut self) -> bool {
        for evt in self.events.poll_iter() {
            match evt {
                Event::KeyDown {
                    keycode: Some(kc), ..
                } => {
                    self.key_queue.push(kc);
                }
                Event::Quit { .. } => {
                    return true;
                }
                _ => {}
            }
        }
        false
    }
}
