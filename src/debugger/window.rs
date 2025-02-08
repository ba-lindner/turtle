use sdl2::{
    event::Event, keyboard::Keycode, pixels::Color, rect::Point, render::Canvas, EventPump,
};

pub struct Window {
    canvas: Canvas<sdl2::video::Window>,
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
        let mut res = Self { canvas, events };
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
        let mut res = Vec::new();
        while let Some(evt) = self.events.poll_event() {
            match evt {
                Event::KeyDown {
                    keycode: Some(kc), ..
                } => {
                    res.push(kc);
                }
                Event::Quit { .. } => {
                    return Err(ExitPressed);
                }
                _ => {}
            }
        }
        Ok(res)
    }

    pub fn exit_pressed(&mut self) -> bool {
        while let Some(evt) = self.events.poll_event() {
            if let Event::Quit { timestamp: _ } = evt {
                return true;
            }
        }
        false
    }
}
