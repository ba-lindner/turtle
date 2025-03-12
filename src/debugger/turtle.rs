use std::{array, f64::consts::PI, time::Duration};

use sdl2::{pixels::Color, rect::Point};

use crate::{
    pos::FilePos, tokens::{PredefVar, Value, Variable, VariableKind}, SymbolTable
};

use super::{varlist::VarList, window::Window};

pub struct Turtle {
    pos: (f64, f64),
    pub dir: f64,
    glob: VarList,
    pub stack: Vec<(Option<usize>, VarList)>,
    marks: Vec<((f64, f64), f64)>,
    col: (f64, f64, f64),
    prog_args: [String; 9],
    max_coord: (f64, f64),
    delay: f64,
    pub window: Window,
}

impl Turtle {
    pub fn new(title: &str, args: &[String]) -> Self {
        Self {
            pos: (0.0, 0.0),
            dir: 0.0,
            glob: VarList::new(),
            stack: vec![(None, VarList::new())],
            marks: Vec::new(),
            col: (100.0, 100.0, 0.0),
            prog_args: array::from_fn(|idx| {
                args.get(idx).cloned().unwrap_or_default()
            }),
            max_coord: (20.0, 15.0),
            delay: 1.0,
            window: Window::new(title),
        }
    }

    pub fn update_col(&mut self) {
        self.window.set_col(Color::RGB(
            (self.col.0 * 2.55) as u8,
            (self.col.1 * 2.55) as u8,
            (self.col.2 * 2.55) as u8,
        ));
    }

    fn map_coord(&self, coord: (f64, f64)) -> Point {
        Point::new(
            (super::WIDTH as f64 / 2.0 * (1.0 + coord.0 / self.max_coord.0)) as i32,
            (super::HEIGHT as f64 / 2.0 * (1.0 + coord.1 / self.max_coord.1)) as i32,
        )
    }

    fn draw(&mut self, next_pos: (f64, f64)) {
        let start = self.map_coord(self.pos);
        let end = self.map_coord(next_pos);
        self.window.draw_line(start, end);
        // TODO: increase responsiveness with high delays
        if self.delay > 0.0 {
            std::thread::sleep(Duration::from_millis(self.delay as u64));
        }
        if self.window.exit_pressed() {
            std::process::exit(1);
        }
    }

    pub fn move_dist(&mut self, dist: f64, back: bool, draw: bool) {
        let dir = if back { self.dir + 180.0 } else { self.dir };
        let next_pos = (
            self.pos.0 + (dir * PI / 180.0).cos() * dist,
            self.pos.1 - (dir * PI / 180.0).sin() * dist,
        );
        self.move_to(next_pos, draw);
    }

    pub fn move_home(&mut self, draw: bool) {
        self.dir = 0.0;
        self.move_to((0.0, 0.0), draw);
    }

    fn move_to(&mut self, to: (f64, f64), draw: bool) {
        if draw {
            self.draw(to);
        }
        self.pos = to;
    }

    pub fn new_mark(&mut self) {
        self.marks.push((self.pos, self.dir));
    }

    pub fn move_mark(&mut self, draw: bool) {
        let mark = self.marks.pop().expect("no mark");
        self.dir = mark.1;
        self.move_to(mark.0, draw);
    }

    pub fn get_var(&mut self, var: &Variable) -> Value {
        match &var.kind {
            VariableKind::Local(id, ty) => self.stack.last_mut().unwrap().1.get_var(*id, *ty).clone(),
            VariableKind::Global(id, ty) => self.glob.get_var(*id, *ty).clone(),
            VariableKind::GlobalPreDef(pdv) => Value::Number(match pdv {
                PredefVar::Dir => self.dir,
                PredefVar::Dist => (self.pos.0 * self.pos.0 + self.pos.1 * self.pos.1).sqrt(),
                PredefVar::X => self.pos.0,
                PredefVar::Y => self.pos.1,
                PredefVar::Arg(idx) => return Value::String(self.prog_args[idx - 1].clone()),
                PredefVar::Pi => PI,
                PredefVar::MaxX => self.max_coord.0,
                PredefVar::MaxY => self.max_coord.1,
                PredefVar::Delay => self.delay,
                PredefVar::Red => self.col.0,
                PredefVar::Green => self.col.1,
                PredefVar::Blue => self.col.2,
            }),
        }
    }

    pub fn set_var(&mut self, var: &Variable, val: Value) {
        match &var.kind {
            VariableKind::Local(id, _) => self.stack.last_mut().unwrap().1.set_var(*id, val),
            VariableKind::Global(id, _) => self.glob.set_var(*id, val),
            VariableKind::GlobalPreDef(pdv) => match pdv {
                PredefVar::MaxX => self.max_coord.0 = val.num(),
                PredefVar::MaxY => self.max_coord.1 = val.num(),
                PredefVar::Delay => self.delay = val.num(),
                PredefVar::Red => {
                    self.col.0 = val.num();
                    self.update_col();
                }
                PredefVar::Green => {
                    self.col.1 = val.num();
                    self.update_col();
                }
                PredefVar::Blue => {
                    self.col.2 = val.num();
                    self.update_col();
                }
                _ => unreachable!("attempted to write to read-only predefined variable"),
            },
        }
    }

    pub fn dump_vars(&mut self, symbols: &SymbolTable) {
        println!("=== variable dump ===");
        for pdv in PredefVar::get_all() {
            let val = self.get_var(&VariableKind::GlobalPreDef(pdv).at(FilePos::default()));
            println!("@{} = {val}", pdv.get_str());
        }
        println!();
        self.glob.dump(symbols, true);
        println!();
        self.stack.last().unwrap().1.dump(symbols, false);
        println!("=== end of dump ===");
    }

    pub fn set_dir(&mut self, dir: f64) {
        self.dir = dir.rem_euclid(360.0);
    }

    pub fn set_col(&mut self, r: f64, g: f64, b: f64) {
        self.col = (r, g, b);
        self.update_col();
    }
}
