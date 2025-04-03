use std::f64::consts::PI;

use crate::{
    pos::FilePos,
    tokens::{EventKind, PredefVar, Value, Variable, VariableKind},
    SymbolTable,
};

use super::{varlist::VarList, window::Window, GlobalCtx, TColor, TCoord};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FuncType {
    Main,
    Path(usize),
    Calc(usize),
    Event(EventKind),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Turtle {
    pos: TCoord,
    pub dir: f64,
    pub stack: Vec<(FuncType, VarList)>,
    marks: Vec<(TCoord, f64)>,
    col: TColor,
}

impl Turtle {
    pub fn new() -> Self {
        Self {
            pos: (0.0, 0.0),
            dir: 0.0,
            stack: vec![(FuncType::Main, VarList::new())],
            marks: Vec::new(),
            col: super::START_COLOR,
        }
    }

    pub fn split(&self) -> Self {
        Self {
            pos: self.pos,
            dir: self.dir,
            stack: vec![(FuncType::Main, VarList::new())],
            marks: self.marks.clone(),
            col: self.col,
        }
    }

    pub fn move_dist(&mut self, ctx: &GlobalCtx<impl Window>, dist: f64, back: bool, draw: bool) {
        let dir = if back { self.dir + 180.0 } else { self.dir };
        let next_pos = (
            self.pos.0 + (dir * PI / 180.0).cos() * dist,
            self.pos.1 + (dir * PI / 180.0).sin() * dist,
        );
        self.move_to(ctx, next_pos, draw);
    }

    pub fn move_home(&mut self, ctx: &GlobalCtx<impl Window>, draw: bool) {
        self.dir = 0.0;
        self.move_to(ctx, (0.0, 0.0), draw);
    }

    fn move_to(&mut self, ctx: &GlobalCtx<impl Window>, to: (f64, f64), draw: bool) {
        if draw {
            ctx.window.lock().draw(self.pos, to, self.col);
        }
        self.pos = to;
    }

    pub fn new_mark(&mut self) {
        self.marks.push((self.pos, self.dir));
    }

    pub fn move_mark(&mut self, ctx: &GlobalCtx<impl Window>, draw: bool) {
        let mark = self.marks.pop().expect("no mark");
        self.dir = mark.1;
        self.move_to(ctx, mark.0, draw);
    }

    pub fn get_var(&mut self, ctx: &GlobalCtx<impl Window>, var: &Variable) -> Value {
        match &var.kind {
            VariableKind::Local(id, ty) => {
                self.stack.last_mut().unwrap().1.get_var(*id, *ty).clone()
            }
            VariableKind::Global(id, ty) => ctx.vars.lock().get_var(*id, *ty).clone(),
            VariableKind::GlobalPreDef(pdv) => Value::Number(match pdv {
                PredefVar::Dir => self.dir,
                PredefVar::Dist => (self.pos.0 * self.pos.0 + self.pos.1 * self.pos.1).sqrt(),
                PredefVar::X => self.pos.0,
                PredefVar::Y => self.pos.1,
                PredefVar::Pi => PI,
                PredefVar::Red => self.col.0,
                PredefVar::Green => self.col.1,
                PredefVar::Blue => self.col.2,
                _ => return ctx.get_var(*pdv),
            }),
        }
    }

    pub fn set_var(&mut self, ctx: &GlobalCtx<impl Window>, var: &Variable, val: Value) {
        match &var.kind {
            VariableKind::Local(id, _) => self.stack.last_mut().unwrap().1.set_var(*id, val),
            VariableKind::Global(id, _) => ctx.vars.lock().set_var(*id, val),
            VariableKind::GlobalPreDef(pdv) => match pdv {
                PredefVar::Red => {
                    self.col.0 = val.num();
                }
                PredefVar::Green => {
                    self.col.1 = val.num();
                }
                PredefVar::Blue => {
                    self.col.2 = val.num();
                }
                _ => ctx.set_var(*pdv, val),
            },
        }
    }

    pub fn dump_vars(&mut self, ctx: &GlobalCtx<impl Window>, symbols: &SymbolTable) {
        println!("=== variable dump ===");
        for pdv in PredefVar::get_all() {
            let val = self.get_var(ctx, &VariableKind::GlobalPreDef(pdv).at(FilePos::default()));
            println!("@{} = {val}", pdv.get_str());
        }
        println!();
        ctx.vars.lock().dump(symbols, true);
        println!();
        self.stack.last().unwrap().1.dump(symbols, false);
        println!("=== end of dump ===");
    }

    pub fn set_dir(&mut self, dir: f64) {
        self.dir = dir.rem_euclid(360.0);
    }

    pub fn set_col(&mut self, r: f64, g: f64, b: f64) {
        self.col = (r, g, b);
    }
}
