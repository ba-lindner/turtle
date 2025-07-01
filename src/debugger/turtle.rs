use std::{f64::consts::PI, fmt::Display};

use crate::{
    Disp, SymbolTable,
    pos::FilePos,
    tokens::{EventKind, PredefVar, Value, Variable, VariableKind},
};

use super::{GlobalCtx, TColor, TCoord, varlist::VarList, window::Window};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum FuncType {
    #[default]
    Main,
    Path(usize),
    Calc(usize),
    Event(EventKind),
}

impl FuncType {
    pub fn short(&self, symbols: &SymbolTable) -> String {
        match self {
            FuncType::Main => "main".to_string(),
            FuncType::Path(id) | FuncType::Calc(id) => symbols.get_index(*id).unwrap().0.clone(),
            FuncType::Event(kind) => kind.to_string(),
        }
    }
}

impl Display for FuncType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncType::Main => write!(f, "main block"),
            FuncType::Path(id) => write!(f, "path #{id}"),
            FuncType::Calc(id) => write!(f, "calculation #{id}"),
            FuncType::Event(kind) => write!(f, "{kind} event handler"),
        }
    }
}

impl Disp for FuncType {
    fn disp(&self, f: &mut std::fmt::Formatter<'_>, symbols: &SymbolTable) -> std::fmt::Result {
        let name = |id: &usize| symbols.get_index(*id).unwrap().0;
        match self {
            FuncType::Main => write!(f, "main block"),
            FuncType::Path(id) => write!(f, "path {}", name(id)),
            FuncType::Calc(id) => write!(f, "calculation {}", name(id)),
            FuncType::Event(kind) => write!(f, "{kind} event handler"),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct StackFrame {
    pub func: FuncType,
    pub vars: VarList,
    pub curr_pos: FilePos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Turtle {
    pos: TCoord,
    pub dir: f64,
    pub stack: Vec<StackFrame>,
    marks: Vec<(TCoord, f64)>,
    col: TColor,
}

impl Turtle {
    pub fn new() -> Self {
        Self {
            pos: (0.0, 0.0),
            dir: 0.0,
            stack: vec![StackFrame::default()],
            marks: Vec::new(),
            col: super::START_COLOR,
        }
    }

    pub fn split(&self) -> Self {
        Self {
            pos: self.pos,
            dir: self.dir,
            stack: vec![StackFrame::default()],
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
            ctx.window.borrow_mut().draw(self.pos, to, self.col);
        }
        self.pos = to;
    }

    pub fn new_mark(&mut self) {
        self.marks.push((self.pos, self.dir));
    }

    pub fn move_mark(&mut self, ctx: &GlobalCtx<impl Window>, draw: bool) {
        let mark = self.marks.pop().unwrap_or_default();
        self.dir = mark.1;
        self.move_to(ctx, mark.0, draw);
    }

    pub fn get_var(&mut self, ctx: &GlobalCtx<impl Window>, var: &Variable) -> Value {
        match &var.kind {
            VariableKind::Local(id, ty) => self
                .stack
                .last_mut()
                .unwrap()
                .vars
                .get_var(*id, *ty)
                .clone(),
            VariableKind::Global(id, ty) => ctx.vars.borrow_mut().get_var(*id, *ty).clone(),
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
            VariableKind::Local(id, _) => self.stack.last_mut().unwrap().vars.set_var(*id, val),
            VariableKind::Global(id, _) => ctx.vars.borrow_mut().set_var(*id, val),
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

    pub fn set_dir(&mut self, dir: f64) {
        self.dir = dir.rem_euclid(360.0);
    }

    pub fn set_col(&mut self, r: f64, g: f64, b: f64) {
        self.col = (r, g, b);
    }
}
