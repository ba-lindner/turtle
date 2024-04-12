mod varlist;

use std::{f64::consts::PI, time::Duration};

use rand::random;
use sdl2::{pixels::Color, rect::Point, render::Canvas, video::Window, EventPump};

use crate::{Cond, Expr, PredefFunc, Statement, TProgram, Variable};

use self::varlist::VarList;

// Aufwand: bisher ~4h

const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;

pub struct Interpreter {
    prog: Option<TProgram>,
    pos: (f64, f64),
    dir: f64,
    glob: VarList,
    stack: Vec<VarList>,
    stopped: bool,
    marks: Vec<((f64, f64), f64)>,
    col: (f64, f64, f64),
    prog_args: [f64; 9],
    max_coord: (f64, f64),
    delay: f64,
    canvas: Canvas<Window>,
    events: EventPump,
}

impl Interpreter {
    pub fn new(prog: TProgram) -> Self {
        let sdl_context = sdl2::init().unwrap();
        let video_sub = sdl_context.video().unwrap();
        let window = video_sub
            .window("Turtle Interpreter", WIDTH, HEIGHT)
            .position_centered()
            .build()
            .unwrap();
        let canvas = window.into_canvas().build().unwrap();
        let events = sdl_context.event_pump().unwrap();
        let mut itp = Self {
            prog: Some(prog),
            pos: (0.0, 0.0),
            dir: 0.0,
            glob: VarList::new(),
            stack: Vec::new(),
            stopped: false,
            marks: Vec::new(),
            col: (100.0, 100.0, 100.0),
            prog_args: [0.0; 9],
            max_coord: (20.0, 15.0),
            delay: 1.0,
            canvas,
            events,
        };
        for (i, arg) in std::env::args().skip(1).enumerate() {
            if i >= 9 {
                break;
            }
            itp.prog_args[i] = arg.parse::<f64>().unwrap_or(0.0);
        }
        itp
    }

    pub fn interpret(&mut self) {
        if self.prog.is_none() {
            return;
        }
        self.canvas.set_draw_color(Color::BLACK);
        self.canvas.clear();
        self.canvas.present();
        self.canvas.set_draw_color(self.curr_col());
        let prog = self.prog.take().unwrap();
        self.stack.push(VarList::new());
        self.itp_stmts(&prog, &prog.main);
    }

    fn curr_col(&self) -> Color {
        Color::RGB(
            (self.col.0 * 2.55) as u8,
            (self.col.1 * 2.55) as u8,
            (self.col.2 * 2.55) as u8,
        )
    }

    fn itp_stmts(&mut self, prog: &TProgram, stmts: &Vec<Statement>) {
        for stmt in stmts {
            self.itp_stm(prog, stmt);
            if self.stopped {
                return;
            }
        }
    }

    fn itp_stm(&mut self, prog: &TProgram, stmt: &Statement) {
        if self.stopped {
            return;
        }
        match stmt {
            Statement::Walk(expr) => {
                let dist = self.eval_expr(prog, expr);
                let next_pos = Self::calc_next_pos(self.pos, self.dir, dist);
                self.draw(next_pos);
            }
            Statement::WalkBack(expr) => {
                let dist = self.eval_expr(prog, expr);
                let next_pos = Self::calc_next_pos(self.pos, self.dir + 180.0, dist);
                self.draw(next_pos);
            }
            Statement::Jump(expr) => {
                let dist = self.eval_expr(prog, expr);
                let new_pos = Self::calc_next_pos(self.pos, self.dir, dist);
                self.pos = new_pos;
            }
            Statement::JumpBack(expr) => {
                let dist = self.eval_expr(prog, expr);
                let new_pos = Self::calc_next_pos(self.pos, self.dir + 180.0, dist);
                self.pos = new_pos;
            }
            Statement::WalkHome => {
                self.draw((0.0, 0.0));
            }
            Statement::JumpHome => {
                self.pos = (0.0, 0.0);
            }
            Statement::TurnLeft(expr) => {
                let angle = self.eval_expr(prog, expr);
                self.dir = (self.dir + angle).rem_euclid(360.0);
            }
            Statement::TurnRight(expr) => {
                let angle = self.eval_expr(prog, expr);
                self.dir = (self.dir - angle).rem_euclid(360.0);
            }
            Statement::Direction(expr) => {
                self.dir = self.eval_expr(prog, expr).rem_euclid(360.0);
            }
            Statement::Color(ex1, ex2, ex3) => {
                self.col = (
                    self.eval_expr(prog, ex1),
                    self.eval_expr(prog, ex2),
                    self.eval_expr(prog, ex3),
                );
                self.canvas.set_draw_color(self.curr_col());
            }
            Statement::Clear => {
                self.canvas.set_draw_color(Color::BLACK);
                self.canvas.clear();
                self.canvas.present();
                self.canvas.set_draw_color(self.curr_col());
            }
            Statement::Stop => {
                self.stopped = true;
                println!("halt and catch fire");
                'run: loop {
                    for evt in self.events.poll_iter() {
                        if let sdl2::event::Event::Quit { timestamp: _ } = evt {
                            break 'run;
                        }
                    }
                }
            }
            Statement::Finish => {
                self.stopped = true;
            }
            Statement::PathCall(id, args) => {
                self.itp_path(prog, *id, args);
            }
            Statement::Store(expr, var) => {
                let val = self.eval_expr(prog, expr);
                self.set_var(var, val);
            }
            Statement::Add(expr, var) => {
                let val = self.get_var(var) + self.eval_expr(prog, expr);
                self.set_var(var, val);
            }
            Statement::Sub(expr, var) => {
                let val = self.get_var(var) - self.eval_expr(prog, expr);
                self.set_var(var, val);
            }
            Statement::Mul(expr, var) => {
                let val = self.get_var(var) * self.eval_expr(prog, expr);
                self.set_var(var, val);
            }
            Statement::Div(expr, var) => {
                let div = self.eval_expr(prog, expr);
                if div == 0.0 {
                    panic!("division by zero");
                }
                let val = self.get_var(var) / div;
                self.set_var(var, val);
            }
            Statement::Mark => {
                self.marks.push(((self.pos), self.dir));
            }
            Statement::WalkMark => {
                let mark = self.marks.pop().expect("no mark");
                self.dir = mark.1;
                self.draw(mark.0);
            }
            Statement::JumpMark => {
                let mark = self.marks.pop().expect("no mark");
                self.pos = mark.0;
                self.dir = mark.1;
            }
            Statement::IfBranch(cond, stmts) => {
                if self.eval_cond(prog, cond) {
                    self.itp_stmts(prog, stmts);
                }
            }
            Statement::IfElseBranch(cond, if_stmts, else_stmts) => {
                if self.eval_cond(prog, cond) {
                    self.itp_stmts(prog, if_stmts);
                } else {
                    self.itp_stmts(prog, else_stmts);
                }
            }
            Statement::DoLoop(expr, stmts) => {
                let count = self.eval_expr(prog, expr).floor();
                for _ in 0..count as isize {
                    self.itp_stmts(prog, stmts);
                    if self.stopped {
                        break;
                    }
                }
            }
            Statement::CounterLoop(var, init, up, end, step, stmts) => {
                let init = self.eval_expr(prog, init);
                let end = self.eval_expr(prog, end);
                let mut step = match step {
                    Some(expr) => self.eval_expr(prog, expr),
                    None => 1.0,
                };
                if !up {
                    step = -step;
                }
                self.set_var(var, init);
                loop {
                    if *up == (self.get_var(var) >= end) {
                        break;
                    }
                    self.itp_stmts(prog, stmts);
                    if self.stopped {
                        return;
                    }
                    let next_val = self.get_var(var) + step;
                    self.set_var(var, next_val);
                }
            }
            Statement::WhileLoop(cond, stmts) => {
                while self.eval_cond(prog, cond) {
                    self.itp_stmts(prog, stmts);
                    if self.stopped {
                        return;
                    }
                }
            }
            Statement::RepeatLoop(cond, stmts) => {
                self.itp_stmts(prog, stmts);
                if self.stopped {
                    return;
                }
                while self.eval_cond(prog, cond) {
                    self.itp_stmts(prog, stmts);
                    if self.stopped {
                        return;
                    }
                }
            }
        }
    }

    fn itp_path(&mut self, prog: &TProgram, id: usize, args: &[Expr]) {
        let args = self.eval_args(prog, args);
        let path = prog
            .paths
            .iter()
            .find(|p| p.name == id)
            .expect("missing path definition");
        assert_eq!(path.args.len(), args.len());
        let mut vars = VarList::new();
        for (i, arg) in args.iter().enumerate() {
            vars.set_var(path.args[i], *arg);
        }
        self.stack.push(vars);
        self.itp_stmts(prog, &path.body);
        self.stack.pop();
    }

    fn itp_calc(&mut self, prog: &TProgram, id: usize, args: &[Expr]) -> f64 {
        let calc = prog
            .calcs
            .iter()
            .find(|cd| cd.name == id)
            .expect("missing calc def");
        let args = self.eval_args(prog, args);
        assert_eq!(calc.args.len(), args.len());
        let mut vars = VarList::new();
        for (i, arg) in args.iter().enumerate() {
            vars.set_var(calc.args[i], *arg);
        }
        self.stack.push(vars);
        self.itp_stmts(prog, &calc.body);
        let res = self.eval_expr(prog, &calc.ret);
        self.stack.pop();
        res
    }

    fn eval_args(&mut self, prog: &TProgram, args: &[Expr]) -> Vec<f64> {
        args.iter()
            .map(|expr| self.eval_expr(prog, expr))
            .collect::<Vec<_>>()
    }

    fn draw(&mut self, next_pos: (f64, f64)) {
        let start = self.map_coord(self.pos);
        let end = self.map_coord(next_pos);
        self.canvas.draw_line(start, end).unwrap();
        self.canvas.present();
        self.pos = next_pos;
        std::thread::sleep(Duration::from_millis(self.delay as u64));
        while let Some(evt) = self.events.poll_event() {
            if let sdl2::event::Event::Quit { timestamp: _ } = evt {
                std::process::exit(1);
            }
        }
    }

    fn map_coord(&self, coord: (f64, f64)) -> Point {
        Point::new(
            (WIDTH as f64 / 2.0 * (1.0 + coord.0 / self.max_coord.0)) as i32,
            (HEIGHT as f64 / 2.0 * (1.0 + coord.1 / self.max_coord.1)) as i32
        )
    }

    fn calc_next_pos(curr_pos: (f64, f64), dir: f64, dist: f64) -> (f64, f64) {
        (
            curr_pos.0 + (dir * PI / 180.0).cos() * dist,
            curr_pos.1 - (dir * PI / 180.0).sin() * dist,
        )
    }

    fn eval_cond(&mut self, prog: &TProgram, cond: &Cond) -> bool {
        match cond {
            Cond::Bracket(bcond) => self.eval_cond(prog, bcond),
            Cond::Cmp(lhs, op, rhs) => {
                let val1 = self.eval_expr(prog, lhs);
                let val2 = self.eval_expr(prog, rhs);
                match op {
                    crate::CmpOperator::Less => val1 < val2,
                    crate::CmpOperator::LessEqual => val1 <= val2,
                    crate::CmpOperator::Greater => val1 > val2,
                    crate::CmpOperator::GreaterEqual => val1 >= val2,
                    crate::CmpOperator::Equal => val1 == val2,
                    crate::CmpOperator::UnEqual => val1 != val2,
                }
            }
            Cond::And(lhs, rhs) => {
                if !self.eval_cond(prog, lhs) {
                    false
                } else {
                    self.eval_cond(prog, rhs)
                }
            }
            Cond::Or(lhs, rhs) => {
                if self.eval_cond(prog, lhs) {
                    true
                } else {
                    self.eval_cond(prog, rhs)
                }
            }
            Cond::Not(sub) => !self.eval_cond(prog, sub),
        }
    }

    fn eval_expr(&mut self, prog: &TProgram, expr: &Expr) -> f64 {
        match expr {
            Expr::Const(val) => *val,
            Expr::Variable(var) => self.get_var(var),
            Expr::BiOperation(lhs, op, rhs) => {
                let lhs = self.eval_expr(prog, lhs);
                let rhs = self.eval_expr(prog, rhs);
                match op {
                    crate::BiOperator::Add => lhs + rhs,
                    crate::BiOperator::Sub => lhs - rhs,
                    crate::BiOperator::Mul => lhs * rhs,
                    crate::BiOperator::Div => {
                        if rhs == 0.0 {
                            panic!("division by zero");
                        }
                        lhs / rhs
                    }
                    crate::BiOperator::Exp => lhs.powf(rhs),
                }
            }
            Expr::UnOperation(op, expr) => {
                let val = self.eval_expr(prog, expr);
                match op {
                    crate::UnOperator::Neg => -val,
                }
            }
            Expr::Absolute(expr) => self.eval_expr(prog, expr).abs(),
            Expr::Bracket(expr) => self.eval_expr(prog, expr),
            Expr::FuncCall(id, args) => {
                let args = self.eval_args(prog, args);
                match id {
                    PredefFunc::Sin => (args[0] * PI / 180.0).sin(),
                    PredefFunc::Cos => (args[0] * PI / 180.0).cos(),
                    PredefFunc::Tan => (args[0] * PI / 180.0).tan(),
                    PredefFunc::Sqrt => args[0].sqrt(),
                    PredefFunc::Rand => args[0] + (args[1] - args[0]) * random::<f64>(),
                }
            }
            Expr::CalcCall(id, args) => self.itp_calc(prog, *id, args),
        }
    }

    fn get_var(&mut self, var: &Variable) -> f64 {
        match var {
            Variable::Local(id) => self.stack.last_mut().unwrap().get_var(*id),
            Variable::Global(id) => self.glob.get_var(*id),
            Variable::GlobalPreDef(id) => match crate::PREDEF_VARS[*id].0 {
                "dir" => self.dir,
                "dist" => (self.pos.0 * self.pos.0 + self.pos.1 * self.pos.1).sqrt(),
                "x" => self.pos.0,
                "y" => self.pos.1,
                "1" => self.prog_args[0],
                "2" => self.prog_args[1],
                "3" => self.prog_args[2],
                "4" => self.prog_args[3],
                "5" => self.prog_args[4],
                "6" => self.prog_args[5],
                "7" => self.prog_args[6],
                "8" => self.prog_args[7],
                "9" => self.prog_args[8],
                "pi" => PI,
                "max_x" => self.max_coord.0,
                "max_y" => self.max_coord.1,
                "delay" => self.delay,
                "red" => self.col.0,
                "green" => self.col.1,
                "blue" => self.col.2,
                v => panic!("unknown predefined global varibale {v}"),
            },
        }
    }

    fn set_var(&mut self, var: &Variable, val: f64) {
        match var {
            Variable::Local(id) => self.stack.last_mut().unwrap().set_var(*id, val),
            Variable::Global(id) => self.glob.set_var(*id, val),
            Variable::GlobalPreDef(id) => {
                let pdvar = crate::PREDEF_VARS[*id];
                if !pdvar.1 {
                    panic!(
                        "tried to write to unwriteable predefined global variable {}",
                        pdvar.0
                    );
                }
                match pdvar.0 {
                    "max_x" => self.max_coord.0 = val,
                    "max_y" => self.max_coord.1 = val,
                    "delay" => self.delay = val,
                    "red" => {
                        self.col.0 = val;
                        self.canvas.set_draw_color(self.curr_col());
                    }
                    "green" => {
                        self.col.1 = val;
                        self.canvas.set_draw_color(self.curr_col());
                    }
                    "blue" => {
                        self.col.2 = val;
                        self.canvas.set_draw_color(self.curr_col());
                    }
                    v => panic!("unknown predefined global varibale {v}"),
                }
            }
        }
    }
}
