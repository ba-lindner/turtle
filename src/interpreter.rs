mod varlist;

use std::f64::consts::PI;

use rand::random;

use crate::{Cond, Expr, Statement, TProgram, Variable};

use self::varlist::VarList;

pub struct Interpreter {
    prog: Option<TProgram>,
    pos: (f64, f64),
    dir: f64,
    glob: VarList,
    stack: Vec<VarList>,
    stopped: bool,
    marks: Vec<(f64, f64)>,
    col: (f64, f64, f64),
    prog_args: [f64; 9],
    max_coord: (f64, f64),
    delay: f64,
}

impl Interpreter {
    pub fn new(prog: TProgram) -> Self {
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
            max_coord: (100.0, 100.0),
            delay: 0.0,
        };
        for (i, arg) in std::env::args().enumerate() {
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
        let prog = self.prog.take().unwrap();
        self.stack.push(VarList::new());
        self.itp_stmts(&prog, &prog.main);
    }

    fn itp_stmts(&mut self, prog: &TProgram, stmts: &Vec<Statement>) {
        for stmt in stmts {
            self.itp_stm(&prog, stmt);
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
                let new_pos = Self::calc_next_pos(self.pos, self.dir, dist);
                println!("drawing from {:?} to {:?}", self.pos, new_pos);
                self.pos = new_pos;
            },
            Statement::WalkBack(expr) => {
                let dist = self.eval_expr(prog, expr);
                let new_pos = Self::calc_next_pos(self.pos, -self.dir, dist);
                println!("drawing from {:?} to {:?}", self.pos, new_pos);
                self.pos = new_pos;
            },
            Statement::Jump(expr) => {
                let dist = self.eval_expr(prog, expr);
                let new_pos = Self::calc_next_pos(self.pos, self.dir, dist);
                self.pos = new_pos;
            },
            Statement::JumpBack(expr) => {
                let dist = self.eval_expr(prog, expr);
                let new_pos = Self::calc_next_pos(self.pos, -self.dir, dist);
                self.pos = new_pos;
            },
            Statement::WalkHome => {
                let new_pos = (0.0, 0.0);
                println!("drawing from {:?} to {:?}", self.pos, new_pos);
                self.pos = new_pos;
            },
            Statement::JumpHome => {
                self.pos = (0.0, 0.0);
            },
            Statement::TurnLeft(expr) => {
                self.dir = (self.dir + self.eval_expr(prog, expr)) % 360.0;
            },
            Statement::TurnRight(expr) => {
                self.dir = (self.dir - self.eval_expr(prog, expr)).rem_euclid(360.0);
            },
            Statement::Direction(expr) => {
                self.dir = self.eval_expr(prog, expr).rem_euclid(360.0);
            },
            Statement::Color(ex1, ex2, ex3) => {
                self.col = (self.eval_expr(prog, ex1), self.eval_expr(prog, ex2), self.eval_expr(prog, ex3));
            },
            Statement::Clear => {
                println!("cleared the screen");
            },
            Statement::Stop => {
                self.stopped = true;
                println!("halt and catch fire");
            },
            Statement::Finish => {
                self.stopped = true;
                println!("aborted");
            },
            Statement::PathCall(id, args) => {
                self.itp_path(prog, *id, args);
            },
            Statement::Store(expr, var) => {
                let val = self.eval_expr(prog, expr);
                self.set_var(var, val);
            },
            Statement::Add(expr, var) => {
                let val = self.get_var(var) + self.eval_expr(prog, expr);
                self.set_var(var, val);
            },
            Statement::Sub(expr, var) => {
                let val = self.get_var(var) - self.eval_expr(prog, expr);
                self.set_var(var, val);
            },
            Statement::Mul(expr, var) => {
                let val = self.get_var(var) * self.eval_expr(prog, expr);
                self.set_var(var, val);
            },
            Statement::Div(expr, var) => {
                let div = self.eval_expr(prog, expr);
                if div == 0.0 {
                    panic!("division by zero");
                }
                let val = self.get_var(var) / div;
                self.set_var(var, val);
            },
            Statement::Mark => {
                self.marks.push(self.pos);
            },
            Statement::WalkMark => {
                let next_pos = self.marks.pop().expect("no mark");
                println!("drawing from {:?} to {:?}", self.pos, next_pos);
                self.pos = next_pos;
            },
            Statement::JumpMark => self.pos = self.marks.pop().expect("no mark"),
            Statement::IfBranch(cond, stmts) => {
                if self.eval_cond(prog, cond) {
                    self.itp_stmts(prog, stmts);
                }
            },
            Statement::IfElseBranch(cond, if_stmts, else_stmts) => {
                if self.eval_cond(prog, cond) {
                    self.itp_stmts(prog, if_stmts);
                } else {
                    self.itp_stmts(prog, else_stmts);
                }
            },
            Statement::DoLoop(expr, stmts) => {
                let count = self.eval_expr(prog, expr).floor();
                for _ in 0..count as isize {
                    self.itp_stmts(prog, stmts);
                    if self.stopped {
                        break;
                    }
                }
            },
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
            },
            Statement::WhileLoop(cond, stmts) => {
                while self.eval_cond(prog, cond) {
                    self.itp_stmts(prog, stmts);
                    if self.stopped {
                        return;
                    }
                }
            },
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
            },
        }
    }

    fn itp_path(&mut self, prog: &TProgram, id: usize, args: &Vec<Expr>) {
        let args = self.eval_args(prog, args);
        let path = prog.paths.iter().find(|p| p.name == id).expect("missing path definition");
        assert_eq!(path.args.len(), args.len());
        let mut vars = VarList::new();
        for i in 0..args.len() {
            vars.set_var(path.args[i], args[i]);
        }
        self.stack.push(vars);
        self.itp_stmts(prog, &path.body);
        self.stack.pop();
    }

    fn itp_calc(&mut self, prog: &TProgram, id: usize, args: &Vec<Expr>) -> f64 {
        let calc = prog.calcs.iter().find(|cd| cd.name == id).expect("missing calc def");
        let args = self.eval_args(prog, args);
        assert_eq!(calc.args.len(), args.len());
        let mut vars = VarList::new();
        for i in 0..args.len() {
            vars.set_var(calc.args[i], args[i]);
        }
        self.stack.push(vars);
        self.itp_stmts(prog, &calc.body);
        let res = self.eval_expr(prog, &calc.ret);
        self.stack.pop();
        res
    }

    fn eval_args(&mut self, prog: &TProgram, args: &Vec<Expr>) -> Vec<f64> {
        args.iter().map(|expr| self.eval_expr(prog, expr)).collect::<Vec<_>>()
    }

    fn calc_next_pos(curr_pos: (f64, f64), dir: f64, dist: f64) -> (f64, f64) {
        (
            curr_pos.0 + (dir * PI / 180.0).cos() * dist,
            curr_pos.1 - (dir * PI / 180.0).sin() * dist
        )
    }

    fn eval_cond(&mut self, prog: &TProgram, cond: &Cond) -> bool {
        match cond {
            Cond::Bracket(bcond) => self.eval_cond(prog, bcond),
            Cond::Cmp(expr1, op, expr2) => {
                let val1 = self.eval_expr(prog, &expr1);
                let val2 = self.eval_expr(prog, &expr2);
                match op {
                    crate::CmpOperator::Less => val1 < val2,
                    crate::CmpOperator::LessEqual => val1 <= val2,
                    crate::CmpOperator::Greater => val1 > val2,
                    crate::CmpOperator::GreaterEqual => val1 >= val2,
                    crate::CmpOperator::Equal => val1 == val2,
                    crate::CmpOperator::UnEqual => val1 != val2,
                }
            },
            Cond::And(cond1, cond2) => {
                if !self.eval_cond(prog, &cond1) {
                    false
                } else {
                    self.eval_cond(prog, &cond2)
                }
            },
            Cond::Or(cond1, cond2) => {
                if self.eval_cond(prog, &cond1) {
                    true
                } else {
                    self.eval_cond(prog, &cond2)
                }
            },
            Cond::Not(ncond) => {
                !self.eval_cond(prog, &ncond)
            },
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
                    },
                    crate::BiOperator::Exp => lhs.powf(rhs),
                }
            },
            Expr::UnOperation(op, expr) => {
                let val = self.eval_expr(prog, expr);
                match op {
                    crate::UnOperator::Neg => -val,
                }
            },
            Expr::Absolute(expr) => self.eval_expr(prog, expr).abs(),
            Expr::Bracket(expr) => self.eval_expr(prog, expr),
            Expr::FuncCall(id, args) => {
                let args = self.eval_args(prog, args);
                match crate::KEYWORDS[*id] {
                    "sin" => (args[0] * PI / 180.0).sin(),
                    "cos" => (args[0] * PI / 180.0).cos(),
                    "tan" => (args[0] * PI / 180.0).tan(),
                    "sqrt" => args[0].sqrt(),
                    "rand" => args[0] + (args[1] - args[0]) * random::<f64>(),
                    kw => panic!("unknown function: {kw}"),
                }
            },
            Expr::CalcCall(id, args) => self.itp_calc(prog, *id, args),
        }
    }

    fn get_var(&mut self, var: &Variable) -> f64 {
        match var {
            Variable::Local(id) => self.stack.last_mut().unwrap().get_var(*id),
            Variable::Global(id) => self.glob.get_var(*id),
            Variable::GlobalPreDef(id) => {
                match crate::PREDEF_VARS[*id].0 {
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
                    "blue" => self.col.1,
                    "green" => self.col.2,
                    v => panic!("unknown predefined global varibale {v}"),
                }
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
                    panic!("tried to write to unwriteable predefined global variable {}", pdvar.0);
                }
                match pdvar.0 {
                    "max_x" => self.max_coord.0 = val,
                    "max_y" => self.max_coord.1 = val,
                    "delay" => self.delay = val,
                    "red" => self.col.0 = val,
                    "blue" => self.col.1 = val,
                    "green" => self.col.2 = val,
                    v => panic!("unknown predefined global varibale {v}"),
                }
            },
        }
    }
}