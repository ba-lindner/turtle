use sdl2::pixels::Color;
use turtle::Turtle;
use window::Window;

use crate::{BiOperator, Cond, Expr, Statement, Statements, TProgram, TurtleError};

use self::varlist::VarList;

pub mod debugger;
mod turtle;
mod varlist;
pub mod window;

// Aufwand: bisher ~4h
// have a cat

const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;
const START_COLOR: Color = Color::YELLOW;

pub struct Interpreter<'p, 'w> {
    prog: &'p TProgram,
    stopped: bool,
    turtle: Turtle<'w>,
}

impl<'p, 'w> Interpreter<'p, 'w> {
    pub fn new(prog: &'p TProgram, window: &'w Window, args: &[String]) -> Self {
        Self {
            prog,
            stopped: false,
            turtle: Turtle::new(window, args),
        }
    }

    pub fn run(&mut self) -> Result<(), ItpError> {
        self.itp_stmts(&self.prog.main)
    }

    fn itp_stmts(&mut self, stmts: &Statements) -> Result<(), ItpError> {
        for stmt in stmts {
            self.itp_stmt(stmt)?;
            if self.stopped {
                break;
            }
        }
        Ok(())
    }

    fn itp_stmt(&mut self, stmt: &Statement) -> Result<(), ItpError> {
        if self.stopped {
            return Ok(());
        }
        match stmt {
            Statement::MoveDist { dist, draw, back } => {
                let dist = self.eval_expr(dist)?;
                self.turtle.move_dist(dist, *back, *draw);
            }
            Statement::MoveHome(draw) => self.turtle.move_home(*draw),
            Statement::Turn { left, by } => {
                let angle = self.eval_expr(by)?;
                self.turtle.set_dir(self.turtle.get_dir() + angle.neg(*left));
            }
            Statement::Direction(expr) => {
                let new_dir = self.eval_expr(expr)?;
                self.turtle.set_dir(new_dir);
            }
            Statement::Color(ex1, ex2, ex3) => {
                let r = self.eval_expr(ex1)?;
                let g = self.eval_expr(ex2)?;
                let b = self.eval_expr(ex3)?;
                self.turtle.set_col(r, g, b);
            }
            Statement::Clear => self.turtle.clear(),
            Statement::Stop => {
                self.stopped = true;
                println!("halt and catch fire");
                self.turtle.wait_exit();
            }
            Statement::Finish => self.stopped = true,
            Statement::PathCall(id, args) => {
                let path = self.prog.get_path(*id)?;
                let args = self.eval_args(args)?;
                assert_eq!(path.args.len(), args.len());
                let mut vars = VarList::new();
                for (i, arg) in args.iter().enumerate() {
                    vars.set_var(path.args[i], *arg);
                }
                self.turtle.push_stack(vars);
                self.itp_stmts(&path.body)?;
                self.turtle.pop_stack();
            }
            Statement::Store(expr, var) => {
                let val = self.eval_expr(expr)?;
                self.turtle.set_var(var, val);
            }
            Statement::Calc { var, val, op } => {
                let val = op.calc(self.turtle.get_var(var), self.eval_expr(val)?);
                self.turtle.set_var(var, val);
            }
            Statement::Mark => self.turtle.new_mark(),
            Statement::MoveMark(draw) => self.turtle.move_mark(*draw),
            Statement::IfBranch(cond, stmts) => {
                if self.eval_cond(cond)? {
                    self.itp_stmts(stmts)?;
                }
            }
            Statement::IfElseBranch(cond, if_stmts, else_stmts) => {
                if self.eval_cond(cond)? {
                    self.itp_stmts(if_stmts)?;
                } else {
                    self.itp_stmts(else_stmts)?;
                }
            }
            Statement::DoLoop(expr, stmts) => {
                let count = self.eval_expr(expr)?.floor();
                for _ in 0..count as isize {
                    self.itp_stmts(stmts)?;
                    if self.stopped {
                        break;
                    }
                }
            }
            Statement::CounterLoop { counter, from, up, to, step, body } => {
                let init = self.eval_expr(from)?;
                let end = self.eval_expr(to)?;
                let step = match step {
                    Some(expr) => self.eval_expr(expr)?,
                    None => 1.0,
                }.neg(*up);
                self.turtle.set_var(counter, init);
                while *up != (self.turtle.get_var(counter) >= end) && !self.stopped {
                    self.itp_stmts(body)?;
                    let next_val = self.turtle.get_var(counter) + step;
                    self.turtle.set_var(counter, next_val);
                }
            }
            Statement::WhileLoop(cond, stmts) => {
                while self.eval_cond(cond)? && !self.stopped {
                    self.itp_stmts(stmts)?;
                }
            }
            Statement::RepeatLoop(cond, stmts) => {
                self.itp_stmts(stmts)?;
                while self.eval_cond(cond)? && !self.stopped {
                    self.itp_stmts(stmts)?;
                }
            }
        }
        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<f64, ItpError> {
        Ok(match expr {
            Expr::Const(val) => *val,
            Expr::Variable(var) => self.turtle.get_var(var),
            Expr::BiOperation(lhs, op, rhs) => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;
                if rhs == 0.0 && *op == BiOperator::Div {
                    return Err(ItpError::DivideByZero);
                }
                op.calc(lhs, rhs)
            }
            Expr::Negate(expr) => {
                -self.eval_expr(expr)?
            }
            Expr::Absolute(expr) => self.eval_expr(expr)?.abs(),
            Expr::Bracket(expr) => self.eval_expr(expr)?,
            Expr::FuncCall(pdf, args) => {
                let args = self.eval_args(args)?;
                pdf.calc(&args)
            }
            Expr::CalcCall(id, args) => {
                let calc = self.prog.get_calc(*id)?;
                let args = self.eval_args(args)?;
                assert_eq!(calc.args.len(), args.len());
                let mut vars = VarList::new();
                for (i, arg) in args.iter().enumerate() {
                    vars.set_var(calc.args[i], *arg);
                }
                self.turtle.push_stack(vars);
                self.itp_stmts(&calc.body)?;
                let res = self.eval_expr(&calc.ret)?;
                self.turtle.pop_stack();
                res
            }
        })
    }

    fn eval_cond(&mut self, cond: &Cond) -> Result<bool, ItpError> {
        Ok(match cond {
            Cond::Bracket(bcond) => self.eval_cond(bcond)?,
            Cond::Cmp(lhs, op, rhs) => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;
                op.compare(lhs, rhs)
            }
            Cond::And(lhs, rhs) => {
                self.eval_cond(lhs)? && self.eval_cond(rhs)?
            }
            Cond::Or(lhs, rhs) => {
                self.eval_cond(lhs)? || self.eval_cond(rhs)?
            }
            Cond::Not(sub) => !self.eval_cond(sub)?,
        })
    }

    fn eval_args(&mut self, args: &[Expr]) -> Result<Vec<f64>, ItpError> {
        args.iter()
            .map(|expr| self.eval_expr(expr))
            .collect::<Result<Vec<_>, _>>()
    }
}

#[derive(Debug)]
pub enum ItpError {
    DivideByZero,
    AlreadyRunning,
    // this should never appear as it means my code contains bugs
    TurtleError(TurtleError),
}

impl From<TurtleError> for ItpError {
    fn from(value: TurtleError) -> Self {
        Self::TurtleError(value)
    }
}

pub trait FloatExt {
    fn neg(self, negate: bool) -> f64;
}

impl FloatExt for f64 {
    fn neg(self, negate: bool) -> f64 {
        if negate {
            -self
        } else {
            self
        }
    }
}
