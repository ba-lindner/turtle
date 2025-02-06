use std::io::Write;

use crate::{
    predef_vars::PredefVar, BiOperator, Cond, Expr, Statement, Statements, TProgram, Variable,
};

use self::context::Context;

mod context;

// Aufwand bisher: ~8h

pub struct CComp {
    prog: TProgram,
    filename: String,
}

impl CComp {
    pub fn new(prog: TProgram) -> Self {
        Self {
            prog,
            filename: String::from("turtlegraphic.c"),
        }
    }

    pub fn set_filename(&mut self, name: impl Into<String>) {
        self.filename = name.into();
    }

    pub fn compile(&self) {
        let context = Context::new();
        // includes
        let mut content = vec![
            String::from("#include <stdlib.h>"),
            String::from("#include <stdbool.h>"),
            String::from("#include <math.h>"),
            String::from("#include \"sdlinterf.h\""),
            String::from("#include \"turtleinterf.h\""),
            String::new(),
        ];
        // function prototypes
        for pathdef in &self.prog.paths {
            let args = self.comp_args(&pathdef.args, |a| format!("double {}", self.get_ident(*a)));
            content.push(format!("void {}({});", self.get_ident(pathdef.name), args));
        }
        for calcdef in &self.prog.calcs {
            let args = self.comp_args(&calcdef.args, |a| format!("double {}", self.get_ident(*a)));
            content.push(format!(
                "double {}({});",
                self.get_ident(calcdef.name),
                args
            ));
        }
        // main function
        content.push(String::new());
        content.push(String::from("int main(int argc, const char *argv[]) {"));
        content.push(String::from("\t__ttl_init(argc, argv);"));
        content.append(&mut self.comp_stmts(&mut context.clone(), &self.prog.main));
        content.push(String::from("\treturn 0;"));
        content.push(String::from("}"));
        // other functions
        for pathdef in &self.prog.paths {
            let mut ctx = context.clone();
            for arg in &pathdef.args {
                ctx.insert(*arg, true);
            }
            let args = self.comp_args(&pathdef.args, |a| format!("double {}", self.get_ident(*a)));
            content.push(String::new());
            content.push(format!(
                "void {}({}) {{",
                self.get_ident(pathdef.name),
                args
            ));
            content.append(&mut self.comp_stmts(&mut ctx, &pathdef.body));
            content.push(String::from("}"));
        }
        for calcdef in &self.prog.calcs {
            let mut ctx = context.clone();
            for arg in &calcdef.args {
                ctx.insert(*arg, true);
            }
            let args = self.comp_args(&calcdef.args, |a| format!("double {}", self.get_ident(*a)));
            content.push(String::new());
            content.push(format!(
                "double {}({}) {{",
                self.get_ident(calcdef.name),
                args
            ));
            content.append(&mut self.comp_stmts(&mut ctx, &calcdef.body));
            content.push(format!(
                "\treturn {};",
                self.comp_expr(&mut ctx, &calcdef.ret)
            ));
            content.push(String::from("}"));
        }
        // write to file
        let path = std::path::Path::new(&self.filename);
        let mut file = std::fs::File::create(path).unwrap();
        for line in content {
            file.write_all(line.as_bytes()).unwrap();
            file.write_all("\n".as_bytes()).unwrap();
        }
    }

    fn comp_stmts(&self, ctx: &mut Context, stmts: &Statements) -> Vec<String> {
        let mut res = Vec::new();
        for stmt in stmts {
            let mut code = self.comp_stmt(ctx, &*stmt);
            for var in ctx.get_new() {
                ctx.insert(var, true);
                res.push(format!("\tdouble {} = 0.0;", self.get_ident(var)));
            }
            code.iter_mut().for_each(|line| *line = format!("\t{line}"));
            res.append(&mut code);
        }
        res
    }

    fn comp_stmt(&self, ctx: &mut Context, stmt: &Statement) -> Vec<String> {
        match stmt {
            Statement::MoveDist { dist, draw, back } => {
                vec![format!(
                    "__ttl_walk({}({}), {});",
                    if *back { "-" } else { "" },
                    self.comp_expr(ctx, dist),
                    draw,
                )]
            }
            Statement::MoveHome(draw) => vec![format!("__ttl_walk_pos(0, 0, {draw});")],
            Statement::Turn { left, by } => vec![format!(
                "__ttl_set_dir(__ttl_dir {} {});",
                if *left { "+" } else { "-" },
                self.comp_expr(ctx, by),
            )],
            Statement::Direction(expr) => {
                vec![format!("__ttl_set_dir({});", self.comp_expr(ctx, expr))]
            }
            Statement::Color(red, green, blue) => vec![
                format!("__ttl_red = {};", self.comp_expr(ctx, red)),
                format!("__ttl_green = {};", self.comp_expr(ctx, green)),
                format!("__ttl_blue = {};", self.comp_expr(ctx, blue)),
            ],
            Statement::Clear => vec![
                String::from("sdlSetBlack();"),
                String::from("sdlUpdate();"),
                String::from("sdlMilliSleep((int) __ttl_delay);"),
            ],
            Statement::Stop => vec![String::from("__ttl_stop();")],
            Statement::Finish => vec![String::from("exit(EXIT_SUCCESS);")],
            Statement::PathCall(id, args) => vec![format!(
                "{}({});",
                self.get_ident(*id),
                self.comp_args(args, |e| self.comp_expr(ctx, e))
            )],
            Statement::Store(expr, var) => vec![format!(
                "{} = {};",
                self.comp_var(ctx, var, VarAct::Init),
                self.comp_expr(ctx, expr)
            )],
            Statement::Calc { var, val, op } => vec![format!(
                "{} {}= {};",
                self.comp_var(ctx, var, VarAct::Write),
                op,
                self.comp_expr(ctx, val),
            )],
            Statement::Mark => vec![String::from("__ttl_set_mark();")],
            Statement::MoveMark(draw) => vec![format!("__ttl_load_mark({draw});")],
            Statement::IfBranch(cond, stmts) => {
                let mut res = vec![format!("if ({}) {{", self.comp_cond(ctx, cond))];
                ctx.nesting += 1;
                res.append(&mut self.comp_stmts(ctx, stmts));
                ctx.nesting -= 1;
                res.push(String::from("}"));
                res
            }
            Statement::IfElseBranch(cond, if_branch, else_branch) => {
                let mut res = vec![format!("if ({}) {{", self.comp_cond(ctx, cond))];
                ctx.nesting += 1;
                res.append(&mut self.comp_stmts(ctx, if_branch));
                res.push(String::from("} else {"));
                res.append(&mut self.comp_stmts(ctx, else_branch));
                ctx.nesting -= 1;
                res.push(String::from("}"));
                res
            }
            Statement::DoLoop(expr, stmts) => {
                let mut res = vec![format!(
                    "for (int __i_{0} = 0; __i_{0} < (int) ({1}); ++__i_{0}) {{",
                    ctx.loop_index(),
                    self.comp_expr(ctx, expr)
                )];
                ctx.nesting += 1;
                res.append(&mut self.comp_stmts(ctx, stmts));
                ctx.nesting -= 1;
                res.push(String::from("}"));
                res
            }
            Statement::CounterLoop { counter, from, up, to, step, body } => {
                let (cmp_op, step_op) = if *up { ("<", "+=") } else { (">", "-=") };
                ctx.nesting += 1;
                let mut res = vec![format!(
                    "for ({var} = {start}; {var} {cmp_op} {end}; {var} {step_op} {step}) {{",
                    var = self.comp_var(ctx, counter, VarAct::Write),
                    start = self.comp_expr(ctx, from),
                    end = self.comp_expr(ctx, to),
                    step = match step {
                        Some(expr) => self.comp_expr(ctx, expr),
                        None => String::from("1.0"),
                    },
                )];
                res.append(&mut self.comp_stmts(ctx, body));
                ctx.nesting -= 1;
                res.push(String::from("}"));
                res
            }
            Statement::WhileLoop(cond, body) => {
                let mut res = vec![format!("while ({}) {{", self.comp_cond(ctx, cond))];
                ctx.nesting += 1;
                res.append(&mut self.comp_stmts(ctx, body));
                ctx.nesting -= 1;
                res.push(String::from("}"));
                res
            }
            Statement::RepeatLoop(cond, body) => {
                let mut res = vec![String::from("do {")];
                ctx.nesting += 1;
                res.append(&mut self.comp_stmts(ctx, body));
                ctx.nesting -= 1;
                res.push(format!("}} while ({});", self.comp_cond(ctx, cond)));
                res
            }
        }
    }

    fn comp_expr(&self, ctx: &mut Context, expr: &Expr) -> String {
        match expr {
            Expr::Const(val) => format!("{val}"),
            Expr::Variable(var) => self.comp_var(ctx, var, VarAct::Read),
            Expr::BiOperation(lhs, op, rhs) => {
                if *op == BiOperator::Exp {
                    format!(
                        "pow({}, {})",
                        self.comp_expr(ctx, lhs),
                        self.comp_expr(ctx, rhs)
                    )
                } else {
                    format!(
                        "{} {op} {}",
                        self.comp_expr(ctx, lhs),
                        self.comp_expr(ctx, rhs)
                    )
                }
            }
            Expr::Negate(sub) => format!("-({})", self.comp_expr(ctx, sub)),
            Expr::Absolute(sub) => format!("abs({})", self.comp_expr(ctx, sub)),
            Expr::Bracket(sub) => format!("({})", self.comp_expr(ctx, sub)),
            Expr::FuncCall(fnname, args) => {
                let args = self.comp_args(args, |e| self.comp_expr(ctx, e));
                let transform_angle = match fnname {
                    crate::PredefFunc::Sin => true,
                    crate::PredefFunc::Cos => true,
                    crate::PredefFunc::Tan => true,
                    crate::PredefFunc::Sqrt => true,
                    crate::PredefFunc::Rand => false,
                };
                if transform_angle {
                    format!("{fnname}(({args}) * M_PI / 180.0)")
                } else {
                    format!("{fnname}({args})")
                }
            }
            Expr::CalcCall(id, args) => format!(
                "{}({})",
                self.get_ident(*id),
                self.comp_args(args, |e| self.comp_expr(ctx, e))
            ),
        }
    }

    fn comp_var(&self, ctx: &mut Context, var: &Variable, act: VarAct) -> String {
        match var {
            Variable::Local(id) => {
                let mut res = self.get_ident(*id);
                if !ctx.has_var(*id) {
                    ctx.insert(*id, act == VarAct::Init);
                    if act == VarAct::Init {
                        res = format!("double {res}");
                    }
                }
                res
            }
            Variable::Global(id) => self.get_ident(*id),
            Variable::GlobalPreDef(pdv) => {
                if act != VarAct::Read && !pdv.is_writeable() {
                    panic!(
                        "writing to readonly predefined global variable @{}",
                        pdv.get_str()
                    );
                }
                match pdv {
                    PredefVar::Dir => String::from("__ttl_dir"),
                    PredefVar::Dist => String::from("__ttl_dist()"),
                    PredefVar::X => String::from("__ttl_x"),
                    PredefVar::Y => String::from("__ttl_y"),
                    PredefVar::Arg(idx) => format!("__ttl_args[{idx}]"),
                    PredefVar::Pi => String::from("M_PI"),
                    PredefVar::MaxX => String::from("__ttl_max_x"),
                    PredefVar::MaxY => String::from("__ttl_max_y"),
                    PredefVar::Delay => String::from("__ttl_delay"),
                    PredefVar::Red => String::from("__ttl_red"),
                    PredefVar::Green => String::from("__ttl_green"),
                    PredefVar::Blue => String::from("__ttl_blue"),
                }
            }
        }
    }

    fn comp_cond(&self, ctx: &mut Context, cond: &Cond) -> String {
        match cond {
            Cond::Bracket(sub) => format!("({})", self.comp_cond(ctx, sub)),
            Cond::Cmp(lhs, op, rhs) => format!(
                "{} {op} {}",
                self.comp_expr(ctx, lhs),
                self.comp_expr(ctx, rhs)
            ),
            Cond::And(lhs, rhs) => format!(
                "{} && {}",
                self.comp_cond(ctx, lhs),
                self.comp_cond(ctx, rhs)
            ),
            Cond::Or(lhs, rhs) => format!(
                "{} || {}",
                self.comp_cond(ctx, lhs),
                self.comp_cond(ctx, rhs)
            ),
            Cond::Not(sub) => format!("!{}", self.comp_cond(ctx, sub)),
        }
    }

    fn comp_args<T, F: FnMut(&T) -> String>(&self, args: &[T], map: F) -> String {
        let mut full = args
            .iter()
            .map(map)
            .fold(String::new(), |f, a| f + ", " + &a);
        if full.is_empty() {
            full
        } else {
            full.split_off(2)
        }
    }

    fn get_ident(&self, id: usize) -> String {
        let res = self
            .prog
            .idents
            .get_index(id)
            .expect("missing identifier")
            .0;
        if res.starts_with("__") {
            format!("__loc{res}")
        } else {
            res.to_string()
        }
    }
}

#[derive(PartialEq, Debug)]
enum VarAct {
    Read,
    Write,
    Init,
}
