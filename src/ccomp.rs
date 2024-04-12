use std::io::Write;

use indexmap::IndexMap;

use crate::{BiOperator, Cond, Expr, Identified, Statement, TProgram, Variable};

use self::context::Context;

mod context;

// Aufwand bisher: ~7h

pub struct CComp<'i> {
    prog: TProgram,
    idents: &'i IndexMap<String, Identified>,
    filename: String,
}

impl<'i> CComp<'i> {
    pub fn new(prog: TProgram, idents: &'i IndexMap<String, Identified>) -> Self {
        Self {
            prog,
            idents,
            filename: "turtlegraphic.c".to_string(),
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
            content.push(format!("double {}({});", self.get_ident(calcdef.name), args));
        }
        // main function
        content.push(String::new());
        content.push("int main(int argc, const char *argv[]) {".to_string());
        content.push("\t__ttl_init(argc, argv);".to_string());
        content.append(&mut self.comp_stmts(&mut context.clone(), &self.prog.main));
        content.push("\treturn 0;".to_string());
        content.push("}".to_string());
        // other functions
        for pathdef in &self.prog.paths {
            let mut ctx = context.clone();
            for arg in &pathdef.args {
                ctx.insert(*arg, true);
            }
            let args = self.comp_args(&pathdef.args, |a| format!("double {}", self.get_ident(*a)));
            content.push(String::new());
            content.push(format!("void {}({}) {{", self.get_ident(pathdef.name), args));
            content.append(&mut self.comp_stmts(&mut ctx, &pathdef.body));
            content.push("}".to_string());
        }
        for calcdef in &self.prog.calcs {
            let mut ctx = context.clone();
            for arg in &calcdef.args {
                ctx.insert(*arg, true);
            }
            let args = self.comp_args(&calcdef.args, |a| format!("double {}", self.get_ident(*a)));
            content.push(String::new());
            content.push(format!("double {}({}) {{", self.get_ident(calcdef.name), args));
            content.append(&mut self.comp_stmts(&mut ctx, &calcdef.body));
            content.push(format!("return {};", self.comp_expr(&mut ctx, &calcdef.ret)));
            content.push("}".to_string());
        }
        // write to file
        let path = std::path::Path::new(&self.filename);
        let mut file = std::fs::File::create(path).unwrap();
        for line in content {
            file.write_all(line.as_bytes()).unwrap();
            file.write_all("\n".as_bytes()).unwrap();
        }
    }

    fn comp_stmts(&self, ctx: &mut Context, stmts: &Vec<Statement>) -> Vec<String> {
        let mut res = Vec::new();
        for stmt in stmts {
            let mut code = self.comp_stmt(ctx, stmt);
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
            Statement::Walk(expr) => vec![format!("__ttl_walk({}, true);", self.comp_expr(ctx, expr))],
            Statement::WalkBack(expr) => vec![format!("__ttl_walk(-({}), true);", self.comp_expr(ctx, expr))],
            Statement::Jump(expr) => vec![format!("__ttl_walk({}, false);", self.comp_expr(ctx, expr))],
            Statement::JumpBack(expr) => vec![format!("__ttl_walk(-({}), false);", self.comp_expr(ctx, expr))],
            Statement::WalkHome => vec![String::from("__ttl_walk_pos(0, 0, true);")],
            Statement::JumpHome => vec![String::from("__ttl_walk_pos(0, 0, false);")],
            Statement::TurnLeft(expr) => vec![format!("__ttl_set_dir(__ttl_dir + {});", self.comp_expr(ctx, expr))],
            Statement::TurnRight(expr) => vec![format!("__ttl_set_dir(__ttl_dir - {});", self.comp_expr(ctx, expr))],
            Statement::Direction(expr) => vec![format!("__ttl_set_dir({});", self.comp_expr(ctx, expr))],
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
            Statement::PathCall(id, args) => vec![
                format!("{}({});", self.get_ident(*id), self.comp_args(args, |e| self.comp_expr(ctx, e)))
            ],
            Statement::Store(expr, var) => vec![
                format!("{} = {};", self.comp_var(ctx, var, VarAct::Init), self.comp_expr(ctx, expr))
            ],
            Statement::Add(expr, var) => vec![
                format!("{} += {};", self.comp_var(ctx, var, VarAct::Write), self.comp_expr(ctx, expr))
            ],
            Statement::Sub(expr, var) => vec![
                format!("{} -= {};", self.comp_var(ctx, var, VarAct::Write), self.comp_expr(ctx, expr))
            ],
            Statement::Mul(expr, var) => vec![
                format!("{} *= {};", self.comp_var(ctx, var, VarAct::Write), self.comp_expr(ctx, expr))
            ],
            Statement::Div(expr, var) => vec![
                format!("{} /= {};", self.comp_var(ctx, var, VarAct::Write), self.comp_expr(ctx, expr))
            ],
            Statement::Mark => vec![String::from("__ttl_set_mark();")],
            Statement::WalkMark => vec![String::from("__ttl_load_mark(true);")],
            Statement::JumpMark => vec![String::from("__ttl_load_mark(false);")],
            Statement::IfBranch(cond, stmts) => {
                let mut res = vec![format!("if ({}) {{", self.comp_cond(ctx, cond))];
                res.append(&mut self.comp_stmts(&mut ctx.clone(), stmts));
                res.push(String::from("}"));
                res
            }
            Statement::IfElseBranch(cond, if_branch, else_branch) => {
                let mut res = vec![format!("if ({}) {{", self.comp_cond(ctx, cond))];
                res.append(&mut self.comp_stmts(&mut ctx.clone(), if_branch));
                res.push(String::from("} else {"));
                res.append(&mut self.comp_stmts(&mut ctx.clone(), else_branch));
                res.push(String::from("}"));
                res
            }
            Statement::DoLoop(expr, stmts) => {
                let mut res = vec![format!(
                    "for (int __i_{0} = 0; __i_{0} < (int) ({1}); ++__i_{0}) {{",
                    ctx.loop_index(),
                    self.comp_expr(ctx, expr)
                )];
                res.append(&mut self.comp_stmts(&mut ctx.clone(), stmts));
                res.push(String::from("}"));
                res
            }
            Statement::CounterLoop(var, start, up, end, step, body) => {
                let (cmp_op, step_op) = if *up {
                    ("<", ">")
                } else {
                    ("+=", "-=")
                };
                let mut res = vec![format!(
                    "for ({var} = {start}; {var} {cmp_op} {end}; {var} {step_op} {step}) {{",
                    var = self.comp_var(ctx, var, VarAct::Init),
                    start = self.comp_expr(ctx, start),
                    end = self.comp_expr(ctx, end),
                    step = match step {
                        Some(expr) => self.comp_expr(ctx, expr),
                        None => String::from("1.0"),
                    },
                )];
                res.append(&mut self.comp_stmts(&mut ctx.clone(), body));
                res.push(String::from("}"));
                res
            }
            Statement::WhileLoop(cond, body) => {
                let mut res = vec![format!("while ({}) {{", self.comp_cond(ctx, cond))];
                res.append(&mut self.comp_stmts(&mut ctx.clone(), body));
                res.push(String::from("}"));
                res
            }
            Statement::RepeatLoop(cond, body) => {
                let mut res = vec![String::from("do {")];
                res.append(&mut self.comp_stmts(&mut ctx.clone(), body));
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
                    format!("pow({}, {})", self.comp_expr(ctx, lhs), self.comp_expr(ctx, rhs))
                } else {
                    format!("{} {op} {}", self.comp_expr(ctx, lhs), self.comp_expr(ctx, rhs))
                }
            }
            Expr::UnOperation(op, sub) => format!("{op}{}", self.comp_expr(ctx, sub)),
            Expr::Absolute(sub) => format!("abs({})", self.comp_expr(ctx, sub)),
            Expr::Bracket(sub) => format!("({})", self.comp_expr(ctx, sub)),
            Expr::FuncCall(fnname, args) => format!(
                "{fnname}({})",
                self.comp_args(args, |e| self.comp_expr(ctx, e))
            ),
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
            Variable::GlobalPreDef(id) => {
                let pdv = crate::PREDEF_VARS[*id];
                if act != VarAct::Read && !pdv.1 {
                    panic!("writing to readonly predefined global variable {}", crate::PREDEF_VARS[*id].0);
                }
                if let Ok(idx) = pdv.0.parse::<u8>() {
                    format!("__ttl_args[{idx}]")
                } else {
                    match pdv.0 {
                        "dist" => String::from("__ttl_dist()"),
                        "pi" => String::from("M_PI"),
                        gv => format!("__ttl_{gv}"),
                    }
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
        let mut full = args.iter().map(map).fold("".to_string(), |f, a| f + ", " + &a);
        if full.is_empty() {
            full
        } else {
            full.split_off(2)
        }
    }

    fn get_ident(&self, id: usize) -> String {
        let res = self.idents.get_index(id).expect("missing identifier").0;
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