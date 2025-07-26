//! Compiling turtle graphics programs
//!
//! Currently, only transpiling to C is available via [`CComp`].
//! Additionally, no features are implemented.

use std::io::Write;

use crate::tokens::{
    BiOperator, Block, Expr, ExprKind, PredefFunc, PredefVar, Statement, Variable, VariableKind,
};
use crate::{Identified, TProgram};

use self::context::Context;

mod context;

// Aufwand bisher: ~8h

/// A turtle-to-C compiler.
///
/// Using [`CComp::compile()`], this can be used to transpile
/// a turtle program into C. The resulting C program then needs
/// to be compiled with a regular C compiler and linked with both
/// `turtleinterf.o` and `sdlinterf.o`, which will **not** be done
/// by this transpiler.
///
/// NOTE: this is not well tested (especially since lexer and
/// parser were changed to handle additional features), thus
/// errors and miscompilations are to be expected.
pub struct CComp {
    /// The program to compile
    prog: TProgram,
    /// The file to compile to
    filename: String,
}

impl CComp {
    /// Create a new compiler for a given turtle program.
    ///
    /// TODO: panic if any features are enabled
    pub fn new(prog: TProgram) -> Self {
        Self {
            prog,
            filename: String::from("turtlegraphic.c"),
        }
    }

    /// Set the output file
    pub fn set_filename(&mut self, name: impl Into<String>) {
        self.filename = name.into();
    }

    /// Run the compilation
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
            let args = self.comp_args(&pathdef.args, |a| format!("double {}", self.get_ident(a.0)));
            content.push(format!("void {}({});", self.get_ident(pathdef.name), args));
        }
        for calcdef in &self.prog.calcs {
            let args = self.comp_args(&calcdef.args, |a| format!("double {}", self.get_ident(a.0)));
            content.push(format!(
                "double {}({});",
                self.get_ident(calcdef.name),
                args
            ));
        }
        // global variables
        for (name, kind) in &self.prog.symbols {
            if *kind == Identified::GlobalVar {
                content.push(format!("double {name} = 0.0;"));
            }
        }
        // main function
        content.push(String::new());
        content.push(String::from("int main(int argc, const char *argv[]) {"));
        content.push(String::from("\t__ttl_init(argc, argv);"));
        content.append(&mut self.comp_block(&mut context.clone(), &self.prog.main));
        content.push(String::from("\treturn 0;"));
        content.push(String::from("}"));
        // other functions
        for pathdef in &self.prog.paths {
            let mut ctx = context.clone();
            for arg in &pathdef.args {
                ctx.insert(arg.0, true);
            }
            let args = self.comp_args(&pathdef.args, |a| format!("double {}", self.get_ident(a.0)));
            content.push(String::new());
            content.push(format!(
                "void {}({}) {{",
                self.get_ident(pathdef.name),
                args
            ));
            content.append(&mut self.comp_block(&mut ctx, &pathdef.body));
            content.push(String::from("}"));
        }
        for calcdef in &self.prog.calcs {
            let mut ctx = context.clone();
            for arg in &calcdef.args {
                ctx.insert(arg.0, true);
            }
            let args = self.comp_args(&calcdef.args, |a| format!("double {}", self.get_ident(a.0)));
            content.push(String::new());
            content.push(format!(
                "double {}({}) {{",
                self.get_ident(calcdef.name),
                args
            ));
            content.append(&mut self.comp_block(&mut ctx, &calcdef.body));
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

    /// Compile a block of statements
    fn comp_block(&self, ctx: &mut Context, block: &Block) -> Vec<String> {
        let mut res = Vec::new();
        for stmt in &block.statements {
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

    /// Compile a single statement
    ///
    /// As control structures also are a single statement,
    /// this function will regularly return more than one line
    /// of code. Additionally, there are statements like `walk home`
    /// that are transpiled to multiple C statements.
    ///
    /// If any inner blocks are present, they will be indented relative
    /// to `stmt`. Thus, any indentation happening on the returned lines
    /// should happen on all lines equally. The first line is never indented.
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
            Statement::MoveHome(draw) => vec![
                String::from("__ttl_dir = 0.0;"),
                format!("__ttl_walk_pos(0.0, 0.0, {draw});"),
            ],
            Statement::Turn { left, by } => vec![format!(
                "__ttl_set_dir(__ttl_dir {} {});",
                if *left { "+" } else { "-" },
                self.comp_expr(ctx, by),
            )],
            Statement::Direction(expr) => {
                vec![format!("__ttl_set_dir({});", self.comp_expr(ctx, expr))]
            }
            Statement::Color(red, green, blue) => vec![format!(
                "__ttl_set_col({}, {}, {});",
                self.comp_expr(ctx, red),
                self.comp_expr(ctx, green),
                self.comp_expr(ctx, blue)
            )],
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
            Statement::Print(_) => todo!(),
            Statement::Split(_, _) => todo!(),
            Statement::Wait => todo!(),
            Statement::MoveMark(draw) => vec![format!("__ttl_load_mark({draw});")],
            Statement::IfBranch(cond, stmts) => {
                let mut res = vec![format!("if ({}) {{", self.comp_expr(ctx, cond))];
                ctx.nesting += 1;
                res.append(&mut self.comp_block(ctx, stmts));
                ctx.nesting -= 1;
                res.push(String::from("}"));
                res
            }
            Statement::IfElseBranch(cond, if_branch, else_branch) => {
                let mut res = vec![format!("if ({}) {{", self.comp_expr(ctx, cond))];
                ctx.nesting += 1;
                res.append(&mut self.comp_block(ctx, if_branch));
                res.push(String::from("} else {"));
                res.append(&mut self.comp_block(ctx, else_branch));
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
                res.append(&mut self.comp_block(ctx, stmts));
                ctx.nesting -= 1;
                res.push(String::from("}"));
                res
            }
            Statement::CounterLoop {
                counter,
                from,
                up,
                to,
                step,
                body,
            } => {
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
                res.append(&mut self.comp_block(ctx, body));
                ctx.nesting -= 1;
                res.push(String::from("}"));
                res
            }
            Statement::WhileLoop(cond, body) => {
                let mut res = vec![format!("while ({}) {{", self.comp_expr(ctx, cond))];
                ctx.nesting += 1;
                res.append(&mut self.comp_block(ctx, body));
                ctx.nesting -= 1;
                res.push(String::from("}"));
                res
            }
            Statement::RepeatLoop(cond, body) => {
                let mut res = vec![String::from("do {")];
                ctx.nesting += 1;
                res.append(&mut self.comp_block(ctx, body));
                ctx.nesting -= 1;
                res.push(format!("}} while (!({}));", self.comp_expr(ctx, cond)));
                res
            }
        }
    }

    /// Compile an expression into the C equivalent.
    ///
    /// Since turtle conditions are now represented as
    /// expressions with return type [`Boolean`](crate::tokens::ValType::Boolean),
    /// this function is also responsible for compiling conditions.
    fn comp_expr(&self, ctx: &mut Context, expr: &Expr) -> String {
        match &***expr {
            ExprKind::Const(val) => format!("{val}"),
            ExprKind::Variable(var) => self.comp_var(ctx, var, VarAct::Read),
            ExprKind::BiOperation(lhs, op, rhs) => {
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
            ExprKind::UnOperation(op, sub) => format!("{op}({})", self.comp_expr(ctx, sub)),
            ExprKind::Absolute(sub) => format!("abs({})", self.comp_expr(ctx, sub)),
            ExprKind::Bracket(sub) => format!("({})", self.comp_expr(ctx, sub)),
            ExprKind::Convert(_, _) => todo!(),
            ExprKind::FuncCall(fnname, args) => {
                let args = self.comp_args(args, |e| self.comp_expr(ctx, e));
                let (transform_angle, c_func) = match fnname {
                    PredefFunc::Sin => (true, "sin"),
                    PredefFunc::Cos => (true, "cos"),
                    PredefFunc::Tan => (true, "tan"),
                    PredefFunc::Sqrt => (false, "sqrt"),
                    PredefFunc::Rand => (false, "__ttl_rand"),
                    PredefFunc::Substr => (false, "__TODO__"),
                    PredefFunc::Strlen => (false, "strlen"),
                    _ => todo!(),
                };
                if transform_angle {
                    format!("{c_func}(({args}) * M_PI / 180.0)")
                } else {
                    format!("{c_func}({args})")
                }
            }
            ExprKind::CalcCall(id, args) => format!(
                "{}({})",
                self.get_ident(*id),
                self.comp_args(args, |e| self.comp_expr(ctx, e))
            ),
        }
    }

    /// "Compile" a variable
    ///
    /// This function is mostly used to get the appropriate name
    /// for a variable. However, in certain situations (distinguished
    /// by [`VarAct::Init`]), `double <name>` might be returned
    /// to declare the variable.
    fn comp_var(&self, ctx: &mut Context, var: &Variable, act: VarAct) -> String {
        match &***var {
            VariableKind::Local(id, _) => {
                let mut res = self.get_ident(*id);
                if !ctx.has_var(*id) {
                    ctx.insert(*id, act == VarAct::Init);
                    if act == VarAct::Init {
                        res = format!("double {res}");
                    }
                }
                res
            }
            VariableKind::Global(id, _) => self.get_ident(*id),
            VariableKind::GlobalPreDef(pdv) => {
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

    /// Convenience function to compile a list of arguments.
    ///
    /// This will iterate over `args`, apply `map` on each element,
    /// and return a string composed of the results of the mapping,
    /// with `", "` in between each element.
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

    /// Read an identifier from the symbol table.
    ///
    /// If the identifier starts with `__` (two underscores),
    /// `__loc` is prepended to avoid any possible conflicts with
    /// built-in identifiers, such as `__ttl_x` etc.
    fn get_ident(&self, id: usize) -> String {
        let res = self
            .prog
            .symbols
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

/// Actions on variables
///
/// This is used by [`CComp::comp_var()`] to prevent
/// writes to read-only global predefined variables and
/// allow declaration and initialisation in appropriate
/// positions.
#[derive(PartialEq, Debug)]
enum VarAct {
    /// Read the value of a variable
    Read,
    /// Write a value to a variable
    Write,
    /// Initialise a variable
    ///
    /// Similar to [`Write`](VarAct::Write), but indicates
    /// a position where a variable can be declared.
    Init,
}
