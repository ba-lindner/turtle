use std::collections::HashMap;

use crate::{
    debugger::ItpRunner,
    pos::Pos,
    tokens::{ArgDefList, Block, Expr, ParseToken, Statement, TypeError, ValType},
    DebugRunner, Identified, SymbolTable, TurtleError,
};

use lexer::Lexer;
use parser::Parser;

pub mod lexer;
pub mod parser;

#[derive(Default)]
struct RawProg {
    paths: Vec<PathDef>,
    calcs: Vec<CalcDef>,
    main: Option<Block>,
}

impl RawProg {
    fn insert_item(&mut self, item: ParseToken) -> Result<(), TurtleError> {
        match item {
            ParseToken::PathDef(def) => self.paths.push(def),
            ParseToken::CalcDef(def) => self.calcs.push(def),
            ParseToken::StartBlock(block) => {
                if let Some(main) = &self.main {
                    return Err(TurtleError::MultipleMains(main.begin, block.begin));
                }
                self.main = Some(block);
            }
        }
        Ok(())
    }

    fn finish(self, symbols: SymbolTable) -> Result<TProgram, TurtleError> {
        let Some(main) = self.main else {
            return Err(TurtleError::MissingMain);
        };
        Ok(TProgram {
            name: None,
            paths: self.paths,
            calcs: self.calcs,
            main,
            symbols,
        })
    }
}

/// A full and valid turtle program.
#[derive(Debug)]
pub struct TProgram {
    pub name: Option<String>,
    pub paths: Vec<PathDef>,
    pub calcs: Vec<CalcDef>,
    pub main: Block,
    pub symbols: SymbolTable,
}

impl TProgram {
    pub fn parse(code: String) -> Result<Self, TurtleError> {
        let mut symbols = SymbolTable::new();
        let ltokens = Lexer::new(&mut symbols, code.chars()).collect_tokens()?;
        let parser = Parser::new(&mut symbols, ltokens);
        let mut raw = RawProg::default();
        for item in parser {
            raw.insert_item(item?)?;
        }
        let mut this = raw.finish(symbols)?;
        this.semantic_check()?;
        Ok(this)
    }

    pub fn from_file(file: &str) -> Result<Self, TurtleError> {
        let code = std::fs::read_to_string(file)?;
        let mut this = Self::parse(code)?;
        this.name = Some(file.to_string());
        Ok(this)
    }

    pub fn check_code(code: String, print_symbols: bool) {
        let mut symbols = SymbolTable::new();
        let pr_sym = |symbols: &SymbolTable| {
            if print_symbols {
                for (idx, (name, kind)) in symbols.iter().enumerate() {
                    println!("#{idx:<3} {name:<20} {kind}")
                }
            }
        };

        let ltokens = match Lexer::new(&mut symbols, code.chars()).collect_tokens() {
            Ok(tokens) => tokens,
            Err(why) => {
                eprintln!("{why}");
                pr_sym(&symbols);
                return;
            }
        };
        let parser = Parser::new(&mut symbols, ltokens);
        let mut raw = RawProg::default();
        for item in parser {
            match item {
                Ok(item) => {
                    if let Err(why) = raw.insert_item(item) {
                        eprintln!("{why}");
                        pr_sym(&symbols);
                        return;
                    }
                }
                Err(why) => {
                    eprintln!("{} at {}", *why, why.get_pos());
                    pr_sym(&symbols);
                    return;
                }
            }
        }
        let mut prog = match raw.finish(symbols) {
            Ok(prog) => prog,
            Err(why) => {
                eprintln!("{why}");
                return;
            }
        };
        if let Err(why) = prog.semantic_check() {
            eprintln!("{why}");
            pr_sym(&prog.symbols);
            return;
        }
    }

    fn semantic_check(&mut self) -> Result<(), TurtleError> {
        self.check_idents()?;
        let mut globals = HashMap::new();
        let protos: HashMap<_, _> = self
            .calcs
            .iter()
            .map(|c| (c.name, c.prototype()))
            .chain(self.paths.iter().map(|p| (p.name, p.prototype())))
            .collect();
        let pref = &protos;
        let mut checks: Vec<
            Box<dyn Fn(&mut TProgram, &mut HashMap<usize, ValType>) -> Result<bool, TurtleError>>,
        > = vec![Box::new(|prog: &mut TProgram, glob| {
            prog.main
                .semantic_check_loop(&protos, glob, &mut HashMap::new())
        })];
        for i in 0..self.calcs.len() {
            checks.push(Box::new(move |prog, glob| {
                prog.calcs[i].semantic_check(pref, glob)
            }));
        }
        for i in 0..self.paths.len() {
            checks.push(Box::new(move |prog, glob| {
                prog.paths[i].semantic_check(pref, glob)
            }));
        }
        while !checks.is_empty() {
            let undef_global_before = globals.values().filter(|&&vt| vt == ValType::Any).count();
            let check_count_before = checks.len();
            let mut unfinished = Vec::new();
            for c in checks {
                if !c(self, &mut globals)? {
                    unfinished.push(c);
                }
            }
            let undef_global_after = globals.values().filter(|&&vt| vt == ValType::Any).count();
            if undef_global_after == undef_global_before && unfinished.len() == check_count_before {
                return Err(TurtleError::UndefGlobals(
                    self.symbols
                        .iter()
                        .enumerate()
                        .filter_map(|(idx, (_, ty))| {
                            (*ty == Identified::GlobalVar && globals[&idx] == ValType::Any)
                                .then_some(idx)
                        })
                        .collect(),
                ));
            }
            checks = unfinished;
        }
        Ok(())
    }

    fn check_idents(&self) -> Result<(), TurtleError> {
        for (id, (_, kind)) in self.symbols.iter().enumerate() {
            match kind {
                Identified::Unknown => {
                    return Err(TurtleError::UnidentifiedIdentifier(id));
                }
                Identified::Path(args) => {
                    let path = self.get_path(id)?;
                    assert_eq!(path.args.len(), *args);
                }
                Identified::Calc(args) => {
                    let calc = self.get_calc(id)?;
                    assert_eq!(calc.args.len(), *args);
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub(crate) fn get_path(&self, name: usize) -> Result<&PathDef, TurtleError> {
        self.paths
            .iter()
            .find(|path| path.name == name)
            .ok_or(TurtleError::MissingDefinition(name))
    }

    pub(crate) fn get_calc(&self, name: usize) -> Result<&CalcDef, TurtleError> {
        self.calcs
            .iter()
            .find(|calc| calc.name == name)
            .ok_or(TurtleError::MissingDefinition(name))
    }

    pub fn title(&self, kind: &str) -> String {
        if let Some(name) = &self.name {
            format!("Turtle {kind} - {name}")
        } else {
            format!("Turtle {kind}")
        }
    }

    pub fn interpret(&self, args: &[String]) {
        ItpRunner::new(self, args, &self.title("Interpreter")).run();
    }

    pub fn debug(&self, args: &[String], breakpoints: &[String]) {
        let mut dbg = DebugRunner::new(self, args, &self.title("Debugger"));
        dbg.set_breakpoints(breakpoints);
        dbg.run();
    }
}

/// Path definition in turtle program
#[derive(Debug)]
pub struct PathDef {
    pub name: usize,
    pub args: ArgDefList,
    pub body: Block,
}

impl PathDef {
    fn prototype(&self) -> Prototype {
        Prototype {
            args: self.args.clone(),
            ret: None,
        }
    }

    fn semantic_check(
        &mut self,
        protos: &HashMap<usize, Prototype>,
        globals: &mut HashMap<usize, ValType>,
    ) -> Result<bool, TurtleError> {
        let mut locals = self.args.iter().cloned().collect();
        self.body.semantic_check_loop(protos, globals, &mut locals)
    }
}

/// Calc definition in turtle program
#[derive(Debug)]
pub struct CalcDef {
    pub name: usize,
    pub args: ArgDefList,
    pub ret_ty: ValType,
    pub body: Block,
    pub ret: Expr,
}

impl CalcDef {
    fn prototype(&self) -> Prototype {
        Prototype {
            args: self.args.clone(),
            ret: Some(self.ret_ty),
        }
    }

    fn semantic_check(
        &mut self,
        protos: &HashMap<usize, Prototype>,
        globals: &mut HashMap<usize, ValType>,
    ) -> Result<bool, TurtleError> {
        let mut locals: HashMap<_, _> = self.args.iter().cloned().collect();
        self.body.semantic_check_loop(protos, globals, &mut locals)
    }
}

pub struct Prototype {
    pub args: ArgDefList,
    pub ret: Option<ValType>,
}

impl Block {
    /// returns whether all global variables could be identified
    fn semantic_check_loop(
        &mut self,
        protos: &HashMap<usize, Prototype>,
        globals: &mut HashMap<usize, ValType>,
        locals: &mut HashMap<usize, ValType>,
    ) -> Result<bool, TurtleError> {
        loop {
            let prev = globals.values().filter(|&&t| t != ValType::Any).count()
                + locals.values().filter(|&&t| t != ValType::Any).count();
            let res = self.semantic_check(protos, globals, locals)?;
            if res.0 {
                return Ok(res.1);
            }
            if prev
                == globals.values().filter(|&&t| t != ValType::Any).count()
                    + locals.values().filter(|&&t| t != ValType::Any).count()
            {
                return Err(TurtleError::UndefLocals);
            }
        }
    }

    /// returns whether all local and/or global variables could be identified
    fn semantic_check(
        &mut self,
        protos: &HashMap<usize, Prototype>,
        globals: &mut HashMap<usize, ValType>,
        locals: &mut HashMap<usize, ValType>,
    ) -> Result<(bool, bool), TurtleError> {
        let (mut loc, mut glob) = (true, true);
        for stmt in &mut self.statements {
            let (l, g) = stmt.semantic_check(protos, globals, locals)?;
            loc &= l;
            glob &= g;
        }
        Ok((loc, glob))
    }
}

impl Pos<Statement> {
    fn semantic_check(
        &mut self,
        protos: &HashMap<usize, Prototype>,
        globals: &mut HashMap<usize, ValType>,
        locals: &mut HashMap<usize, ValType>,
    ) -> Result<(bool, bool), TurtleError> {
        match &mut **self {
            Statement::MoveDist { dist: expr, .. }
            | Statement::Turn { by: expr, .. }
            | Statement::Direction(expr) => {
                Ok(expr.expect_type(ValType::Number, protos, globals, locals)?)
            }
            Statement::Color(r, g, b) => {
                let r = r.expect_type(ValType::Number, protos, globals, locals)?;
                let g = g.expect_type(ValType::Number, protos, globals, locals)?;
                let b = b.expect_type(ValType::Number, protos, globals, locals)?;
                Ok((r.0 && g.0 && b.0, r.1 && g.1 && b.1))
            }
            Statement::MoveHome(_)
            | Statement::Clear
            | Statement::Stop
            | Statement::Finish
            | Statement::Mark
            | Statement::MoveMark(_) => Ok((true, true)),
            Statement::PathCall(id, exprs) => {
                let proto = &protos[&*id];
                if proto.args.len() != exprs.len() {
                    return Err(TurtleError::TypeError(
                        TypeError::ArgsWrongLength(proto.args.len(), exprs.len()),
                        self.get_pos(),
                    ));
                }
                let (mut loc, mut glob) = (true, true);
                for i in 0..proto.args.len() {
                    let (l, g) = exprs[i].expect_type(proto.args[i].1, protos, globals, locals)?;
                    loc &= l;
                    glob &= g;
                }
                Ok((loc, glob))
            }
            Statement::Store(expr, var) => {
                let var_ty = var.val_type(locals, globals)?;
                if var_ty.0 == ValType::Any {
                    let expr = expr.val_type(protos, globals, locals)?;
                    if expr.0 == ValType::Any {
                        Ok((var_ty.1 && expr.1, var_ty.2 && expr.2))
                    } else {
                        var.expect_type(expr.0, locals, globals)?;
                        Ok((expr.1, expr.2))
                    }
                } else {
                    Ok(expr.expect_type(var_ty.0, protos, globals, locals)?)
                }
            }
            Statement::Calc { var, val, op } => {
                let var_ty = var.val_type(locals, globals)?;
                let types = op.types();
                if var_ty.0 != ValType::Any {
                    if !types.iter().any(|(_, t)| *t == var_ty.0) {
                        return Err(TurtleError::TypeError(
                            TypeError::BiOpWrongType(*op, var_ty.0),
                            self.get_pos(),
                        ));
                    }
                    Ok(val.expect_type(var_ty.0, protos, globals, locals)?)
                } else if types.len() == 1 {
                    var.expect_type(types[0].0, locals, globals)?;
                    Ok(val.expect_type(types[0].0, protos, globals, locals)?)
                } else {
                    let expr = val.val_type(protos, globals, locals)?;
                    if expr.0 == ValType::Any {
                        Ok((var_ty.1 && expr.1, var_ty.2 && expr.2))
                    } else {
                        if !types.iter().any(|(_, t)| *t == expr.0) {
                            return Err(TurtleError::TypeError(
                                TypeError::BiOpWrongType(*op, expr.0),
                                self.get_pos(),
                            ));
                        }
                        var.expect_type(expr.0, locals, globals)?;
                        Ok((expr.1, expr.2))
                    }
                }
            }
            Statement::Print(expr) => {
                Ok(expr.expect_type(ValType::String, protos, globals, locals)?)
            }
            Statement::IfBranch(expr, block)
            | Statement::WhileLoop(expr, block)
            | Statement::RepeatLoop(expr, block) => {
                let cond = expr.expect_type(ValType::Boolean, protos, globals, locals)?;
                let block = block.semantic_check(protos, globals, locals)?;
                Ok((cond.0 && block.0, cond.1 && block.1))
            }
            Statement::DoLoop(expr, block) => {
                let count = expr.expect_type(ValType::Number, protos, globals, locals)?;
                let block = block.semantic_check(protos, globals, locals)?;
                Ok((count.0 && block.0, count.1 && block.1))
            }
            Statement::IfElseBranch(expr, if_block, else_block) => {
                let cond = expr.expect_type(ValType::Boolean, protos, globals, locals)?;
                let ib = if_block.semantic_check(protos, globals, locals)?;
                let eb = else_block.semantic_check(protos, globals, locals)?;
                Ok((cond.0 && ib.0 && eb.0, cond.1 && ib.1 && eb.1))
            }
            Statement::CounterLoop {
                counter,
                from,
                up: _,
                to,
                step,
                body,
            } => {
                counter.expect_type(ValType::String, locals, globals)?;
                let f = from.expect_type(ValType::Number, protos, globals, locals)?;
                let t = to.expect_type(ValType::Number, protos, globals, locals)?;
                let s = step
                    .as_mut()
                    .map(|s| s.expect_type(ValType::Number, protos, globals, locals))
                    .unwrap_or(Ok((true, true)))?;
                let b = body.semantic_check(protos, globals, locals)?;
                Ok((f.0 && t.0 && s.0 && b.0, f.1 && t.1 && s.1 && b.1))
            }
        }
    }
}
