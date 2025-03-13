use std::collections::HashMap;

use crate::{pos::Pos, tokens::*, Identified, TurtleError};

use super::{CalcDef, PathDef, TProgram};

impl TProgram {
    pub(crate) fn semantic_check(&mut self) -> Result<(), TurtleError> {
        self.check_idents()?;
        let mut globals = HashMap::new();
        let protos: HashMap<_, _> = self
            .calcs
            .iter()
            .map(|c| (c.name, c.prototype()))
            .chain(self.paths.iter().map(|p| (p.name, p.prototype())))
            .collect();
        let pref = &protos;
        type CheckFunc<'p> = dyn Fn(&'_ mut TProgram, &'_ mut HashMap<usize, ValType>) -> Result<bool, TurtleError>
            + 'p;
        let mut checks: Vec<Box<CheckFunc<'_>>> = vec![Box::new(|prog: &mut TProgram, glob| {
            prog.main
                .semantic_check_loop(pref, glob, &mut HashMap::new())
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
}

pub struct Prototype {
    pub args: ArgDefList,
    pub ret: Option<ValType>,
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
                for (arg, expr) in proto.args.iter().zip(exprs.iter_mut()) {
                    let (l, g) = expr.expect_type(arg.1, protos, globals, locals)?;
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

impl Expr {
    pub fn expect_type(
        &mut self,
        ty: ValType,
        protos: &HashMap<usize, Prototype>,
        globals: &mut HashMap<usize, ValType>,
        locals: &mut HashMap<usize, ValType>,
    ) -> Result<(bool, bool), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeErrorSpan(e, self.start, self.end);
        match &mut self.kind {
            ExprKind::Const(value) => {
                value.val_type().assert(ty).map_err(e_map)?;
                Ok((true, true))
            }
            ExprKind::Variable(var) => {
                var.expect_type(ty, locals, globals)?;
                Ok((true, true))
            }
            ExprKind::BiOperation(lhs, op, rhs) => {
                for (inp, out) in op.types() {
                    if out == ty {
                        let (lhs_ok, rhs_ok) = (
                            lhs.expect_type(inp, protos, globals, locals)?,
                            rhs.expect_type(inp, protos, globals, locals)?,
                        );
                        return Ok((lhs_ok.0 && rhs_ok.0, lhs_ok.1 && rhs_ok.1));
                    }
                }
                Err(e_map(TypeError::BiOpWrongType(*op, ty)))
            }
            ExprKind::UnOperation(op, expr) => {
                if op.val_type() != ty {
                    return Err(e_map(TypeError::UnOpWrongType(*op, ty)));
                }
                expr.expect_type(ty, protos, globals, locals)
            }
            ExprKind::Absolute(expr) => {
                if ty != ValType::Number {
                    return Err(e_map(TypeError::AbsoluteValue(ty)));
                }
                expr.expect_type(ty, protos, globals, locals)
            }
            ExprKind::Bracket(expr) => expr.expect_type(ty, protos, globals, locals),
            ExprKind::Convert(expr, vt) => {
                vt.assert(ty).map_err(e_map)?;
                let (_, l, g) = expr.val_type(protos, globals, locals)?;
                Ok((l, g))
            }
            ExprKind::FuncCall(pdf, exprs) => {
                pdf.ret_type().assert(ty).map_err(e_map)?;
                let fargs = pdf.args();
                if fargs.len() != exprs.len() {
                    return Err(e_map(TypeError::ArgsWrongLength(fargs.len(), exprs.len())));
                }
                let (mut loc, mut glob) = (true, true);
                for i in 0..fargs.len() {
                    let (l, g) = exprs[i].expect_type(fargs[i], protos, globals, locals)?;
                    loc &= l;
                    glob &= g;
                }
                Ok((loc, glob))
            }
            ExprKind::CalcCall(idx, exprs) => {
                let proto = &protos[&*idx];
                proto
                    .ret
                    .expect("calc should have ret type")
                    .assert(ty)
                    .map_err(e_map)?;
                if proto.args.len() != exprs.len() {
                    return Err(e_map(TypeError::ArgsWrongLength(
                        proto.args.len(),
                        exprs.len(),
                    )));
                }
                let (mut loc, mut glob) = (true, true);
                for (arg, expr) in proto.args.iter().zip(exprs.iter_mut()) {
                    let (l, g) = expr.expect_type(arg.1, protos, globals, locals)?;
                    loc &= l;
                    glob &= g;
                }
                Ok((loc, glob))
            }
        }
    }

    pub fn val_type(
        &mut self,
        protos: &HashMap<usize, Prototype>,
        globals: &mut HashMap<usize, ValType>,
        locals: &mut HashMap<usize, ValType>,
    ) -> Result<(ValType, bool, bool), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeErrorSpan(e, self.start, self.end);
        match &mut self.kind {
            ExprKind::Const(val) => Ok((val.val_type(), true, true)),
            ExprKind::Variable(var) => var.val_type(locals, globals),
            ExprKind::BiOperation(lhs, op, rhs) => {
                let (lhs_type, rhs_type) = (
                    lhs.val_type(protos, globals, locals)?,
                    rhs.val_type(protos, globals, locals)?,
                );
                if lhs_type.0 != rhs_type.0 {
                    Err(e_map(TypeError::BiOpDifferentTypes(
                        *op, lhs_type.0, rhs_type.0,
                    )))
                } else {
                    for (inp, out) in op.types() {
                        if inp == lhs_type.0 {
                            return Ok((out, lhs_type.1 && rhs_type.1, lhs_type.2 && rhs_type.2));
                        }
                    }
                    Err(e_map(TypeError::BiOpWrongType(*op, lhs_type.0)))
                }
            }
            ExprKind::UnOperation(op, expr) => {
                let ty = expr.val_type(protos, globals, locals)?;
                if ty.0 == op.val_type() {
                    Ok(ty)
                } else {
                    Err(e_map(TypeError::UnOpWrongType(*op, ty.0)))
                }
            }
            ExprKind::Absolute(expr) => {
                let ty = expr.val_type(protos, globals, locals)?;
                if ty.0 == ValType::Number {
                    Ok(ty)
                } else {
                    Err(e_map(TypeError::AbsoluteValue(ty.0)))
                }
            }
            ExprKind::Bracket(expr) => expr.val_type(protos, globals, locals),
            ExprKind::Convert(from, to) => {
                let (_, loc, glob) = from.val_type(protos, globals, locals)?;
                Ok((*to, loc, glob))
            }
            ExprKind::FuncCall(pdf, args) => {
                let fargs = pdf.args();
                if fargs.len() != args.len() {
                    return Err(e_map(TypeError::ArgsWrongLength(args.len(), fargs.len())));
                }
                let (mut loc, mut glob) = (true, true);
                for (idx, arg) in args.iter_mut().enumerate() {
                    let arg_ty = arg.val_type(protos, globals, locals)?;
                    if fargs[idx] != arg_ty.0 {
                        return Err(e_map(TypeError::ArgWrongType(idx, arg_ty.0, fargs[idx])));
                    }
                    loc &= arg_ty.1;
                    glob &= arg_ty.2;
                }
                Ok((pdf.ret_type(), loc, glob))
            }
            ExprKind::CalcCall(idx, exprs) => {
                let proto = &protos[&*idx];
                if proto.args.len() != exprs.len() {
                    return Err(e_map(TypeError::ArgsWrongLength(
                        exprs.len(),
                        proto.args.len(),
                    )));
                }
                let (mut loc, mut glob) = (true, true);
                for (idx, arg) in exprs.iter_mut().enumerate() {
                    let arg_ty = arg.val_type(protos, globals, locals)?;
                    if proto.args[idx].1 != arg_ty.0 {
                        return Err(e_map(TypeError::ArgWrongType(
                            idx,
                            arg_ty.0,
                            proto.args[idx].1,
                        )));
                    }
                    loc &= arg_ty.1;
                    glob &= arg_ty.2;
                }
                Ok((proto.ret.expect("calc should have return type"), loc, glob))
            }
        }
    }
}

impl Variable {
    pub fn val_type(
        &mut self,
        locals: &mut HashMap<usize, ValType>,
        globals: &mut HashMap<usize, ValType>,
    ) -> Result<(ValType, bool, bool), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeError(e, self.pos);
        match &mut self.kind {
            VariableKind::Local(idx, vt) => {
                if let Some(l) = locals.get(idx) {
                    if *vt == ValType::Any {
                        *vt = *l;
                    } else {
                        vt.assert(*l).map_err(e_map)?;
                    }
                }
                Ok((*vt, *vt != ValType::Any, true))
            }
            VariableKind::Global(idx, vt) => {
                if let Some(g) = globals.get(idx) {
                    if *vt == ValType::Any {
                        *vt = *g;
                    } else {
                        vt.assert(*g).map_err(e_map)?;
                    }
                }
                Ok((*vt, true, *vt != ValType::Any))
            }
            VariableKind::GlobalPreDef(pdv) => Ok((pdv.val_type(), true, true)),
        }
    }

    pub fn expect_type(
        &mut self,
        ty: ValType,
        locals: &mut HashMap<usize, ValType>,
        globals: &mut HashMap<usize, ValType>,
    ) -> Result<(), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeError(e, self.pos);
        match &mut self.kind {
            VariableKind::Local(idx, vt) => {
                if *vt == ValType::Any {
                    if let Some(l) = locals.get(idx) {
                        l.assert(ty).map_err(e_map)?;
                        *vt = *l;
                    } else {
                        locals.insert(*idx, ty);
                        *vt = ty;
                    }
                } else {
                    vt.assert(ty).map_err(e_map)?;
                    if let Some(l) = locals.get(idx) {
                        vt.assert(*l).map_err(e_map)?;
                    }
                }
            }
            VariableKind::Global(idx, vt) => {
                if *vt == ValType::Any {
                    if let Some(g) = globals.get(idx) {
                        g.assert(ty).map_err(e_map)?;
                        *vt = *g;
                    } else {
                        globals.insert(*idx, ty);
                        *vt = ty;
                    }
                } else {
                    vt.assert(ty).map_err(e_map)?;
                    if let Some(g) = globals.get(idx) {
                        vt.assert(*g).map_err(e_map)?;
                    }
                }
            }
            VariableKind::GlobalPreDef(pdv) => pdv.val_type().assert(ty).map_err(e_map)?,
        }
        Ok(())
    }
}

impl ValType {
    pub fn assert(&self, expected: ValType) -> Result<(), TypeError> {
        if *self == expected {
            Ok(())
        } else {
            Err(TypeError::WrongType(*self, expected))
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("different types for operator {0}: {1} != {2}")]
    BiOpDifferentTypes(BiOperator, ValType, ValType),
    #[error("operator {0} not defined for {1}")]
    BiOpWrongType(BiOperator, ValType),
    #[error("operator {0} not defined for {1}")]
    UnOpWrongType(UnOperator, ValType),
    #[error("absolute value not defined for {0}")]
    AbsoluteValue(ValType),
    #[error("wrong number of arguments: got {0}, expected {1}")]
    ArgsWrongLength(usize, usize),
    #[error("argument #{0} has type {1}, should have {2}")]
    ArgWrongType(usize, ValType, ValType),
    #[error("wrong type: got {0}, expected {1}")]
    WrongType(ValType, ValType),
}
