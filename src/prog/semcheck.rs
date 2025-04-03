use std::collections::HashMap;

use crate::{features::FeatureConf, pos::Pos, tokens::*, Identified, TurtleError};

use super::{CalcDef, PathDef, TProgram};

struct CheckContext {
    features: FeatureConf,
    protos: HashMap<usize, Prototype>,
    globals: HashMap<usize, ValType>,
    locals: HashMap<usize, ValType>,
}

impl CheckContext {
    fn var_count(&self) -> usize {
        self.globals
            .values()
            .filter(|&&vt| vt != ValType::Any)
            .count()
            + self
                .locals
                .values()
                .filter(|&&vt| vt != ValType::Any)
                .count()
    }
}

impl TProgram {
    pub(crate) fn semantic_check(&mut self) -> Result<(), TurtleError> {
        self.check_idents()?;
        self.check_event_args()?;
        let mut ctx = CheckContext {
            features: self.features,
            protos: self
                .calcs
                .iter()
                .map(|c| (c.name, c.prototype()))
                .chain(self.paths.iter().map(|p| (p.name, p.prototype())))
                .collect(),
            globals: HashMap::new(),
            locals: HashMap::new(),
        };
        type CheckFunc<'p> =
            dyn Fn(&'_ mut TProgram, &'_ mut CheckContext) -> Result<bool, TurtleError> + 'p;
        let mut checks: Vec<Box<CheckFunc<'_>>> =
            vec![Box::new(|prog, ctx| prog.main.semantic_check_loop(ctx))];
        for i in 0..self.calcs.len() {
            checks.push(Box::new(move |prog, ctx| prog.calcs[i].semantic_check(ctx)));
        }
        for i in 0..self.paths.len() {
            checks.push(Box::new(move |prog, ctx| prog.paths[i].semantic_check(ctx)));
        }
        checks.push(Box::new(move |prog, ctx| {
            if let Some(evt) = &mut prog.key_event {
                evt.semantic_check(ctx)
            } else {
                Ok(true)
            }
        }));
        checks.push(Box::new(move |prog, ctx| {
            if let Some(evt) = &mut prog.mouse_event {
                evt.semantic_check(ctx)
            } else {
                Ok(true)
            }
        }));
        while !checks.is_empty() {
            let prev_undef = ctx.var_count();
            let prev_checks = checks.len();
            let mut unfinished = Vec::new();
            for c in checks {
                if !c(self, &mut ctx)? {
                    unfinished.push(c);
                }
            }
            if ctx.var_count() == prev_undef && unfinished.len() == prev_checks {
                return Err(TurtleError::UndefGlobals(
                    self.symbols
                        .iter()
                        .enumerate()
                        .filter_map(|(idx, (_, ty))| {
                            (*ty == Identified::GlobalVar
                                && ctx.globals.get(&idx).is_none_or(|t| *t == ValType::Any))
                            .then_some(idx)
                        })
                        .collect(),
                ));
            }
            checks = unfinished;
        }
        Ok(())
    }

    fn check_event_args(&self) -> Result<(), TurtleError> {
        fn check_args(kind: EventKind, is: &[(usize, ValType)], should: &[ValType]) -> Result<(), TurtleError> {
            if is.len() != should.len() {
                return Err(TurtleError::EventArgsLength(kind, is.len(), should.len()))
            }
            for idx in 0..is.len() {
                if is[idx].1 != should[idx] {
                    return Err(TurtleError::EventArgsType(kind, idx, is[idx].1, should[idx]));
                }
            }
            Ok(())
        }
        if let Some(evt) = &self.key_event {
            check_args(EventKind::Key, &evt.args, &[ValType::String])?;
        }
        if let Some(evt) = &self.mouse_event {
            check_args(EventKind::Mouse, &evt.args, &[ValType::Number, ValType::Number, ValType::Boolean])?;
        }
        Ok(())
    }
}

#[must_use]
struct Vars(bool, bool);

impl Vars {
    pub fn new() -> Self {
        Self(true, true)
    }
}

impl std::ops::BitAnd for Vars {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 && rhs.0, self.1 && rhs.1)
    }
}

impl std::ops::BitAndAssign for Vars {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
        self.1 &= rhs.1;
    }
}

#[derive(Clone)]
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

    fn semantic_check(&mut self, ctx: &mut CheckContext) -> Result<bool, TurtleError> {
        ctx.locals = self.args.iter().cloned().collect();
        let res = self.body.semantic_check_loop(ctx);
        ctx.locals.clear();
        res
    }
}

impl CalcDef {
    fn prototype(&self) -> Prototype {
        Prototype {
            args: self.args.clone(),
            ret: Some(self.ret_ty),
        }
    }

    fn semantic_check(&mut self, ctx: &mut CheckContext) -> Result<bool, TurtleError> {
        ctx.locals = self.args.iter().cloned().collect();
        let res = self.body.semantic_check_loop(ctx);
        ctx.locals.clear();
        res
    }
}

impl Block {
    /// returns whether all global variables could be identified
    fn semantic_check_loop(&mut self, ctx: &mut CheckContext) -> Result<bool, TurtleError> {
        loop {
            let prev = ctx.var_count();
            let res = self.semantic_check(ctx)?;
            if res.0 {
                return Ok(res.1);
            }
            if prev == ctx.var_count() {
                return Err(TurtleError::UndefLocals);
            }
        }
    }

    /// returns whether all local and/or global variables could be identified
    fn semantic_check(&mut self, ctx: &mut CheckContext) -> Result<Vars, TurtleError> {
        self.statements
            .iter_mut()
            .try_fold(Vars::new(), |v, stmt| Ok(v & stmt.semantic_check(ctx)?))
    }
}

fn check_args(
    exprs: &mut [Expr],
    args: &[(usize, ValType)],
    e_map: impl Fn(TypeError) -> TurtleError,
    ctx: &mut CheckContext,
) -> Result<Vars, TurtleError> {
    if exprs.len() != args.len() {
        return Err(e_map(TypeError::ArgsWrongLength(exprs.len(), args.len())));
    }
    exprs
        .iter_mut()
        .zip(args.iter())
        .try_fold(
            Vars::new(),
            |v, (e, (_, a))| Ok(v & e.expect_type(*a, ctx)?),
        )
}

impl Pos<Statement> {
    fn semantic_check(&mut self, ctx: &mut CheckContext) -> Result<Vars, TurtleError> {
        let pos = self.get_pos();
        let e_map = |e: TypeError| TurtleError::TypeError(e, pos);
        match &mut **self {
            Statement::MoveDist { dist: expr, .. }
            | Statement::Turn { by: expr, .. }
            | Statement::Direction(expr) => Ok(expr.expect_type(ValType::Number, ctx)?),
            Statement::Color(r, g, b) => {
                let r = r.expect_type(ValType::Number, ctx)?;
                let g = g.expect_type(ValType::Number, ctx)?;
                let b = b.expect_type(ValType::Number, ctx)?;
                Ok(r & g & b)
            }
            Statement::MoveHome(_)
            | Statement::Clear
            | Statement::Stop
            | Statement::Finish
            | Statement::Mark
            | Statement::Wait
            | Statement::MoveMark(_) => Ok(Vars::new()),
            Statement::PathCall(id, exprs)
            | Statement::Split(id, exprs) => {
                check_args(exprs, &ctx.protos[&*id].clone().args, e_map, ctx)
            }
            Statement::Store(expr, var) => {
                let var_ty = var.val_type(ctx)?;
                if var_ty.0 == ValType::Any {
                    let expr = expr.val_type(ctx)?;
                    if expr.0 == ValType::Any {
                        Ok(var_ty.1 & expr.1)
                    } else {
                        var.expect_type(expr.0, ctx)?;
                        Ok(expr.1)
                    }
                } else {
                    Ok(expr.expect_type(var_ty.0, ctx)?)
                }
            }
            Statement::Calc { var, val, op } => {
                let var_ty = var.val_type(ctx)?;
                let types = op.types();
                if var_ty.0 != ValType::Any {
                    if !types.iter().any(|(_, t)| *t == var_ty.0) {
                        return Err(TurtleError::TypeError(
                            TypeError::BiOpWrongType(*op, var_ty.0),
                            self.get_pos(),
                        ));
                    }
                    Ok(val.expect_type(var_ty.0, ctx)?)
                } else if types.len() == 1 {
                    var.expect_type(types[0].0, ctx)?;
                    Ok(val.expect_type(types[0].0, ctx)?)
                } else {
                    let expr = val.val_type(ctx)?;
                    if expr.0 == ValType::Any {
                        Ok(var_ty.1 & expr.1)
                    } else {
                        if !types.iter().any(|(_, t)| *t == expr.0) {
                            return Err(TurtleError::TypeError(
                                TypeError::BiOpWrongType(*op, expr.0),
                                self.get_pos(),
                            ));
                        }
                        var.expect_type(expr.0, ctx)?;
                        Ok(expr.1)
                    }
                }
            }
            Statement::Print(expr) => Ok(expr.expect_type(ValType::String, ctx)?),
            Statement::IfBranch(expr, block)
            | Statement::WhileLoop(expr, block)
            | Statement::RepeatLoop(expr, block) => {
                let cond = expr.expect_type(ValType::Boolean, ctx)?;
                let block = block.semantic_check(ctx)?;
                Ok(cond & block)
            }
            Statement::DoLoop(expr, block) => {
                let count = expr.expect_type(ValType::Number, ctx)?;
                let block = block.semantic_check(ctx)?;
                Ok(count & block)
            }
            Statement::IfElseBranch(expr, if_block, else_block) => {
                let cond = expr.expect_type(ValType::Boolean, ctx)?;
                let ib = if_block.semantic_check(ctx)?;
                let eb = else_block.semantic_check(ctx)?;
                Ok(cond & ib & eb)
            }
            Statement::CounterLoop {
                counter,
                from,
                up: _,
                to,
                step,
                body,
            } => {
                counter.expect_type(ValType::String, ctx)?;
                let f = from.expect_type(ValType::Number, ctx)?;
                let t = to.expect_type(ValType::Number, ctx)?;
                let s = step
                    .as_mut()
                    .map(|s| s.expect_type(ValType::Number, ctx))
                    .unwrap_or(Ok(Vars::new()))?;
                let b = body.semantic_check(ctx)?;
                Ok(f & t & s & b)
            }
        }
    }
}

impl Expr {
    fn expect_type(&mut self, ty: ValType, ctx: &mut CheckContext) -> Result<Vars, TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeErrorSpan(e, self.start, self.end);
        match &mut self.kind {
            ExprKind::Const(value) => {
                value.val_type().assert(ty).map_err(e_map)?;
                Ok(Vars::new())
            }
            ExprKind::Variable(var) => {
                var.expect_type(ty, ctx)?;
                Ok(Vars::new())
            }
            ExprKind::BiOperation(lhs, op, rhs) => {
                let poss_types: Vec<_> = op
                    .types()
                    .into_iter()
                    .filter_map(|(inp, out)| (out == ty).then_some(inp))
                    .collect();
                if poss_types.len() == 1 {
                    return Ok(lhs.expect_type(poss_types[0], ctx)? & rhs.expect_type(poss_types[0], ctx)?);
                }
                let lhs_ty = lhs.val_type(ctx)?;
                if lhs_ty.0 != ValType::Any && poss_types.contains(&lhs_ty.0) {
                    return Ok(lhs_ty.1 & rhs.expect_type(lhs_ty.0, ctx)?);
                }
                let rhs_ty = rhs.val_type(ctx)?;
                if rhs_ty.0 != ValType::Any && poss_types.contains(&rhs_ty.0) {
                    return Ok(lhs.expect_type(rhs_ty.0, ctx)? & rhs_ty.1);
                }
                Err(e_map(TypeError::BiOpWrongType(*op, ty)))
            }
            ExprKind::UnOperation(op, expr) => {
                if op.val_type() != ty {
                    return Err(e_map(TypeError::UnOpWrongType(*op, ty)));
                }
                expr.expect_type(ty, ctx)
            }
            ExprKind::Absolute(expr) => {
                if ty != ValType::Number {
                    return Err(e_map(TypeError::AbsoluteValue(ty)));
                }
                expr.expect_type(ty, ctx)
            }
            ExprKind::Bracket(expr) => expr.expect_type(ty, ctx),
            ExprKind::Convert(expr, vt) => {
                vt.assert(ty).map_err(e_map)?;
                Ok(expr.val_type(ctx)?.1)
            }
            ExprKind::FuncCall(pdf, exprs) => {
                pdf.ret_type().assert(ty).map_err(e_map)?;
                let fargs = pdf.args();
                if fargs.len() != exprs.len() {
                    return Err(e_map(TypeError::ArgsWrongLength(fargs.len(), exprs.len())));
                }
                let mut v = Vars::new();
                for i in 0..fargs.len() {
                    v &= exprs[i].expect_type(fargs[i], ctx)?;
                }
                Ok(v)
            }
            ExprKind::CalcCall(idx, exprs) => {
                let proto = ctx.protos[&*idx].clone();
                proto
                    .ret
                    .expect("calc should have ret type")
                    .assert(ty)
                    .map_err(e_map)?;
                check_args(exprs, &proto.args, e_map, ctx)
            }
        }
    }

    fn val_type(&mut self, ctx: &mut CheckContext) -> Result<(ValType, Vars), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeErrorSpan(e, self.start, self.end);
        match &mut self.kind {
            ExprKind::Const(val) => Ok((val.val_type(), Vars::new())),
            ExprKind::Variable(var) => var.val_type(ctx),
            ExprKind::BiOperation(lhs, op, rhs) => {
                let (lhs_type, rhs_type) = (lhs.val_type(ctx)?, rhs.val_type(ctx)?);
                if lhs_type.0 != rhs_type.0 {
                    Err(e_map(TypeError::BiOpDifferentTypes(
                        *op, lhs_type.0, rhs_type.0,
                    )))
                } else {
                    for (inp, out) in op.types() {
                        if inp == lhs_type.0 {
                            return Ok((out, lhs_type.1 & rhs_type.1));
                        }
                    }
                    Err(e_map(TypeError::BiOpWrongType(*op, lhs_type.0)))
                }
            }
            ExprKind::UnOperation(op, expr) => {
                let ty = expr.val_type(ctx)?;
                if ty.0 == op.val_type() {
                    Ok(ty)
                } else {
                    Err(e_map(TypeError::UnOpWrongType(*op, ty.0)))
                }
            }
            ExprKind::Absolute(expr) => {
                let ty = expr.val_type(ctx)?;
                if ty.0 == ValType::Number {
                    Ok(ty)
                } else {
                    Err(e_map(TypeError::AbsoluteValue(ty.0)))
                }
            }
            ExprKind::Bracket(expr) => expr.val_type(ctx),
            ExprKind::Convert(from, to) => Ok((*to, from.val_type(ctx)?.1)),
            ExprKind::FuncCall(pdf, args) => {
                let fargs = pdf.args();
                if fargs.len() != args.len() {
                    return Err(e_map(TypeError::ArgsWrongLength(args.len(), fargs.len())));
                }
                let mut v = Vars::new();
                for (idx, arg) in args.iter_mut().enumerate() {
                    let arg_ty = arg.val_type(ctx)?;
                    if fargs[idx] != arg_ty.0 {
                        return Err(e_map(TypeError::ArgWrongType(idx, arg_ty.0, fargs[idx])));
                    }
                    v &= arg_ty.1;
                }
                Ok((pdf.ret_type(), v))
            }
            ExprKind::CalcCall(idx, exprs) => {
                let proto = ctx.protos[&*idx].clone();
                Ok((
                    proto.ret.expect("calc should have return type"),
                    check_args(exprs, &proto.args, e_map, ctx)?,
                ))
            }
        }
    }
}

impl Variable {
    fn val_type(&mut self, ctx: &mut CheckContext) -> Result<(ValType, Vars), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeError(e, self.pos);
        match &mut self.kind {
            VariableKind::Local(idx, vt) => {
                if let Some(l) = ctx.locals.get(idx) {
                    if *vt == ValType::Any {
                        *vt = *l;
                    } else {
                        vt.assert(*l).map_err(e_map)?;
                    }
                }
                Ok((*vt, Vars(*vt != ValType::Any, true)))
            }
            VariableKind::Global(idx, vt) => {
                if let Some(g) = ctx.globals.get(idx) {
                    if *vt == ValType::Any {
                        *vt = *g;
                    } else {
                        vt.assert(*g).map_err(e_map)?;
                    }
                }
                Ok((*vt, Vars(true, *vt != ValType::Any)))
            }
            VariableKind::GlobalPreDef(pdv) => Ok((pdv.val_type(&ctx.features), Vars::new())),
        }
    }

    fn expect_type(&mut self, ty: ValType, ctx: &mut CheckContext) -> Result<(), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeError(e, self.pos);
        match &mut self.kind {
            VariableKind::Local(idx, vt) => {
                if *vt == ValType::Any {
                    if let Some(l) = ctx.locals.get(idx) {
                        l.assert(ty).map_err(e_map)?;
                        *vt = *l;
                    } else {
                        ctx.locals.insert(*idx, ty);
                        *vt = ty;
                    }
                } else {
                    vt.assert(ty).map_err(e_map)?;
                    if let Some(l) = ctx.locals.get(idx) {
                        vt.assert(*l).map_err(e_map)?;
                    }
                }
            }
            VariableKind::Global(idx, vt) => {
                if *vt == ValType::Any {
                    if let Some(g) = ctx.globals.get(idx) {
                        g.assert(ty).map_err(e_map)?;
                        *vt = *g;
                    } else {
                        ctx.globals.insert(*idx, ty);
                        *vt = ty;
                    }
                } else {
                    vt.assert(ty).map_err(e_map)?;
                    if let Some(g) = ctx.globals.get(idx) {
                        vt.assert(*g).map_err(e_map)?;
                    }
                }
            }
            VariableKind::GlobalPreDef(pdv) => pdv.val_type(&ctx.features).assert(ty).map_err(e_map)?,
        }
        Ok(())
    }
}

impl ValType {
    fn assert(&self, expected: ValType) -> Result<(), TypeError> {
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
