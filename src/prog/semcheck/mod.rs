use std::collections::HashMap;

use crate::{Identified, TurtleError, debugger::FuncType, features::FeatureConf, tokens::*};

use super::{CalcDef, PathDef, TProgram};

mod expr;
mod statements;
#[cfg(test)]
mod test;
mod variable;

pub(crate) struct CheckContext {
    pub(crate) features: FeatureConf,
    pub(crate) protos: HashMap<usize, Prototype>,
    pub(crate) globals: HashMap<usize, ValType>,
    pub(crate) locals: HashMap<usize, ValType>,
}

impl CheckContext {
    pub fn var_count(&self) -> usize {
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
        for param in &mut self.params {
            ctx.globals.insert(param.name, param.value.val_type());
        }
        type CheckFunc<'p> =
            dyn Fn(&'_ mut TProgram, &'_ mut CheckContext) -> Result<bool, TurtleError> + 'p;
        let mut checks: Vec<Box<CheckFunc<'_>>> = vec![Box::new(|prog, ctx| {
            prog.main.semantic_check_loop(ctx, FuncType::Main)
        })];
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
                        .filter_map(|(idx, (_, ty))| match (*ty, ctx.globals.get(&idx)) {
                            (Identified::GlobalVar, None)
                            | (Identified::GlobalVar, Some(ValType::Any)) => Some(idx),
                            _ => None,
                        })
                        .collect(),
                ));
            }
            checks = unfinished;
        }
        Ok(())
    }

    pub(crate) fn check_event_args(&self) -> Result<(), TurtleError> {
        if let Some(evt) = &self.key_event {
            compare_args(EventKind::Key, &evt.args, &[ValType::String])?;
        }
        if let Some(evt) = &self.mouse_event {
            compare_args(
                EventKind::Mouse,
                &evt.args,
                &[ValType::Number, ValType::Number, ValType::Boolean],
            )?;
        }
        Ok(())
    }

    pub(crate) fn get_context(&self) -> CheckContext {
        let mut res = CheckContext {
            features: self.features,
            protos: HashMap::new(),
            globals: HashMap::new(),
            locals: HashMap::new(),
        };
        for path in &self.paths {
            path.collect_context(&mut res);
        }
        for calc in &self.calcs {
            calc.collect_context(&mut res);
        }
        if let Some(evt) = &self.key_event {
            evt.collect_context(&mut res);
        }
        if let Some(evt) = &self.mouse_event {
            evt.collect_context(&mut res);
        }
        for path in &*self.extensions.paths.read() {
            path.collect_context(&mut res);
        }
        for calc in &*self.extensions.calcs.read() {
            calc.collect_context(&mut res);
        }
        if let Some(evt) = &*self.extensions.key_event.read() {
            evt.collect_context(&mut res);
        }
        if let Some(evt) = &*self.extensions.mouse_event.read() {
            evt.collect_context(&mut res);
        }
        res.locals.clear();
        res
    }
}

pub(crate) fn compare_args(
    kind: EventKind,
    is: &[(usize, ValType)],
    should: &[ValType],
) -> Result<(), TurtleError> {
    if is.len() > should.len() {
        return Err(TurtleError::EventArgsLength(kind, is.len(), should.len()));
    }
    for idx in 0..is.len() {
        if is[idx].1 != should[idx] {
            return Err(TurtleError::EventArgsType(
                kind,
                idx,
                is[idx].1,
                should[idx],
            ));
        }
    }
    Ok(())
}

#[must_use]
pub(crate) struct Vars(bool, bool);

impl Vars {
    pub fn new() -> Self {
        Self(true, true)
    }

    pub fn locals(&self) -> bool {
        self.0
    }

    pub fn globals(&self) -> bool {
        self.1
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
pub(crate) struct Prototype {
    pub args: ArgDefList,
    pub ret: Option<ValType>,
}

impl PathDef {
    pub(crate) fn prototype(&self) -> Prototype {
        Prototype {
            args: self.args.clone(),
            ret: None,
        }
    }

    pub(crate) fn semantic_check(&mut self, ctx: &mut CheckContext) -> Result<bool, TurtleError> {
        ctx.locals = self.args.iter().cloned().collect();
        let res = self
            .body
            .semantic_check_loop(ctx, FuncType::Path(self.name));
        ctx.locals.clear();
        res
    }

    pub(crate) fn collect_context(&self, ctx: &mut CheckContext) {
        ctx.protos.insert(self.name, self.prototype());
        self.body.collect_context(ctx);
    }
}

impl CalcDef {
    pub(crate) fn prototype(&self) -> Prototype {
        Prototype {
            args: self.args.clone(),
            ret: Some(self.ret_ty),
        }
    }

    pub(crate) fn semantic_check(&mut self, ctx: &mut CheckContext) -> Result<bool, TurtleError> {
        ctx.locals = self.args.iter().cloned().collect();
        let res = self
            .body
            .semantic_check_loop(ctx, FuncType::Calc(self.name));
        ctx.locals.clear();
        res
    }

    pub(crate) fn collect_context(&self, ctx: &mut CheckContext) {
        ctx.protos.insert(self.name, self.prototype());
        self.body.collect_context(ctx);
    }
}

impl Block {
    /// returns whether all global variables could be identified
    pub(crate) fn semantic_check_loop(
        &mut self,
        ctx: &mut CheckContext,
        func: FuncType,
    ) -> Result<bool, TurtleError> {
        loop {
            let prev = ctx.var_count();
            let res = self.semantic_check(ctx)?;
            if res.0 {
                return Ok(res.1);
            }
            if prev == ctx.var_count() {
                return Err(TurtleError::UndefLocals(
                    func,
                    collect_undef(self.collect_variables(), true),
                ));
            }
        }
    }

    /// returns whether all local and/or global variables could be identified
    pub(crate) fn semantic_check(&mut self, ctx: &mut CheckContext) -> Result<Vars, TurtleError> {
        self.statements
            .iter_mut()
            .try_fold(Vars::new(), |v, stmt| Ok(v & stmt.semantic_check(ctx)?))
    }

    pub(crate) fn collect_context(&self, ctx: &mut CheckContext) {
        for var in self.collect_variables() {
            match var {
                VariableKind::Global(id, ty) => _ = ctx.globals.insert(id, ty),
                VariableKind::Local(id, ty) => _ = ctx.locals.insert(id, ty),
                VariableKind::GlobalPreDef(_) => {}
            }
        }
    }

    pub(crate) fn collect_variables(&self) -> Vec<VariableKind> {
        self.statements
            .iter()
            .flat_map(|s| s.collect_variables())
            .collect()
    }
}

pub(crate) fn check_args(
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

impl ValType {
    pub(crate) fn assert(&self, expected: ValType) -> Result<(), TypeError> {
        if *self == expected {
            Ok(())
        } else {
            Err(TypeError::WrongType(*self, expected))
        }
    }
}

pub fn collect_undef(vars: Vec<VariableKind>, local: bool) -> Vec<usize> {
    vars.into_iter()
        .filter_map(|kind| match (local, kind) {
            (true, VariableKind::Local(id, ValType::Any)) => Some(id),
            (false, VariableKind::Global(id, ValType::Any)) => Some(id),
            _ => None,
        })
        .collect()
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("different types for operator {0}: {1} != {2}")]
    BiOpDifferentTypes(BiOperator, ValType, ValType),
    #[error("operator {0} not defined for {1}")]
    BiOpWrongType(BiOperator, ValType),
    #[error("operator {0} cannot result in {1}")]
    BiOpWrongResult(BiOperator, ValType),
    #[error("operator {0} for {1} doesn't result in {2}")]
    BiOpWrongTypeForResult(BiOperator, ValType, ValType),
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
