use std::collections::HashMap;

use crate::{features::FeatureConf, tokens::*, Identified, TurtleError};

use super::{CalcDef, PathDef, TProgram};

mod statements;
mod expr;
mod variable;
#[cfg(test)]
mod test;

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
        fn check_args(
            kind: EventKind,
            is: &[(usize, ValType)],
            should: &[ValType],
        ) -> Result<(), TurtleError> {
            if is.len() != should.len() {
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
        if let Some(evt) = &self.key_event {
            check_args(EventKind::Key, &evt.args, &[ValType::String])?;
        }
        if let Some(evt) = &self.mouse_event {
            check_args(
                EventKind::Mouse,
                &evt.args,
                &[ValType::Number, ValType::Number, ValType::Boolean],
            )?;
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
