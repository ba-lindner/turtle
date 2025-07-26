use std::collections::HashMap;

use crate::{
    Identified, ProgError, TurtleError,
    debugger::FuncType,
    features::FeatureConf,
    pos::{Positionable, Span, Spanned},
    prog::Otherwise,
    tokens::*,
};

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

enum Check {
    Main,
    Path(usize),
    Calc(usize),
    KeyEvent,
    MouseEvent,
}

impl Check {
    fn exec(
        &self,
        prog: &mut TProgram,
        ctx: &mut CheckContext,
    ) -> Result<bool, Vec<Spanned<TypeError>>> {
        match self {
            Check::Main => prog.main.semantic_check_loop(ctx, FuncType::Main),
            Check::Path(i) => prog.paths[*i].semantic_check(ctx),
            Check::Calc(i) => prog.calcs[*i].semantic_check(ctx),
            Check::KeyEvent => {
                if let Some(evt) = &mut prog.key_event {
                    evt.semantic_check(ctx)
                } else {
                    Ok(true)
                }
            }
            Check::MouseEvent => {
                if let Some(evt) = &mut prog.mouse_event {
                    evt.semantic_check(ctx)
                } else {
                    Ok(true)
                }
            }
        }
    }

    fn global_vars(&self, prog: &TProgram) -> Vec<usize> {
        match self {
            Check::Main => prog.main.global_vars(),
            Check::Path(i) => prog.paths[*i].body.global_vars(),
            Check::Calc(i) => prog.calcs[*i].body.global_vars(),
            Check::KeyEvent => {
                if let Some(evt) = &prog.key_event {
                    evt.body.global_vars()
                } else {
                    Vec::new()
                }
            }
            Check::MouseEvent => {
                if let Some(evt) = &prog.mouse_event {
                    evt.body.global_vars()
                } else {
                    Vec::new()
                }
            }
        }
    }
}

impl TProgram {
    pub(crate) fn semantic_check(&mut self) -> Result<(), TurtleError> {
        let mut errs = self.check_idents();
        errs.append(&mut self.check_event_args());
        if !errs.is_empty() {
            return Err(TurtleError::ProgErrors(errs));
        }
        let mut errs = Vec::new();
        let calc_proto = self.calcs.iter().map(|c| (c.name, c.prototype()));
        let path_proto = self.paths.iter().map(|p| (p.name, p.prototype()));
        let mut ctx = CheckContext {
            features: self.features,
            protos: calc_proto.chain(path_proto).collect(),
            globals: HashMap::new(),
            locals: HashMap::new(),
        };
        for param in &mut self.params {
            ctx.globals.insert(param.name, param.value.val_type());
        }
        let mut checks: Vec<Check> = vec![Check::Main, Check::KeyEvent, Check::MouseEvent];
        for i in 0..self.calcs.len() {
            checks.push(Check::Calc(i));
        }
        for i in 0..self.paths.len() {
            checks.push(Check::Path(i));
        }
        let mut excluded = Vec::new();
        while !checks.is_empty() {
            let prev_defined = ctx.var_count();
            let prev_checks = checks.len();
            let mut unfinished = Vec::new();
            for check in checks {
                match check.exec(self, &mut ctx) {
                    Ok(true) => {}
                    Ok(false) => unfinished.push(check),
                    Err(mut new) => {
                        errs.append(&mut new);
                        excluded.append(&mut check.global_vars(&self));
                    }
                }
            }
            if ctx.var_count() == prev_defined && unfinished.len() == prev_checks {
                break;
            }
            checks = unfinished;
        }
        for (idx, (_, ty)) in self.symbols.iter().enumerate() {
            if *ty == Identified::GlobalVar
                && ctx.globals.get(&idx).is_none_or(|t| *t == ValType::Any)
            {
                errs.push(TypeError::UndefGlobal(idx).with_span(Span::default()));
            }
        }
        errs.otherwise(()).map_err(TurtleError::TypeErrors)
    }

    fn check_idents(&self) -> Vec<ProgError> {
        let mut errs = Vec::new();
        for (id, (_, kind)) in self.symbols.iter().enumerate() {
            match kind {
                Identified::Unknown => {
                    errs.push(ProgError::UnidentifiedIdentifier(id));
                }
                Identified::Path => {
                    errs.extend(self.get_path(id).err());
                }
                Identified::Calc => {
                    errs.extend(self.get_calc(id).err());
                }
                _ => {}
            }
        }
        for idx in 0..self.paths.len() - 1 {
            let name = self.paths[idx].name;
            let dups: Vec<_> = self.paths[idx..]
                .iter()
                .filter(|p| p.name == name)
                .map(|p| p.body.begin)
                .collect();
            if dups.len() > 1 {
                errs.push(ProgError::MultipleFuncs(FuncType::Path(name), dups));
            }
        }
        for idx in 0..self.calcs.len() - 1 {
            let name = self.calcs[idx].name;
            let dups: Vec<_> = self.calcs[idx..]
                .iter()
                .filter(|c| c.name == name)
                .map(|c| c.body.begin)
                .collect();
            if dups.len() > 1 {
                errs.push(ProgError::MultipleFuncs(FuncType::Calc(name), dups));
            }
        }
        errs
    }

    pub(crate) fn check_event_args(&self) -> Vec<ProgError> {
        let mut errs = Vec::new();
        if let Some(evt) = &self.key_event {
            errs.extend(compare_args(EventKind::Key, &evt.args, &[ValType::String]).err());
        }
        if let Some(evt) = &self.mouse_event {
            errs.extend(
                compare_args(
                    EventKind::Mouse,
                    &evt.args,
                    &[ValType::Number, ValType::Number, ValType::Boolean],
                )
                .err(),
            );
        }
        errs
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
) -> Result<(), ProgError> {
    if is.len() > should.len() {
        return Err(ProgError::EventArgsLength(kind, is.len(), should.len()));
    }
    for idx in 0..is.len() {
        if is[idx].1 != should[idx] {
            return Err(ProgError::EventArgsType(kind, idx, is[idx].1, should[idx]));
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

    pub(crate) fn semantic_check(
        &mut self,
        ctx: &mut CheckContext,
    ) -> Result<bool, Vec<Spanned<TypeError>>> {
        ctx.locals = self.args.iter().copied().collect();
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

    pub(crate) fn semantic_check(
        &mut self,
        ctx: &mut CheckContext,
    ) -> Result<bool, Vec<Spanned<TypeError>>> {
        ctx.locals = self.args.iter().copied().collect();
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
    ) -> Result<bool, Vec<Spanned<TypeError>>> {
        loop {
            let prev = ctx.var_count();
            let res = self.check_statements(ctx)?;
            if res.0 {
                return Ok(res.1);
            }
            if prev == ctx.var_count() {
                return Err(self
                    .collect_variables()
                    .filter_map(|v| {
                        if let VariableKind::Local(id, ValType::Any) = v {
                            Some(TypeError::UndefLocal(func, id).with_span(Span::default()))
                        } else {
                            None
                        }
                    })
                    .collect());
            }
        }
    }

    /// returns whether all local and/or global variables could be identified
    pub(crate) fn check_statements(
        &mut self,
        ctx: &mut CheckContext,
    ) -> Result<Vars, Vec<Spanned<TypeError>>> {
        let mut res = Vars::new();
        let mut errs = Vec::new();
        for stmt in &mut self.statements {
            match stmt.semantic_check(ctx) {
                Ok(v) => res &= v,
                Err(mut new) => errs.append(&mut new),
            }
        }
        errs.otherwise(res)
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

    pub(crate) fn collect_variables(&self) -> impl Iterator<Item = VariableKind> {
        self.statements.iter().flat_map(|s| s.collect_variables())
    }

    fn global_vars(&self) -> Vec<usize> {
        self.collect_variables()
            .filter_map(|v| {
                if let VariableKind::Global(id, _) = v {
                    Some(id)
                } else {
                    None
                }
            })
            .collect()
    }
}

pub(crate) fn check_args(
    exprs: &mut [Expr],
    args: &[(usize, ValType)],
    e_map: impl Fn(TypeError) -> Spanned<TypeError>,
    ctx: &mut CheckContext,
) -> Result<Vars, Vec<Spanned<TypeError>>> {
    if exprs.len() != args.len() {
        return Err(vec![e_map(TypeError::ArgsWrongLength(
            exprs.len(),
            args.len(),
        ))]);
    }
    let mut res = Vars::new();
    let mut errs = Vec::new();
    for (e, (_, a)) in exprs.iter_mut().zip(args.iter()) {
        match e.expect_type(*a, ctx) {
            Ok(v) => res &= v,
            Err(mut why) => errs.append(&mut why),
        }
    }
    errs.otherwise(res)
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
    #[error("type could not be inferred for global variable #{0}")]
    UndefGlobal(usize),
    #[error("type could not be inferred for local variable #{1} in {0}")]
    UndefLocal(FuncType, usize),
}
