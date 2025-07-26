use crate::{
    pos::{Positionable, Spanned},
    prog::Otherwise,
    tokens::{Expr, Statement, ValType, VariableKind},
};

use super::{CheckContext, TypeError, Vars};

impl Spanned<Statement> {
    pub(crate) fn semantic_check(
        &mut self,
        ctx: &mut CheckContext,
    ) -> Result<Vars, Vec<Spanned<TypeError>>> {
        let span = self.get_span();
        let e_map = |e: TypeError| e.with_span(span);
        let mut errs = Vec::new();
        let mut vars = Vars::new();
        macro_rules! add_res {
            ($res:expr) => {
                match $res {
                    Ok(v) => vars &= v,
                    Err(mut e) => errs.append(&mut e),
                }
            };
        }
        macro_rules! var_expr {
            ($ve:expr, $ee:expr => $vo:ident, $eo:ident {$($t:tt)*}) => {
                match ($ve, $ee) {
                    (Err(one), Err(mut many)) => {
                        errs.push(one);
                        errs.append(&mut many);
                    }
                    (Err(one), Ok(_)) => {
                        errs.push(one);
                    }
                    (Ok(_), Err(mut many)) => {
                        errs.append(&mut many);
                    }
                    (Ok($vo), Ok($eo)) => {$($t)*}
                }
            };
        }
        match &mut **self {
            Statement::MoveDist { dist: expr, .. }
            | Statement::Turn { by: expr, .. }
            | Statement::Direction(expr) => add_res!(expr.expect_type(ValType::Number, ctx)),
            Statement::Color(r, g, b) => {
                add_res!(r.expect_type(ValType::Number, ctx));
                add_res!(g.expect_type(ValType::Number, ctx));
                add_res!(b.expect_type(ValType::Number, ctx));
            }
            Statement::MoveHome(_)
            | Statement::Clear
            | Statement::Stop
            | Statement::Finish
            | Statement::Mark
            | Statement::Wait
            | Statement::MoveMark(_) => {}
            Statement::PathCall(id, exprs) | Statement::Split(id, exprs) => {
                let proto = &ctx.protos[&*id];
                let args = proto.args.clone();
                add_res!(super::check_args(exprs, &args, e_map, ctx));
            }
            Statement::Store(expr, var) => var_expr! {
                var.val_type(ctx), expr.val_type(ctx) => var_res, expr_res {
                    match (var_res.0, expr_res.0) {
                        (ValType::Any, ValType::Any) => {
                            vars &= var_res.1;
                            vars &= expr_res.1;
                        }
                        (ValType::Any, expr_ty) => {
                            errs.extend(var.expect_type(expr_ty, ctx).err());
                            vars &= expr_res.1;
                        }
                        (var_ty, ValType::Any) => add_res!(expr.expect_type(var_ty, ctx)),
                        (var_ty, expr_ty) => {
                            vars &= var_res.1;
                            vars &= expr_res.1;
                            if var_ty != expr_ty {
                                errs.push(e_map(TypeError::WrongType(expr_ty, var_ty)));
                            }
                        }
                    }
                }
            },
            Statement::Calc { var, val, op } => {
                let types = op.types();
                if types.len() == 1 {
                    add_res!(val.expect_type(types[0].0, ctx));
                    errs.extend(var.expect_type(types[0].1, ctx).err());
                } else {
                    var_expr!(
                        var.val_type(ctx), val.val_type(ctx) => var_res, expr_res {
                            if expr_res.0 != ValType::Any {
                                if let Some((_, vt)) = types.iter().find(|(t, _)| *t == expr_res.0) {
                                    vars &= expr_res.1;
                                    if var_res.0 == ValType::Any {
                                        errs.extend(var.expect_type(*vt, ctx).err());
                                    } else if var_res.0 != *vt {
                                        errs.push(e_map(TypeError::BiOpWrongTypeForResult(*op, expr_res.0, var_res.0)));
                                    }
                                } else {
                                    errs.push(e_map(TypeError::BiOpWrongType(*op, expr_res.0)));
                                }
                            } else if var_res.0 != ValType::Any {
                                let rem: Vec<_> = types.iter().filter(|(_, out)| *out == var_res.0).copied().collect();
                                match rem.len() {
                                    0 => errs.push(e_map(TypeError::BiOpWrongResult(*op, var_res.0))),
                                    1 => add_res!(val.expect_type(rem[0].0, ctx)),
                                    _ => {
                                        vars &= expr_res.1;
                                    }
                                }
                            } else {
                                vars &= expr_res.1;
                                let first = types[0].1;
                                if types.iter().all(|(_, out)| *out == first) {
                                    errs.extend(var.expect_type(first, ctx).err());
                                } else {
                                    vars &= var_res.1;
                                }
                            }
                        }
                    );
                }
            }
            Statement::Print(expr) => add_res!(expr.expect_type(ValType::String, ctx)),
            Statement::IfBranch(expr, block)
            | Statement::WhileLoop(expr, block)
            | Statement::RepeatLoop(expr, block) => {
                add_res!(expr.expect_type(ValType::Boolean, ctx));
                add_res!(block.check_statements(ctx));
            }
            Statement::DoLoop(expr, block) => {
                add_res!(expr.expect_type(ValType::Number, ctx));
                add_res!(block.check_statements(ctx));
            }
            Statement::IfElseBranch(expr, if_block, else_block) => {
                add_res!(expr.expect_type(ValType::Boolean, ctx));
                add_res!(if_block.check_statements(ctx));
                add_res!(else_block.check_statements(ctx));
            }
            Statement::CounterLoop {
                counter,
                from,
                up: _,
                to,
                step,
                body,
            } => {
                if let Err(why) = counter.expect_type(ValType::Number, ctx) {
                    errs.push(why);
                }
                add_res!(from.expect_type(ValType::Number, ctx));
                add_res!(to.expect_type(ValType::Number, ctx));
                if let Some(step) = step {
                    add_res!(step.expect_type(ValType::Number, ctx));
                }
                add_res!(body.check_statements(ctx));
            }
        }
        errs.otherwise(vars)
    }

    pub(crate) fn collect_variables(&self) -> Vec<VariableKind> {
        match &**self {
            Statement::MoveDist { dist: expr, .. }
            | Statement::Turn { by: expr, .. }
            | Statement::Direction(expr)
            | Statement::Print(expr) => expr.collect_variables(),
            Statement::MoveHome(_)
            | Statement::Clear
            | Statement::Stop
            | Statement::Finish
            | Statement::Mark
            | Statement::MoveMark(_)
            | Statement::Wait => Vec::new(),
            Statement::Color(r, g, b) => {
                let mut res = r.collect_variables();
                res.append(&mut g.collect_variables());
                res.append(&mut b.collect_variables());
                res
            }
            Statement::PathCall(_, exprs) | Statement::Split(_, exprs) => {
                exprs.iter().flat_map(|e| e.collect_variables()).collect()
            }
            Statement::Store(val, var) | Statement::Calc { var, val, .. } => {
                let mut res = val.collect_variables();
                res.push(***var);
                res
            }
            Statement::IfBranch(expr, block)
            | Statement::DoLoop(expr, block)
            | Statement::WhileLoop(expr, block)
            | Statement::RepeatLoop(expr, block) => block
                .collect_variables()
                .chain(expr.collect_variables())
                .collect(),
            Statement::IfElseBranch(expr, if_block, else_block) => if_block
                .collect_variables()
                .chain(else_block.collect_variables())
                .chain(expr.collect_variables())
                .collect(),
            Statement::CounterLoop {
                counter,
                from,
                to,
                step,
                body,
                ..
            } => body
                .collect_variables()
                .chain(Some(***counter))
                .chain(from.collect_variables())
                .chain(to.collect_variables())
                .chain(
                    step.as_ref()
                        .map(Expr::collect_variables)
                        .unwrap_or_default(),
                )
                .collect(),
        }
    }
}
