use crate::{
    tokens::{Expr, ExprKind, ValType, VariableKind},
    TurtleError,
};

use super::{CheckContext, TypeError, Vars};

impl Expr {
    pub(crate) fn expect_type(
        &mut self,
        ty: ValType,
        ctx: &mut CheckContext,
    ) -> Result<Vars, TurtleError> {
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
                match poss_types.len() {
                    0 => Err(e_map(TypeError::BiOpWrongResult(*op, ty))),
                    1 => Ok(lhs.expect_type(poss_types[0], ctx)?
                        & rhs.expect_type(poss_types[0], ctx)?),
                    _ => {
                        let lhs_ty = lhs.val_type(ctx)?;
                        if lhs_ty.0 != ValType::Any {
                            return if poss_types.contains(&lhs_ty.0) {
                                Ok(lhs_ty.1 & rhs.expect_type(lhs_ty.0, ctx)?)
                            } else if op.types().iter().any(|(inp, _)| *inp == lhs_ty.0) {
                                Err(e_map(TypeError::BiOpWrongTypeForResult(*op, lhs_ty.0, ty)))
                            } else {
                                Err(e_map(TypeError::BiOpWrongType(*op, lhs_ty.0)))
                            };
                        }
                        let rhs_ty = rhs.val_type(ctx)?;
                        if rhs_ty.0 != ValType::Any {
                            return if poss_types.contains(&rhs_ty.0) {
                                Ok(lhs.expect_type(rhs_ty.0, ctx)? & rhs_ty.1)
                            } else if op.types().iter().any(|(inp, _)| *inp == rhs_ty.0) {
                                Err(e_map(TypeError::BiOpWrongTypeForResult(*op, rhs_ty.0, ty)))
                            } else {
                                Err(e_map(TypeError::BiOpWrongType(*op, rhs_ty.0)))
                            };
                        }
                        Ok(lhs_ty.1 & rhs_ty.1)
                    }
                }
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
                super::check_args(exprs, &proto.args, e_map, ctx)
            }
        }
    }

    pub(crate) fn val_type(
        &mut self,
        ctx: &mut CheckContext,
    ) -> Result<(ValType, Vars), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeErrorSpan(e, self.start, self.end);
        match &mut self.kind {
            ExprKind::Const(val) => Ok((val.val_type(), Vars::new())),
            ExprKind::Variable(var) => var.val_type(ctx),
            ExprKind::BiOperation(lhs, op, rhs) => {
                let types = op.types();
                if types.len() == 1 {
                    Ok((
                        types[0].1,
                        lhs.expect_type(types[0].0, ctx)? & rhs.expect_type(types[0].0, ctx)?,
                    ))
                } else {
                    let lhs_ty = lhs.val_type(ctx)?;
                    if lhs_ty.0 != ValType::Any {
                        return if types.iter().any(|(inp, _)| *inp == lhs_ty.0) {
                            Ok((lhs_ty.0, lhs_ty.1 & rhs.expect_type(lhs_ty.0, ctx)?))
                        } else {
                            Err(e_map(TypeError::BiOpWrongType(*op, lhs_ty.0)))
                        };
                    }
                    let rhs_ty = rhs.val_type(ctx)?;
                    if rhs_ty.0 != ValType::Any {
                        return if types.iter().any(|(inp, _)| *inp == rhs_ty.0) {
                            Ok((rhs_ty.0, lhs.expect_type(rhs_ty.0, ctx)? & rhs_ty.1))
                        } else {
                            Err(e_map(TypeError::BiOpWrongType(*op, rhs_ty.0)))
                        };
                    }
                    Ok((ValType::Any, lhs_ty.1 & rhs_ty.1))
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
                    super::check_args(exprs, &proto.args, e_map, ctx)?,
                ))
            }
        }
    }

    pub(crate) fn collect_variables(&self) -> Vec<VariableKind> {
        match &self.kind {
            ExprKind::Const(_) => Vec::new(),
            ExprKind::Variable(var) => vec![var.kind],
            ExprKind::BiOperation(lhs, _, rhs) => {
                let mut res = lhs.collect_variables();
                res.append(&mut rhs.collect_variables());
                res
            }
            ExprKind::UnOperation(_, expr)
            | ExprKind::Absolute(expr)
            | ExprKind::Bracket(expr)
            | ExprKind::Convert(expr, _) => expr.collect_variables(),
            ExprKind::FuncCall(_, exprs) | ExprKind::CalcCall(_, exprs) => {
                exprs.iter().flat_map(|e| e.collect_variables()).collect()
            }
        }
    }
}
