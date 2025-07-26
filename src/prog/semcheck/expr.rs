use crate::{
    pos::{Positionable, Spanned},
    prog::Otherwise,
    tokens::{Expr, ExprKind, ValType, VariableKind},
};

use super::{CheckContext, TypeError, Vars};

impl Expr {
    pub(crate) fn expect_type(
        &mut self,
        ty: ValType,
        ctx: &mut CheckContext,
    ) -> Result<Vars, Vec<Spanned<TypeError>>> {
        let span = self.get_span();
        let e_map = |e: TypeError| e.with_span(span);
        let mut vars = Vars::new();
        let mut errs = Vec::new();
        macro_rules! add_res {
            ($res:expr) => {
                match $res {
                    Ok(v) => vars &= v,
                    Err(mut e) => errs.append(&mut e),
                }
            };
        }
        match &mut ***self {
            ExprKind::Const(value) => errs.extend(value.val_type().assert(ty).err().map(e_map)),
            ExprKind::Variable(var) => errs.extend(var.expect_type(ty, ctx).err()),
            ExprKind::BiOperation(lhs, op, rhs) => {
                let types = op.types();
                let poss_types: Vec<_> = types
                    .iter()
                    .cloned()
                    .filter_map(|(inp, out)| (out == ty).then_some(inp))
                    .collect();
                let fits = |t: ValType| {
                    if t == ValType::Any || poss_types.contains(&t) {
                        Ok(())
                    } else if types.iter().any(|(inp, _)| *inp == t) {
                        Err(e_map(TypeError::BiOpWrongTypeForResult(*op, t, ty)))
                    } else {
                        Err(e_map(TypeError::BiOpWrongType(*op, t)))
                    }
                };
                match poss_types.len() {
                    0 => errs.push(e_map(TypeError::BiOpWrongResult(*op, ty))),
                    1 => {
                        add_res!(lhs.expect_type(poss_types[0], ctx));
                        add_res!(rhs.expect_type(poss_types[0], ctx));
                    }
                    _ => match (lhs.val_type(ctx), rhs.val_type(ctx)) {
                        (Err(mut lhs), Err(mut rhs)) => {
                            errs.append(&mut lhs);
                            errs.append(&mut rhs);
                        }
                        (Err(mut either), Ok(_)) | (Ok(_), Err(mut either)) => {
                            errs.append(&mut either);
                        }
                        (Ok(lhs_res), Ok(rhs_res)) => {
                            match (lhs_res.0, rhs_res.0) {
                                (ValType::Any, ValType::Any) => vars = lhs_res.1 & rhs_res.1,
                                (ValType::Any, rhs_ty) => {
                                    add_res!(lhs.expect_type(rhs_ty, ctx));
                                    vars &= rhs_res.1;
                                }
                                (lhs_ty, ValType::Any) => {
                                    add_res!(rhs.expect_type(lhs_ty, ctx));
                                    vars &= lhs_res.1;
                                }
                                (lhs_ty, rhs_ty) if lhs_ty == rhs_ty => {
                                    vars = lhs_res.1 & rhs_res.1
                                }
                                (lhs_ty, rhs_ty) => errs.push(e_map(
                                    TypeError::BiOpDifferentTypes(*op, lhs_ty, rhs_ty),
                                )),
                            };
                            errs.extend(fits(lhs_res.0).err());
                            errs.extend(fits(rhs_res.0).err());
                        }
                    },
                }
            }
            ExprKind::UnOperation(op, expr) => {
                if op.val_type() != ty {
                    errs.push(e_map(TypeError::UnOpWrongType(*op, ty)));
                }
                add_res!(expr.expect_type(ty, ctx));
            }
            ExprKind::Absolute(expr) => {
                if ty != ValType::Number {
                    errs.push(e_map(TypeError::AbsoluteValue(ty)));
                }
                add_res!(expr.expect_type(ty, ctx));
            }
            ExprKind::Bracket(expr) => add_res!(expr.expect_type(ty, ctx)),
            ExprKind::Convert(expr, vt) => {
                errs.extend(vt.assert(ty).err().map(e_map));
                match expr.val_type(ctx) {
                    Ok((_, v)) => vars &= v,
                    Err(mut e) => errs.append(&mut e),
                }
            }
            ExprKind::FuncCall(pdf, exprs) => {
                errs.extend(pdf.ret_type().assert(ty).err().map(e_map));
                let fargs = pdf.args();
                if fargs.len() != exprs.len() {
                    errs.push(e_map(TypeError::ArgsWrongLength(fargs.len(), exprs.len())));
                }
                for i in 0..fargs.len().max(exprs.len()) {
                    add_res!(exprs[i].expect_type(fargs[i], ctx));
                }
            }
            ExprKind::CalcCall(idx, exprs) => {
                let proto = ctx.protos[&*idx].clone();
                errs.extend(
                    proto
                        .ret
                        .expect("calc should have ret type")
                        .assert(ty)
                        .err()
                        .map(e_map),
                );
                add_res!(super::check_args(exprs, &proto.args, e_map, ctx));
            }
        }
        errs.otherwise(vars)
    }

    pub(crate) fn val_type(
        &mut self,
        ctx: &mut CheckContext,
    ) -> Result<(ValType, Vars), Vec<Spanned<TypeError>>> {
        let span = self.get_span();
        let e_map = |e: TypeError| e.with_span(span);
        let mut res_ty = ValType::Any;
        let mut vars = Vars::new();
        let mut errs = Vec::new();
        macro_rules! add_res {
            ($res:expr) => {
                match $res {
                    Ok(v) => vars &= v,
                    Err(mut e) => errs.append(&mut e),
                }
            };
        }
        match &mut ***self {
            ExprKind::Const(val) => res_ty = val.val_type(),
            ExprKind::Variable(var) => return var.val_type(ctx).map_err(|e| vec![e]),
            ExprKind::BiOperation(lhs, op, rhs) => {
                let types = op.types();
                if types.len() == 1 {
                    add_res!(lhs.expect_type(types[0].0, ctx));
                    add_res!(rhs.expect_type(types[0].0, ctx));
                    res_ty = types[0].1;
                } else {
                    match (lhs.val_type(ctx), rhs.val_type(ctx)) {
                        (Err(mut lhs), Err(mut rhs)) => {
                            errs.append(&mut lhs);
                            errs.append(&mut rhs);
                        }
                        (Err(mut either), Ok(_)) | (Ok(_), Err(mut either)) => {
                            errs.append(&mut either);
                        }
                        (Ok(lhs_res), Ok(rhs_res)) => {
                            if lhs_res.0 != ValType::Any
                                && !types.iter().any(|(inp, _)| *inp == lhs_res.0)
                            {
                                errs.push(e_map(TypeError::BiOpWrongType(*op, lhs_res.0)));
                            }
                            if rhs_res.0 != ValType::Any
                                && !types.iter().any(|(inp, _)| *inp == rhs_res.0)
                            {
                                errs.push(e_map(TypeError::BiOpWrongType(*op, rhs_res.0)));
                            }
                            match (lhs_res.0, rhs_res.0) {
                                (ValType::Any, ValType::Any) => vars = lhs_res.1 & rhs_res.1,
                                (ValType::Any, rhs_ty) => {
                                    vars = rhs_res.1;
                                    add_res!(lhs.expect_type(rhs_ty, ctx));
                                }
                                (lhs_ty, ValType::Any) => {
                                    vars = lhs_res.1;
                                    add_res!(rhs.expect_type(lhs_ty, ctx));
                                }
                                (lhs_ty, rhs_ty) if lhs_ty == rhs_ty => {
                                    vars = lhs_res.1 & rhs_res.1
                                }
                                (lhs_ty, rhs_ty) => {
                                    errs.push(e_map(TypeError::BiOpDifferentTypes(
                                        *op, lhs_ty, rhs_ty,
                                    )));
                                }
                            }
                        }
                    }
                }
            }
            ExprKind::UnOperation(op, expr) => {
                res_ty = op.val_type();
                add_res!(expr.expect_type(res_ty, ctx));
            }
            ExprKind::Absolute(expr) => {
                res_ty = ValType::Number;
                add_res!(expr.expect_type(ValType::Number, ctx));
            }
            ExprKind::Bracket(expr) => return expr.val_type(ctx),
            ExprKind::Convert(from, to) => {
                res_ty = *to;
                add_res!(from.val_type(ctx).map(|(_, vars)| vars));
            }
            ExprKind::FuncCall(pdf, args) => {
                res_ty = pdf.ret_type();
                let fargs = pdf.args();
                if fargs.len() != args.len() {
                    errs.push(e_map(TypeError::ArgsWrongLength(args.len(), fargs.len())));
                }
                for (expr, arg) in args.iter_mut().zip(fargs) {
                    add_res!(expr.expect_type(arg, ctx));
                }
            }
            ExprKind::CalcCall(idx, exprs) => {
                let proto = ctx.protos[&*idx].clone();
                add_res!(super::check_args(exprs, &proto.args, e_map, ctx));
                res_ty = proto.ret.expect("calc should have return type");
            }
        }
        errs.otherwise((res_ty, vars))
    }

    pub(crate) fn collect_variables(&self) -> Vec<VariableKind> {
        match &***self {
            ExprKind::Const(_) => Vec::new(),
            ExprKind::Variable(var) => vec![***var],
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
