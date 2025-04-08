use crate::{pos::Pos, tokens::{Statement, ValType}, TurtleError};

use super::{CheckContext, TypeError, Vars};

impl Pos<Statement> {
    pub(super) fn semantic_check(&mut self, ctx: &mut CheckContext) -> Result<Vars, TurtleError> {
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
            Statement::PathCall(id, exprs) | Statement::Split(id, exprs) => {
                super::check_args(exprs, &ctx.protos[&*id].clone().args, e_map, ctx)
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
                counter.expect_type(ValType::Number, ctx)?;
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