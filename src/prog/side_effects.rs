use crate::tokens::{Block, Expr, ExprKind, Statement, Variable, VariableKind};

use super::TProgram;

impl Block {
    pub fn side_effects(&self, prog: &TProgram, rec: &mut Vec<usize>) -> bool {
        self.statements
            .iter()
            .any(|stmt| stmt.side_effects(prog, rec))
    }
}

impl Statement {
    pub fn side_effects(&self, prog: &TProgram, rec: &mut Vec<usize>) -> bool {
        match self {
            Statement::MoveDist { .. }
            | Statement::MoveHome(_)
            | Statement::Turn { .. }
            | Statement::Direction(_)
            | Statement::Color(_, _, _)
            | Statement::Clear
            | Statement::Stop
            | Statement::Mark
            | Statement::MoveMark(_)
            | Statement::Print(_)
            | Statement::Split(_, _)
            | Statement::Wait
            | Statement::Finish
            | Statement::PathCall(_, _) => true,
            Statement::Store(val, var) | Statement::Calc { var, val, .. } => {
                var.side_effects() || val.side_effects(prog, rec)
            }
            Statement::IfBranch(expr, block)
            | Statement::DoLoop(expr, block)
            | Statement::WhileLoop(expr, block)
            | Statement::RepeatLoop(expr, block) => {
                expr.side_effects(prog, rec) || block.side_effects(prog, rec)
            }
            Statement::IfElseBranch(expr, if_br, else_br) => {
                expr.side_effects(prog, rec)
                    || if_br.side_effects(prog, rec)
                    || else_br.side_effects(prog, rec)
            }
            Statement::CounterLoop {
                counter,
                from,
                to,
                step,
                body,
                ..
            } => {
                counter.side_effects()
                    || from.side_effects(prog, rec)
                    || to.side_effects(prog, rec)
                    || step.as_ref().is_some_and(|e| e.side_effects(prog, rec))
                    || body.side_effects(prog, rec)
            }
        }
    }
}

impl Expr {
    pub fn side_effects(&self, prog: &TProgram, rec: &mut Vec<usize>) -> bool {
        match &***self {
            ExprKind::Const(_) | ExprKind::Variable(_) => false,
            ExprKind::BiOperation(lhs, _, rhs) => {
                lhs.side_effects(prog, rec) || rhs.side_effects(prog, rec)
            }
            ExprKind::UnOperation(_, expr)
            | ExprKind::Absolute(expr)
            | ExprKind::Bracket(expr)
            | ExprKind::Convert(expr, _) => expr.side_effects(prog, rec),
            ExprKind::FuncCall(_, exprs) => exprs.iter().any(|e| e.side_effects(prog, rec)),
            ExprKind::CalcCall(name, exprs) => {
                exprs.iter().any(|e| e.side_effects(prog, rec))
                    || prog.get_calc(*name).is_ok_and(|calc| {
                        rec.contains(&calc.name) || {
                            rec.push(calc.name);
                            calc.body.side_effects(prog, rec)
                        }
                    })
            }
        }
    }
}

impl Variable {
    pub fn side_effects(&self) -> bool {
        matches!(***self, VariableKind::Local(_, _))
    }
}
