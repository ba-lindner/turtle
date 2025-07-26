use crate::{
    pos::{Positionable, Spanned},
    tokens::{Block, Expr, ExprKind, Statement, Value},
};

impl Expr {
    pub fn is_const(&self) -> Option<Value> {
        if let ExprKind::Const(val) = &***self {
            Some(val.clone())
        } else {
            None
        }
    }

    pub fn const_fold(&mut self) {
        let mut kind = ExprKind::Const(Value::Boolean(false));
        std::mem::swap(&mut kind, &mut **self);
        *self = kind.const_fold().at(self.get_span());
    }
}

impl ExprKind {
    fn const_fold(self) -> ExprKind {
        match self {
            ExprKind::Const(_) | ExprKind::Variable(_) => self,
            ExprKind::BiOperation(mut lhs, op, mut rhs) => {
                lhs.const_fold();
                rhs.const_fold();
                if let (Some(lhs), Some(rhs)) = (lhs.is_const(), rhs.is_const()) {
                    ExprKind::Const(op.eval(&lhs, &rhs))
                } else {
                    ExprKind::BiOperation(lhs, op, rhs)
                }
            }
            ExprKind::UnOperation(op, mut expr) => {
                expr.const_fold();
                if let Some(val) = expr.is_const() {
                    ExprKind::Const(op.eval(&val))
                } else {
                    ExprKind::UnOperation(op, expr)
                }
            }
            ExprKind::Absolute(mut expr) => {
                expr.const_fold();
                if let Some(val) = expr.is_const() {
                    ExprKind::Const(val.num().abs().into())
                } else {
                    ExprKind::Absolute(expr)
                }
            }
            ExprKind::Bracket(mut expr) => {
                expr.const_fold();
                expr.0.into_inner()
            }
            ExprKind::Convert(mut expr, to) => {
                expr.const_fold();
                if let Some(val) = expr.is_const() {
                    ExprKind::Const(val.convert(to))
                } else {
                    ExprKind::Convert(expr, to)
                }
            }
            ExprKind::FuncCall(pdf, mut exprs) => {
                exprs.iter_mut().for_each(|e| e.const_fold());
                ExprKind::FuncCall(pdf, exprs)
            }
            ExprKind::CalcCall(id, mut exprs) => {
                exprs.iter_mut().for_each(|e| e.const_fold());
                ExprKind::CalcCall(id, exprs)
            }
        }
    }
}

impl Spanned<Statement> {
    pub fn const_fold(mut self) -> Vec<Spanned<Statement>> {
        match &mut *self {
            Statement::MoveDist { dist, .. } => dist.const_fold(),
            Statement::Turn { by, .. } => by.const_fold(),
            Statement::Calc { val, .. } => val.const_fold(),
            Statement::CounterLoop { from, to, step, .. } => {
                from.const_fold();
                to.const_fold();
                if let Some(s) = step {
                    s.const_fold();
                }
            }
            Statement::Direction(expr)
            | Statement::Store(expr, _)
            | Statement::Print(expr)
            | Statement::IfBranch(expr, _)
            | Statement::IfElseBranch(expr, _, _)
            | Statement::DoLoop(expr, _)
            | Statement::WhileLoop(expr, _)
            | Statement::RepeatLoop(expr, _) => expr.const_fold(),
            Statement::Color(r, g, b) => {
                r.const_fold();
                g.const_fold();
                b.const_fold();
            }
            Statement::PathCall(_, exprs) | Statement::Split(_, exprs) => {
                for e in exprs {
                    e.const_fold();
                }
            }
            Statement::MoveHome(_)
            | Statement::Clear
            | Statement::Stop
            | Statement::Finish
            | Statement::Mark
            | Statement::MoveMark(_)
            | Statement::Wait => {}
        };

        let to_bool = |v: Value| v.bool();
        let to_num = |v: Value| v.num();

        let span = self.get_span();
        match self.into_inner() {
            Statement::IfBranch(expr, block) => match expr.is_const().map(to_bool) {
                Some(true) => block.statements,
                Some(false) => Vec::new(),
                None => vec![Statement::IfBranch(expr, block).with_span(span)],
            },
            Statement::IfElseBranch(expr, if_block, else_block) => {
                match expr.is_const().map(to_bool) {
                    Some(true) => if_block.statements,
                    Some(false) => else_block.statements,
                    None => {
                        vec![Statement::IfElseBranch(expr, if_block, else_block).with_span(span)]
                    }
                }
            }
            Statement::DoLoop(expr, block) => match expr.is_const().map(to_num) {
                Some(c) if c < 1.0 => Vec::new(),
                Some(c) if c < 2.0 => block.statements,
                _ => vec![Statement::DoLoop(expr, block).with_span(span)],
            },
            Statement::CounterLoop {
                counter,
                from,
                up,
                to,
                step,
                body,
            } => {
                if let (Some(from), Some(to)) =
                    (from.is_const().map(to_num), to.is_const().map(to_num))
                    && from != to
                    && (from > to) == up
                {
                    return Vec::new();
                }
                vec![
                    Statement::CounterLoop {
                        counter,
                        from,
                        up,
                        to,
                        step,
                        body,
                    }
                    .with_span(span),
                ]
            }
            Statement::WhileLoop(expr, block) => {
                if expr.is_const().is_some_and(|v| !v.bool()) {
                    Vec::new()
                } else {
                    vec![Statement::WhileLoop(expr, block).with_span(span)]
                }
            }
            Statement::RepeatLoop(expr, block) => {
                if expr.is_const().is_some_and(|v| !v.bool()) {
                    block.statements
                } else {
                    vec![Statement::RepeatLoop(expr, block).with_span(span)]
                }
            }
            stmt => vec![stmt.with_span(span)],
        }
    }
}

impl Block {
    pub fn const_fold(&mut self) {
        let mut new_stmt = Vec::new();
        for stmt in self.statements.drain(..) {
            new_stmt.append(&mut stmt.const_fold());
        }
        self.statements = new_stmt;
    }
}
