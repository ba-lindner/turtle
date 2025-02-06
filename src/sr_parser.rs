use crate::{lexer::LexToken, Cond, Expr, ParseToken, Pos, Positionable, Statement, Statements, Variable};

enum ASTNode {
    Lex(LexToken),
    Parse(ParseToken),
    Stmt(Statement),
    Stmts(Statements),
    Expr(Expr),
    Cond(Cond),
    Var(Variable),
}

pub struct Parser {
    inp: Vec<Pos<LexToken>>,
    stack: Vec<Pos<ASTNode>>,
}

impl Parser {
    fn merge_stmts(&mut self) {
        let Some(last) = self.stack.pop() else { return; };
        let Pos { token: ASTNode::Stmt(stmt), pos } = last else {
            self.stack.push(last);
            return;
        };
        if let Some(Pos { token: ASTNode::Stmts(stmts), .. }) = self.stack.last_mut() {
            stmts.push(stmt.attach_pos(pos));
        } else {
            self.stack.push(ASTNode::Stmts(vec![stmt.attach_pos(pos)]).attach_pos(pos));
        }
    }
}