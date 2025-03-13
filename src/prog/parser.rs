use std::fmt::Display;

use crate::{
    prog::{lexer::LexToken, CalcDef, PathDef},
    tokens::*,
    FilePos, Identified, Pos, Positionable, SymbolTable,
};

mod statements;
#[cfg(test)]
mod test;

/// Result of parsing a specific node
type PRes<T> = Result<T, Pos<ParseError>>;

/// Turtle Parser
pub struct Parser<'a> {
    ltokens: Vec<Pos<LexToken>>,
    pos: usize,
    symbols: &'a mut SymbolTable,
}

impl<'a> Parser<'a> {
    pub fn new(symbols: &'a mut SymbolTable, ltokens: Vec<Pos<LexToken>>) -> Self {
        Self {
            ltokens,
            pos: 0,
            symbols,
        }
    }

    pub fn parse_next(&mut self) -> Option<PRes<ParseToken>> {
        self.eof()?;
        let begin = self.curr_pos();
        Some(if self.match_keyword(Keyword::Path) {
            self.parse_path(begin)
        } else if self.match_keyword(Keyword::Calculation) {
            self.parse_calc(begin)
        } else if self.match_keyword(Keyword::Begin) {
            self.parse_main(begin)
        } else {
            Err(self.unexpected_token(TokenExpectation::BlockStart))
        })
    }

    fn lookahead(&self) -> Option<LexToken> {
        self.eof()?;
        Some((*self.ltokens[self.pos]).clone())
    }

    fn next_token(&mut self) -> Option<LexToken> {
        self.eof()?;
        self.pos += 1;
        Some((*self.ltokens[self.pos - 1]).clone())
    }

    fn match_keyword(&mut self, kw: Keyword) -> bool {
        match self.lookahead() {
            Some(LexToken::Keyword(found)) if found == kw => {
                self.pos += 1;
                true
            }
            _ => false,
        }
    }

    fn expect_keyword(&mut self, kw: Keyword) -> PRes<()> {
        if !self.match_keyword(kw) {
            Err(self.unexpected_token(TokenExpectation::ExactKeyword(kw)))
        } else {
            Ok(())
        }
    }

    fn match_identifier(&mut self) -> PRes<usize> {
        let Some(LexToken::Identifier(name)) = self.lookahead() else {
            return Err(self.unexpected_token(TokenExpectation::AnyIdentifier));
        };
        self.pos += 1;
        Ok(name)
    }

    fn match_symbol(&mut self, c: char) -> bool {
        match self.lookahead() {
            Some(LexToken::Symbol(found)) if found == c => {
                self.pos += 1;
                true
            }
            _ => false,
        }
    }

    fn expect_symbol(&mut self, c: char) -> PRes<()> {
        if !self.match_symbol(c) {
            Err(self.unexpected_token(TokenExpectation::ExactSymbol(c)))
        } else {
            Ok(())
        }
    }

    fn set_ident_type(&mut self, ident: usize, kind: Identified) -> PRes<()> {
        let pos = self.curr_pos();
        let old_kind = self
            .symbols
            .get_index_mut(ident)
            .expect("should be set by parser")
            .1;
        if *old_kind != Identified::Unknown && *old_kind != kind {
            Err(ParseError::ConflictingIdentifiers(ident, *old_kind, kind).attach_pos(pos))
        } else {
            *old_kind = kind;
            Ok(())
        }
    }

    fn parse_type(&mut self) -> PRes<ValType> {
        Ok(match self.next_token() {
            Some(LexToken::Keyword(Keyword::Num)) => ValType::Number,
            Some(LexToken::Keyword(Keyword::String)) => ValType::String,
            Some(LexToken::Keyword(Keyword::Bool)) => ValType::Boolean,
            _ => return Err(self.unexpected_token(TokenExpectation::ValType)),
        })
    }

    fn parse_path(&mut self, begin: FilePos) -> PRes<ParseToken> {
        let name = self.match_identifier()?;
        let mut args = Vec::new();
        if self.match_symbol('(') {
            while !self.match_symbol(')') {
                if !args.is_empty() {
                    self.expect_symbol(',')?;
                }
                let arg = self.match_identifier()?;
                self.expect_symbol(':')?;
                let ty = self.parse_type()?;
                args.push((arg, ty));
            }
        }
        self.set_ident_type(name, Identified::Path(args.len()))?;
        Ok(ParseToken::PathDef(PathDef {
            name,
            args,
            body: self.parse_statements(begin, Keyword::Endpath)?,
        }))
    }

    fn parse_calc(&mut self, begin: FilePos) -> PRes<ParseToken> {
        let name = self.match_identifier()?;
        let mut args = Vec::new();
        self.expect_symbol('(')?;
        while !self.match_symbol(')') {
            if !args.is_empty() {
                self.expect_symbol(',')?;
            }
            let arg = self.match_identifier()?;
            self.expect_symbol(':')?;
            let ty = self.parse_type()?;
            args.push((arg, ty));
        }
        self.expect_symbol(':')?;
        let ret_ty = self.parse_type()?;
        self.set_ident_type(name, Identified::Calc(args.len()))?;
        let stmts = self.parse_statements(begin, Keyword::Returns)?;
        let ret = self.parse_expr()?;
        self.expect_keyword(Keyword::Endcalc)?;
        Ok(ParseToken::CalcDef(CalcDef {
            name,
            args,
            ret_ty,
            body: stmts,
            ret,
        }))
    }

    fn parse_main(&mut self, begin: FilePos) -> PRes<ParseToken> {
        Ok(ParseToken::StartBlock(
            self.parse_statements(begin, Keyword::End)?,
        ))
    }

    fn parse_variable(&mut self) -> PRes<Variable> {
        let pos = self.curr_pos();
        let token = self
            .next_token()
            .ok_or(ParseError::UnexpectedEnd.attach_pos(self.curr_pos()))?;
        match token {
            LexToken::GlobalVar(id) => {
                self.set_ident_type(id, Identified::GlobalVar)?;
                Ok(Variable {
                    pos,
                    kind: VariableKind::Global(id, ValType::Any),
                })
            }
            LexToken::PredefVar(pdv) => Ok(Variable {
                pos,
                kind: VariableKind::GlobalPreDef(pdv),
            }),
            LexToken::Identifier(id) => {
                self.set_ident_type(id, Identified::LocalVar)?;
                Ok(Variable {
                    pos,
                    kind: VariableKind::Local(id, ValType::Any),
                })
            }
            t => Err(
                ParseError::UnexpectedToken(t, TokenExpectation::AnyIdentifier)
                    .attach_pos(self.curr_pos()),
            ),
        }
    }

    fn parse_expr(&mut self) -> PRes<Expr> {
        #[derive(Debug, PartialEq, Clone, Copy)]
        enum NodeKind {
            Start,
            Empty,
            Op(BiOperator),
        }

        enum Associativity {
            LeftToRight,
            RightToLeft,
        }

        const PRECEDENCE: [(&[BiOperator], Associativity); 7] = [
            (&[BiOperator::Exp], Associativity::RightToLeft),
            (
                &[BiOperator::Mul, BiOperator::Div],
                Associativity::LeftToRight,
            ),
            (
                &[BiOperator::Add, BiOperator::Sub],
                Associativity::LeftToRight,
            ),
            (
                &[
                    BiOperator::Less,
                    BiOperator::LessEqual,
                    BiOperator::Greater,
                    BiOperator::GreaterEqual,
                ],
                Associativity::LeftToRight,
            ),
            (
                &[BiOperator::Equal, BiOperator::UnEqual],
                Associativity::LeftToRight,
            ),
            (&[BiOperator::And], Associativity::LeftToRight),
            (&[BiOperator::Or], Associativity::LeftToRight),
        ];

        let base_expr = self.parse_operand()?;
        let mut nodes = vec![(NodeKind::Start, Some(base_expr))];
        while let Some(op) = self.match_operator() {
            nodes.push((NodeKind::Op(op), Some(self.parse_operand()?)));
        }
        for (ops, assoc) in PRECEDENCE {
            match assoc {
                Associativity::LeftToRight => {
                    for i in 0..nodes.len() - 1 {
                        if let NodeKind::Op(op) = nodes[i + 1].0 {
                            if ops.contains(&op) {
                                let lhs = nodes[i].1.take().unwrap();
                                let rhs = nodes[i + 1].1.take().unwrap();
                                nodes[i + 1].1 = Some(Expr {
                                    start: lhs.start,
                                    end: rhs.end,
                                    kind: ExprKind::BiOperation(Box::new(lhs), op, Box::new(rhs)),
                                });
                                nodes[i + 1].0 = nodes[i].0;
                                nodes[i].0 = NodeKind::Empty;
                            }
                        }
                    }
                }
                Associativity::RightToLeft => {
                    for i in (1..nodes.len()).rev() {
                        if let NodeKind::Op(op) = nodes[i].0 {
                            if ops.contains(&op) {
                                let lhs = nodes[i - 1].1.take().unwrap();
                                let rhs = nodes[i].1.take().unwrap();
                                nodes[i - 1].1 = Some(Expr {
                                    start: lhs.start,
                                    end: rhs.end,
                                    kind: ExprKind::BiOperation(Box::new(lhs), op, Box::new(rhs)),
                                });
                                nodes[i].0 = NodeKind::Empty;
                            }
                        }
                    }
                }
            }
            nodes.retain(|(op, _)| *op != NodeKind::Empty);
        }
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].0, NodeKind::Start);
        Ok(nodes[0].1.take().unwrap())
    }

    fn parse_operand(&mut self) -> PRes<Expr> {
        let start = self.curr_pos();
        Ok(
            match self
                .next_token()
                .ok_or(ParseError::UnexpectedEnd.attach_pos(start))?
            {
                LexToken::Symbol('(') => {
                    let expr = self.parse_expr()?;
                    self.expect_symbol(')')?;
                    ExprKind::Bracket(Box::new(expr)).at(start, self.last_pos())
                }
                LexToken::Symbol('|') => {
                    let expr = self.parse_expr()?;
                    self.expect_symbol('|')?;
                    ExprKind::Absolute(Box::new(expr)).at(start, self.last_pos())
                }
                LexToken::Symbol('-') => {
                    ExprKind::UnOperation(UnOperator::Negate, Box::new(self.parse_operand()?))
                        .at(start, self.last_pos())
                }
                LexToken::Keyword(Keyword::Not) => {
                    ExprKind::UnOperation(UnOperator::Not, Box::new(self.parse_operand()?))
                        .at(start, self.last_pos())
                }
                LexToken::IntLiteral(i) => {
                    ExprKind::Const(Value::Number(i as f64)).at(start, start)
                }
                LexToken::FloatLiteral(f) => ExprKind::Const(Value::Number(f)).at(start, start),
                LexToken::StringLiteral(s) => ExprKind::Const(Value::String(s)).at(start, start),
                LexToken::Keyword(Keyword::True) => {
                    ExprKind::Const(Value::Boolean(true)).at(start, start)
                }
                LexToken::Keyword(Keyword::False) => {
                    ExprKind::Const(Value::Boolean(false)).at(start, start)
                }
                LexToken::GlobalVar(v) => {
                    ExprKind::Variable(VariableKind::Global(v, ValType::Any).at(start))
                        .at(start, start)
                }
                LexToken::PredefVar(pdv) => {
                    ExprKind::Variable(VariableKind::GlobalPreDef(pdv).at(start)).at(start, start)
                }
                LexToken::Keyword(kw) => {
                    if let Some(pdf) = PredefFunc::parse(kw) {
                        let args = self.parse_args(Some(pdf.args().len()))?;
                        ExprKind::FuncCall(pdf, args).at(start, self.last_pos())
                    } else if let Some(conv) = ValType::parse(kw) {
                        self.expect_symbol('(')?;
                        let expr = self.parse_expr()?;
                        self.expect_symbol(')')?;
                        ExprKind::Convert(Box::new(expr), conv).at(start, self.last_pos())
                    } else {
                        self.pos -= 1;
                        return Err(self.unexpected_token(TokenExpectation::PredefFunc));
                    }
                }
                LexToken::Identifier(id) => {
                    if self.lookahead() == Some(LexToken::Symbol('(')) {
                        let args = self.parse_args(None)?;
                        self.set_ident_type(id, Identified::Calc(args.len()))?;
                        ExprKind::CalcCall(id, args).at(start, self.last_pos())
                    } else {
                        self.set_ident_type(id, Identified::LocalVar)?;
                        ExprKind::Variable(VariableKind::Local(id, ValType::Any).at(start))
                            .at(start, start)
                    }
                }
                t => {
                    return Err(
                        ParseError::UnexpectedToken(t, TokenExpectation::Expr).attach_pos(start)
                    )
                }
            },
        )
    }

    fn parse_args(&mut self, count: Option<usize>) -> PRes<Vec<Expr>> {
        let pos = self.curr_pos();
        let mut args = Vec::new();
        self.expect_symbol('(')?;
        while !self.match_symbol(')') {
            if !args.is_empty() {
                self.expect_symbol(',')?;
            }
            let arg = self.parse_expr()?;
            args.push(arg);
        }
        if let Some(c) = count {
            if c != args.len() {
                return Err(ParseError::ArgCount(args.len(), c).attach_pos(pos));
            }
        }
        Ok(args)
    }

    fn match_operator(&mut self) -> Option<BiOperator> {
        Some(match self.next_token()? {
            LexToken::Symbol('+') => BiOperator::Add,
            LexToken::Symbol('-') => BiOperator::Sub,
            LexToken::Symbol('*') => BiOperator::Mul,
            LexToken::Symbol('/') => BiOperator::Div,
            LexToken::Symbol('^') => BiOperator::Exp,
            LexToken::Symbol('=') => BiOperator::Equal,
            LexToken::Symbol('<') => {
                if self.match_symbol('=') {
                    BiOperator::LessEqual
                } else if self.match_symbol('>') {
                    BiOperator::UnEqual
                } else {
                    BiOperator::Less
                }
            }
            LexToken::Symbol('>') => {
                if self.match_symbol('=') {
                    BiOperator::GreaterEqual
                } else {
                    BiOperator::Greater
                }
            }
            LexToken::Keyword(Keyword::And) => BiOperator::And,
            LexToken::Keyword(Keyword::Or) => BiOperator::Or,
            _ => {
                self.pos -= 1;
                return None;
            }
        })
    }

    fn eof(&self) -> Option<()> {
        (self.pos < self.ltokens.len()).then_some(())
    }

    fn curr_pos(&self) -> FilePos {
        self.ltokens[self.pos.clamp(0, self.ltokens.len() - 1)].get_pos()
    }

    fn last_pos(&self) -> FilePos {
        self.ltokens[(self.pos - 1).clamp(0, self.ltokens.len() - 1)].get_pos()
    }

    fn unexpected_token(&mut self, expected: TokenExpectation) -> Pos<ParseError> {
        let pos = self.curr_pos();
        let Some(token) = self.next_token() else {
            return ParseError::UnexpectedEnd.attach_pos(pos);
        };
        ParseError::UnexpectedToken(token, expected).attach_pos(pos)
    }
}

impl Iterator for Parser<'_> {
    type Item = PRes<ParseToken>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_next()
    }
}

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum ParseError {
    #[error("unexpected token: got {0}, expected {1}")]
    UnexpectedToken(LexToken, TokenExpectation),
    #[error("unknown statement '{0}'")]
    UnknownStatement(Keyword),
    #[error("end of file reached")]
    UnexpectedEnd,
    #[error("conflicting use of identifier #{0}: was {1}, now used as {2}")]
    ConflictingIdentifiers(usize, Identified, Identified),
    #[error("attempted to modify readonly variable @{}", .0.get_str())]
    WriteToReadOnly(PredefVar),
    #[error("wrong number of arguments: got {0}, expected {1}")]
    ArgCount(usize, usize),
}

#[derive(Debug, PartialEq)]
pub enum TokenExpectation {
    Statement,
    BlockStart,
    ExactKeyword(Keyword),
    ExactSymbol(char),
    AnyIdentifier,
    Expr,
    PredefFunc,
    ValType,
}

impl Display for TokenExpectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenExpectation::Statement => write!(f, "begin of statement"),
            TokenExpectation::BlockStart => write!(f, "begin of function"),
            TokenExpectation::ExactKeyword(kw) => write!(f, "keyword `{kw}`"),
            TokenExpectation::ExactSymbol(c) => write!(f, "symbol `{c}`"),
            TokenExpectation::AnyIdentifier => write!(f, "identifier"),
            TokenExpectation::Expr => write!(f, "expression"),
            TokenExpectation::PredefFunc => write!(f, "predefined function or type"),
            TokenExpectation::ValType => write!(f, "type"),
        }
    }
}
