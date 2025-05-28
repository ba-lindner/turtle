use std::fmt::Display;

use crate::{
    features::{Feature, FeatureConf, FeatureState},
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
pub struct Parser<'s, 'f> {
    ltokens: Vec<Pos<LexToken>>,
    pos: usize,
    symbols: &'s mut SymbolTable,
    features: &'f mut FeatureConf,
}

impl<'s, 'f> Parser<'s, 'f> {
    pub fn new(
        symbols: &'s mut SymbolTable,
        ltokens: Vec<Pos<LexToken>>,
        features: &'f mut FeatureConf,
    ) -> Self {
        Self {
            ltokens,
            pos: 0,
            symbols,
            features,
        }
    }

    pub fn reset(&mut self) {
        self.pos = 0;
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
        } else if self.match_keyword(Keyword::Event) {
            self.parse_event(begin)
        } else if self.match_keyword(Keyword::Param) {
            self.parse_param()
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

    fn next_token_err(&mut self) -> PRes<LexToken> {
        self.next_token()
            .ok_or(ParseError::UnexpectedEnd.attach_pos(self.curr_pos()))
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

    fn expect_feature(&mut self, feature: Feature) -> PRes<()> {
        self.features
            .expect(feature)
            .map_err(|f| ParseError::MissingFeature(f).attach_pos(self.curr_pos()))
    }

    fn parse_type(&mut self) -> PRes<ValType> {
        self.expect_feature(Feature::Types)?;
        Ok(match self.next_token_err()? {
            LexToken::Keyword(Keyword::Num) => ValType::Number,
            LexToken::Keyword(Keyword::String) => ValType::String,
            LexToken::Keyword(Keyword::Bool) => ValType::Boolean,
            _ => return Err(self.unexpected_last_token(TokenExpectation::ValType)),
        })
    }

    fn parse_param(&mut self) -> PRes<ParseToken> {
        self.expect_feature(Feature::Parameters)?;
        let Some(LexToken::GlobalVar(id)) = self.next_token() else {
            return Err(self.unexpected_last_token(TokenExpectation::GlobalVar));
        };
        self.expect_symbol('=')?;
        let val = self.parse_expr()?;
        let Some(val) = val.is_const() else {
            return Err(ParseError::ParamNotConst(id).attach_pos(val.start));
        };
        Ok(ParseToken::Param(id, val))
    }

    fn parse_event(&mut self, begin: FilePos) -> PRes<ParseToken> {
        self.expect_feature(Feature::Events)?;
        let kind = match self.next_token_err()? {
            LexToken::Keyword(Keyword::Mouse) => EventKind::Mouse,
            LexToken::Keyword(Keyword::Key) => EventKind::Key,
            _ => return Err(self.unexpected_last_token(TokenExpectation::EventKind)),
        };
        let args = self.parse_proto_args(false)?;
        Ok(ParseToken::EventHandler(
            kind,
            PathDef {
                name: 0,
                args,
                body: self.parse_statements(begin, Keyword::EndEvent)?,
            },
        ))
    }

    fn parse_path(&mut self, begin: FilePos) -> PRes<ParseToken> {
        let name = self.match_identifier()?;
        let args = self.parse_proto_args(true)?;
        self.set_ident_type(name, Identified::Path)?;
        Ok(ParseToken::PathDef(PathDef {
            name,
            args,
            body: self.parse_statements(begin, Keyword::Endpath)?,
        }))
    }

    fn parse_calc(&mut self, begin: FilePos) -> PRes<ParseToken> {
        let name = self.match_identifier()?;
        let args = self.parse_proto_args(false)?;
        let ret_ty = self.parse_type_hint()?;
        self.set_ident_type(name, Identified::Calc)?;
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

    fn parse_proto_args(&mut self, optional: bool) -> PRes<Vec<(usize, ValType)>> {
        if optional && !self.match_symbol('(') {
            return Ok(Vec::new());
        } else if !optional {
            self.expect_symbol('(')?;
        }
        let mut res = Vec::new();
        while !self.match_symbol(')') {
            if !res.is_empty() {
                self.expect_symbol(',')?;
            }
            let arg = self.match_identifier()?;
            self.set_ident_type(arg, Identified::LocalVar)?;
            let ty = self.parse_type_hint()?;
            res.push((arg, ty));
        }
        Ok(res)
    }

    fn parse_type_hint(&mut self) -> PRes<ValType> {
        Ok(match self.features[Feature::Types] {
            FeatureState::Enabled => {
                self.expect_symbol(':')?;
                self.parse_type()?
            }
            FeatureState::Auto => {
                if self.match_symbol(':') {
                    self.features[Feature::Types] = FeatureState::Enabled;
                    self.parse_type()?
                } else {
                    self.features[Feature::Types] = FeatureState::Disabled;
                    ValType::Number
                }
            }
            FeatureState::Disabled => ValType::Number,
        })
    }

    fn parse_main(&mut self, begin: FilePos) -> PRes<ParseToken> {
        Ok(ParseToken::StartBlock(
            self.parse_statements(begin, Keyword::End)?,
        ))
    }

    fn parse_variable(&mut self) -> PRes<Variable> {
        let pos = self.curr_pos();
        match self.next_token_err()? {
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

    pub fn parse_expr(&mut self) -> PRes<Expr> {
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
        Ok(match self.next_token_err()? {
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
            LexToken::IntLiteral(i) => ExprKind::Const(Value::Number(i as f64)).at(start, start),
            LexToken::FloatLiteral(f) => ExprKind::Const(Value::Number(f)).at(start, start),
            LexToken::StringLiteral(s) => ExprKind::Const(Value::String(s)).at(start, start),
            LexToken::Keyword(Keyword::True) => {
                ExprKind::Const(Value::Boolean(true)).at(start, start)
            }
            LexToken::Keyword(Keyword::False) => {
                ExprKind::Const(Value::Boolean(false)).at(start, start)
            }
            LexToken::GlobalVar(v) => {
                ExprKind::Variable(VariableKind::Global(v, ValType::Any).at(start)).at(start, start)
            }
            LexToken::PredefVar(pdv) => {
                ExprKind::Variable(VariableKind::GlobalPreDef(pdv).at(start)).at(start, start)
            }
            LexToken::Keyword(kw) => {
                if let Some(pdf) = PredefFunc::parse(kw) {
                    let args = self.parse_args(Some(pdf.args().len()))?;
                    ExprKind::FuncCall(pdf, args).at(start, self.last_pos())
                } else if let Some(conv) = ValType::parse(kw) {
                    self.expect_feature(Feature::Types)?;
                    self.expect_symbol('(')?;
                    let expr = self.parse_expr()?;
                    self.expect_symbol(')')?;
                    ExprKind::Convert(Box::new(expr), conv).at(start, self.last_pos())
                } else {
                    return Err(self.unexpected_last_token(TokenExpectation::PredefFunc));
                }
            }
            LexToken::Identifier(id) => {
                if self.lookahead() == Some(LexToken::Symbol('(')) {
                    let args = self.parse_args(None)?;
                    self.set_ident_type(id, Identified::Calc)?;
                    ExprKind::CalcCall(id, args).at(start, self.last_pos())
                } else {
                    self.set_ident_type(id, Identified::LocalVar)?;
                    ExprKind::Variable(VariableKind::Local(id, ValType::Any).at(start))
                        .at(start, start)
                }
            }
            t => {
                return Err(ParseError::UnexpectedToken(t, TokenExpectation::Expr).attach_pos(start))
            }
        })
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
        match self.next_token_err() {
            Ok(t) => ParseError::UnexpectedToken(t, expected).attach_pos(pos),
            Err(eof) => eof,
        }
    }

    fn unexpected_last_token(&mut self, expected: TokenExpectation) -> Pos<ParseError> {
        self.pos -= 1;
        self.unexpected_token(expected)
    }
}

impl Iterator for Parser<'_, '_> {
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
    #[error("missing feature {0}")]
    MissingFeature(Feature),
    #[error("param #{0} doesn't have a constant default value")]
    ParamNotConst(usize),
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
    EventKind,
    GlobalVar,
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
            TokenExpectation::EventKind => write!(f, "event kind"),
            TokenExpectation::GlobalVar => write!(f, "global variable"),
        }
    }
}
