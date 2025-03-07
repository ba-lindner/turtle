use std::fmt::Display;

use crate::{
    prog::{lexer::LexToken, CalcDef, PathDef},
    tokens::*,
    FilePos, Identified, Pos, Positionable, SymbolTable,
};

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
        Ok(ParseToken::StartBlock(self.parse_statements(begin, Keyword::End)?))
    }

    fn parse_stm(&mut self) -> PRes<Pos<Statement>> {
        let Some(LexToken::Keyword(kw)) = self.lookahead() else {
            return Err(self.unexpected_token(TokenExpectation::Statement));
        };
        let fp = self.curr_pos();
        self.pos += 1;
        Ok(match kw {
            Keyword::Walk => self.parse_move(true),
            Keyword::Jump => self.parse_move(false),
            Keyword::Turn => self.parse_turn(),
            Keyword::Direction => Ok(Statement::Direction(self.parse_expr()?)),
            Keyword::Color => self.parse_color(),
            Keyword::Clear => Ok(Statement::Clear),
            Keyword::Stop => Ok(Statement::Stop),
            Keyword::Finish => Ok(Statement::Finish),
            Keyword::Path => self.parse_path_call(),
            Keyword::Store => {
                let expr = self.parse_expr()?;
                self.expect_keyword(Keyword::In)?;
                let var = self.parse_variable()?;
                if let Variable::GlobalPreDef(pdv) = var {
                    if !pdv.is_writeable() {
                        return Err(ParseError::WriteToReadOnly(pdv).attach_pos(self.curr_pos()));
                    }
                }
                Ok(Statement::Store(expr, var))
            }
            Keyword::Add | Keyword::Append => self.parse_calc_stm(Keyword::To, BiOperator::Add, false),
            Keyword::Sub => self.parse_calc_stm(Keyword::From, BiOperator::Sub, false),
            Keyword::Mul => self.parse_calc_stm(Keyword::By, BiOperator::Mul, true),
            Keyword::Div => self.parse_calc_stm(Keyword::By, BiOperator::Div, true),
            Keyword::Mark => Ok(Statement::Mark),
            Keyword::Print => Ok(Statement::Print(self.parse_expr()?)),
            Keyword::If => self.parse_if(fp),
            Keyword::Do => {
                let expr = self.parse_expr()?;
                self.expect_keyword(Keyword::Times)?;
                Ok(Statement::DoLoop(
                    expr,
                    self.parse_statements(fp, Keyword::Done)?,
                ))
            }
            Keyword::Counter => self.parse_counter(fp),
            Keyword::While => {
                let cond = self.parse_expr()?;
                self.expect_keyword(Keyword::Do)?;
                Ok(Statement::WhileLoop(
                    cond,
                    self.parse_statements(fp, Keyword::Done)?,
                ))
            }
            Keyword::Repeat => {
                let stmts = self.parse_statements(fp, Keyword::Until)?;
                Ok(Statement::RepeatLoop(self.parse_expr()?, stmts))
            }
            c => panic!("unknown statement {c}"), // Err(ParseError::UnknownStatement(c).attach_pos(fp)),
        }?
        .attach_pos(fp))
    }

    fn parse_move(&mut self, draw: bool) -> PRes<Statement> {
        if self.match_keyword(Keyword::Home) {
            Ok(Statement::MoveHome(draw))
        } else if self.match_keyword(Keyword::Mark) {
            Ok(Statement::MoveMark(draw))
        } else {
            let back = self.match_keyword(Keyword::Back);
            let dist = self.parse_expr()?;
            Ok(Statement::MoveDist { dist, draw, back })
        }
    }

    fn parse_turn(&mut self) -> PRes<Statement> {
        if self.match_keyword(Keyword::Left) {
            Ok(Statement::Turn {
                left: true,
                by: self.parse_expr()?,
            })
        } else {
            let _ = self.match_keyword(Keyword::Right); // ignore as it's optional
            Ok(Statement::Turn {
                left: false,
                by: self.parse_expr()?,
            })
        }
    }

    fn parse_color(&mut self) -> PRes<Statement> {
        let expr_red = self.parse_expr()?;
        self.expect_symbol(',')?;
        let expr_green = self.parse_expr()?;
        self.expect_symbol(',')?;
        let expr_blue = self.parse_expr()?;
        Ok(Statement::Color(expr_red, expr_green, expr_blue))
    }

    fn parse_path_call(&mut self) -> PRes<Statement> {
        let name = self.match_identifier()?;
        let mut args = Vec::new();
        if self.match_symbol('(') && !self.match_symbol(')') {
            args.push(self.parse_expr()?);
            loop {
                if self.match_symbol(')') {
                    break;
                }
                self.expect_symbol(',')?;
                args.push(self.parse_expr()?);
            }
        }
        self.set_ident_type(name, Identified::Path(args.len()))?;
        Ok(Statement::PathCall(name, args))
    }

    fn parse_calc_stm(&mut self, mid: Keyword, op: BiOperator, var_first: bool) -> PRes<Statement> {
        let (var, val) = if var_first {
            let var = self.parse_variable()?;
            self.expect_keyword(mid)?;
            let val = self.parse_expr()?;
            (var, val)
        } else {
            let val = self.parse_expr()?;
            self.expect_keyword(mid)?;
            let var = self.parse_variable()?;
            (var, val)
        };
        Ok(Statement::Calc { val, var, op })
    }

    fn parse_if(&mut self, begin: FilePos) -> PRes<Statement> {
        let cond = self.parse_expr()?;
        self.expect_keyword(Keyword::Then)?;
        let mut statements = Vec::new();
        let else_start = loop {
            let fp = self.curr_pos();
            if self.match_keyword(Keyword::Else) {
                break Some(fp);
            }
            if self.match_keyword(Keyword::Endif) {
                break None;
            }
            statements.push(self.parse_stm()?);
        };
        let if_block = Block { begin, statements };
        if let Some(else_start) = else_start {
            Ok(Statement::IfElseBranch(
                cond,
                if_block,
                self.parse_statements(else_start, Keyword::Endif)?,
            ))
        } else {
            Ok(Statement::IfBranch(cond, if_block))
        }
    }

    fn parse_counter(&mut self, begin: FilePos) -> PRes<Statement> {
        let counter = self.parse_variable()?;
        self.expect_keyword(Keyword::From)?;
        let from = self.parse_expr()?;
        let up = self.match_keyword(Keyword::To);
        if !up {
            self.expect_keyword(Keyword::Downto)?;
        }
        let to = self.parse_expr()?;
        let step = if self.match_keyword(Keyword::Step) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect_keyword(Keyword::Do)?;
        Ok(Statement::CounterLoop {
            counter,
            from,
            up,
            to,
            step,
            body: self.parse_statements(begin, Keyword::Done)?,
        })
    }

    fn parse_statements(&mut self, begin: FilePos, end_key: Keyword) -> PRes<Block> {
        let mut statements = Vec::new();
        while !self.match_keyword(end_key) {
            statements.push(self.parse_stm()?);
        }
        Ok(Block { begin, statements })
    }

    fn parse_variable(&mut self) -> PRes<Variable> {
        let token = self
            .next_token()
            .ok_or(ParseError::UnexpectedEnd.attach_pos(self.curr_pos()))?;
        match token {
            LexToken::GlobalVar(id) => {
                self.set_ident_type(id, Identified::GlobalVar)?;
                Ok(Variable::Global(id, ValType::Any))
            }
            LexToken::PredefVar(pdv) => Ok(Variable::GlobalPreDef(pdv)),
            LexToken::Identifier(id) => {
                self.set_ident_type(id, Identified::LocalVar)?;
                Ok(Variable::Local(id, ValType::Any))
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
            (&[BiOperator::Mul, BiOperator::Div], Associativity::LeftToRight),
            (&[BiOperator::Add, BiOperator::Sub], Associativity::LeftToRight),
            (&[BiOperator::Less, BiOperator::LessEqual, BiOperator::Greater, BiOperator::GreaterEqual], Associativity::LeftToRight),
            (&[BiOperator::Equal, BiOperator::UnEqual], Associativity::LeftToRight),
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
                                nodes[i + 1].1 = Some(Expr::BiOperation(
                                    Box::new(nodes[i].1.take().unwrap()),
                                    op,
                                    Box::new(nodes[i + 1].1.take().unwrap()),
                                ));
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
                                nodes[i - 1].1 = Some(Expr::BiOperation(
                                    Box::new(nodes[i - 1].1.take().unwrap()),
                                    op,
                                    Box::new(nodes[i].1.take().unwrap()),
                                ));
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
        let curr_pos = self.curr_pos();
        Ok(match self.next_token().ok_or(ParseError::UnexpectedEnd.attach_pos(curr_pos))? {
            LexToken::Symbol('(') => {
                let expr = self.parse_expr()?;
                self.expect_symbol(')')?;
                Expr::Bracket(Box::new(expr))
            }
            LexToken::Symbol('|') => {
                let expr = self.parse_expr()?;
                self.expect_symbol('|')?;
                Expr::Absolute(Box::new(expr))
            }
            LexToken::Symbol('-') => Expr::UnOperation(UnOperator::Negate, Box::new(self.parse_operand()?)),
            LexToken::Keyword(Keyword::Not) => Expr::UnOperation(UnOperator::Not, Box::new(self.parse_operand()?)),
            LexToken::IntLiteral(i) => Expr::Const(Value::Number(i as f64)),
            LexToken::FloatLiteral(f) => Expr::Const(Value::Number(f)),
            LexToken::StringLiteral(s) => Expr::Const(Value::String(s)),
            LexToken::Keyword(Keyword::True) => Expr::Const(Value::Boolean(true)),
            LexToken::Keyword(Keyword::False) => Expr::Const(Value::Boolean(false)),
            LexToken::GlobalVar(v) => Expr::Variable(Variable::Global(v, ValType::Any)),
            LexToken::PredefVar(pdv) => Expr::Variable(Variable::GlobalPreDef(pdv)),
            LexToken::Keyword(kw) => {
                if let Some(pdf) = PredefFunc::parse(kw) {
                    let args = self.parse_args(Some(pdf.args().len()))?;
                    Expr::FuncCall(pdf, args)
                } else if let Some(conv) = ValType::parse(kw) {
                    self.expect_symbol('(')?;
                    let expr = self.parse_expr()?;
                    self.expect_symbol(')')?;
                    Expr::Convert(Box::new(expr), conv)
                } else {
                    self.pos -= 1;
                    return Err(self.unexpected_token(TokenExpectation::PredefFunc));
                }
            }
            LexToken::Identifier(id) => {
                if self.lookahead() == Some(LexToken::Symbol('(')) {
                    let args = self.parse_args(None)?;
                    self.set_ident_type(id, Identified::Calc(args.len()))?;
                    Expr::CalcCall(id, args)
                } else {
                    self.set_ident_type(id, Identified::LocalVar)?;
                    Expr::Variable(Variable::Local(id, ValType::Any))
                }
            }
            t => return Err(ParseError::UnexpectedToken(t, TokenExpectation::Expr).attach_pos(curr_pos)),
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
                return Err(ParseError::ArgCount(args.len(), c).attach_pos(pos))
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

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("different types for operator {0}: {1} != {2}")]
    BiOpDifferentTypes(BiOperator, ValType, ValType),
    #[error("operator {0} not defined for {1}")]
    BiOpWrongType(BiOperator, ValType),
    #[error("operator {0} not defined for {1}")]
    UnOpWrongType(UnOperator, ValType),
    #[error("absolute value not defined for {0}")]
    AbsoluteValue(ValType),
    #[error("wrong number of arguments: got {0}, expected {1}")]
    ArgsWrongLength(usize, usize),
    #[error("argument #{0} has type {1}, should have {2}")]
    ArgWrongType(usize, ValType, ValType)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn if_branch() {
        let fp = FilePos::new(0, 0);
        let mut ident = SymbolTable::new();
        ident.insert(String::from("a"), Identified::LocalVar);
        let mut parse = Parser::new(
            &mut ident,
            vec![
                LexToken::Keyword(Keyword::If).attach_pos(fp),
                LexToken::Identifier(0).attach_pos(fp),
                LexToken::Symbol('<').attach_pos(fp),
                LexToken::IntLiteral(2).attach_pos(fp),
                LexToken::Keyword(Keyword::Then).attach_pos(fp),
                LexToken::Keyword(Keyword::Stop).attach_pos(fp),
                LexToken::Keyword(Keyword::Endif).attach_pos(fp),
            ],
        );
        let stmt = parse.parse_stm().unwrap();
        assert_eq!(
            *stmt,
            Statement::IfBranch(
                Expr::BiOperation(
                    Box::new(Expr::Variable(Variable::Local(0, ValType::Any))),
                    BiOperator::Less,
                    Box::new(Expr::Const(Value::Number(2.0)))
                ),
                Block {
                    begin: fp,
                    statements: vec![Statement::Stop.attach_pos(fp)]
                }
            )
        );
    }

    #[test]
    fn if_else_branch() {
        let fp = FilePos::new(0, 0);
        let mut ident = SymbolTable::new();
        ident.insert(String::from("a"), Identified::LocalVar);
        let mut parse = Parser::new(
            &mut ident,
            vec![
                LexToken::Keyword(Keyword::If).attach_pos(fp),
                LexToken::Keyword(Keyword::Not).attach_pos(fp),
                LexToken::Identifier(0).attach_pos(fp),
                LexToken::Symbol('>').attach_pos(fp),
                LexToken::Symbol('=').attach_pos(fp),
                LexToken::IntLiteral(2).attach_pos(fp),
                LexToken::Keyword(Keyword::Then).attach_pos(fp),
                LexToken::Keyword(Keyword::Stop).attach_pos(fp),
                LexToken::Keyword(Keyword::Else).attach_pos(fp),
                LexToken::Keyword(Keyword::Walk).attach_pos(fp),
                LexToken::IntLiteral(30).attach_pos(fp),
                LexToken::Keyword(Keyword::Endif).attach_pos(fp),
            ],
        );
        let stmt = parse.parse_stm().unwrap();
        assert_eq!(
            *stmt,
            Statement::IfElseBranch(
                Expr::UnOperation(
                    UnOperator::Not,
                    Box::new(Expr::BiOperation(
                        Box::new(Expr::Variable(Variable::Local(0, ValType::Any))),
                        BiOperator::GreaterEqual,
                        Box::new(Expr::Const(Value::Number(2.0)))
                    )
                )),
                Block {
                    begin: fp,
                    statements: vec![Statement::Stop.attach_pos(fp)],
                },
                Block {
                    begin: fp,
                    statements: vec![Statement::MoveDist {
                        dist: Expr::Const(Value::Number(30.0)),
                        draw: true,
                        back: false,
                    }
                    .attach_pos(fp)]
                }
            )
        );
    }

    #[test]
    fn unfinished_expression() {
        let fp = FilePos::new(0, 0);
        let mut ident = SymbolTable::new();
        let mut parse = Parser::new(
            &mut ident,
            vec![
                LexToken::IntLiteral(3).attach_pos(fp),
                LexToken::Symbol('+').attach_pos(fp),
            ],
        );
        let res = parse.parse_expr();
        assert_eq!(res, Err(ParseError::UnexpectedEnd.attach_pos(fp)));
    }

    #[test]
    fn nested_expr() {
        let fp = FilePos::new(0, 0);
        let mut ident = SymbolTable::new();
        let mut parse = Parser::new(
            &mut ident,
            vec![
                LexToken::IntLiteral(3).attach_pos(fp),
                LexToken::Symbol('+').attach_pos(fp),
                LexToken::Symbol('|').attach_pos(fp),
                LexToken::IntLiteral(2).attach_pos(fp),
                LexToken::Symbol('-').attach_pos(fp),
                LexToken::Symbol('(').attach_pos(fp),
                LexToken::IntLiteral(5).attach_pos(fp),
                LexToken::Symbol('*').attach_pos(fp),
                LexToken::IntLiteral(4).attach_pos(fp),
                LexToken::Symbol(')').attach_pos(fp),
                LexToken::Symbol('|').attach_pos(fp),
                LexToken::Symbol('^').attach_pos(fp),
                LexToken::Symbol('(').attach_pos(fp),
                LexToken::IntLiteral(1).attach_pos(fp),
                LexToken::Symbol('+').attach_pos(fp),
                LexToken::IntLiteral(2).attach_pos(fp),
                LexToken::Symbol('*').attach_pos(fp),
                LexToken::IntLiteral(3).attach_pos(fp),
                LexToken::Symbol(')').attach_pos(fp),
            ],
        ); //3+|2-(5*4)|^(1+2*3)
        let expr = parse.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::BiOperation(
                Box::new(Expr::Const(Value::Number(3.0))),
                BiOperator::Add,
                Box::new(Expr::BiOperation(
                    Box::new(Expr::Absolute(Box::new(Expr::BiOperation(
                        Box::new(Expr::Const(Value::Number(2.0))),
                        BiOperator::Sub,
                        Box::new(Expr::Bracket(Box::new(Expr::BiOperation(
                            Box::new(Expr::Const(Value::Number(5.0))),
                            BiOperator::Mul,
                            Box::new(Expr::Const(Value::Number(4.0)))
                        ))))
                    )))),
                    BiOperator::Exp,
                    Box::new(Expr::Bracket(Box::new(Expr::BiOperation(
                        Box::new(Expr::Const(Value::Number(1.0))),
                        BiOperator::Add,
                        Box::new(Expr::BiOperation(
                            Box::new(Expr::Const(Value::Number(2.0))),
                            BiOperator::Mul,
                            Box::new(Expr::Const(Value::Number(3.0)))
                        ))
                    ))))
                ))
            )
        );
    }
}
