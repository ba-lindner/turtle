use crate::{
    prog::lexer::LexToken,
    prog::{CalcDef, PathDef},
    tokens::{keywords::Keyword, predef_vars::PredefVar, *},
    FilePos, Identified, Pos, Positionable,
};
use indexmap::IndexMap;

/// Result of parsing a specific node
type PRes<T> = Result<T, Pos<ParseError>>;

/// Turtle Parser
pub struct Parser<'a> {
    ltokens: Vec<Pos<LexToken>>,
    pos: usize,
    ident: &'a mut IndexMap<String, Identified>,
}

impl<'a> Parser<'a> {
    pub fn new(ident: &'a mut IndexMap<String, Identified>, ltokens: Vec<Pos<LexToken>>) -> Self {
        Self {
            ltokens,
            pos: 0,
            ident,
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
        Some(*self.ltokens[self.pos])
    }

    fn next_token(&mut self) -> Option<LexToken> {
        self.eof()?;
        self.pos += 1;
        Some(*self.ltokens[self.pos - 1])
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
            Err(self.unexpected_token(TokenExpectation::ExactKeyword(LexToken::Keyword(kw))))
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
            Err(self.unexpected_token(TokenExpectation::ExactSymbol(LexToken::Symbol(c))))
        } else {
            Ok(())
        }
    }

    fn set_ident_type(&mut self, ident: usize, kind: Identified) -> PRes<()> {
        let pos = self.curr_pos();
        let old_kind = self
            .ident
            .get_index_mut(ident)
            .ok_or(ParseError::MissingIdentifier(ident).attach_pos(pos))?
            .1;
        if *old_kind != Identified::Unknown && *old_kind != kind {
            Err(ParseError::ConflictingIdentifiers(ident, *old_kind, kind).attach_pos(pos))
        } else {
            *old_kind = kind;
            Ok(())
        }
    }

    fn parse_path(&mut self, begin: FilePos) -> PRes<ParseToken> {
        let name = self.match_identifier()?;
        self.set_ident_type(name, Identified::Path)?;
        let mut args = Vec::new();
        if self.match_symbol('(') {
            while let Some(LexToken::Identifier(arg)) = self.lookahead() {
                self.set_ident_type(arg, Identified::LocalVar)?;
                args.push(arg);
                self.pos += 1;
                if self.match_symbol(')') {
                    break;
                }
                self.expect_symbol(',')?;
            }
        }
        Ok(ParseToken::PathDef(PathDef {
            name,
            args,
            body: self.parse_statements(begin, Keyword::Endpath)?,
        }))
    }

    fn parse_calc(&mut self, begin: FilePos) -> PRes<ParseToken> {
        let name = self.match_identifier()?;
        self.set_ident_type(name, Identified::Calc)?;
        let mut args = Vec::new();
        if self.match_symbol('(') {
            while let Some(LexToken::Identifier(arg)) = self.lookahead() {
                self.set_ident_type(arg, Identified::LocalVar)?;
                args.push(arg);
                self.pos += 1;
                if self.match_symbol(')') {
                    break;
                }
                self.expect_symbol(',')?;
            }
        }
        let stmts = self.parse_statements(begin, Keyword::Returns)?;
        let ret = self.parse_expr()?;
        self.expect_keyword(Keyword::Endcalc)?;
        Ok(ParseToken::CalcDef(CalcDef {
            name,
            args,
            body: stmts,
            ret,
        }))
    }

    fn parse_main(&mut self, begin: FilePos) -> PRes<ParseToken> {
        Ok(ParseToken::StartBlock(self.parse_statements(begin, Keyword::End)?))
    }

    fn parse_stm(&mut self) -> PRes<Pos<Statement>> {
        let Some(LexToken::Keyword(kw)) = self.lookahead() else {
            return Err(self.unexpected_token(TokenExpectation::AnyKeyword));
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
            Keyword::Add => self.parse_calc_stm(Keyword::To, BiOperator::Add),
            Keyword::Sub => self.parse_calc_stm(Keyword::From, BiOperator::Sub),
            Keyword::Mul => self.parse_calc_stm(Keyword::By, BiOperator::Mul),
            Keyword::Div => self.parse_calc_stm(Keyword::By, BiOperator::Div),
            Keyword::Mark => Ok(Statement::Mark),
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
                let cond = self.parse_cond()?;
                self.expect_keyword(Keyword::Do)?;
                Ok(Statement::WhileLoop(
                    cond,
                    self.parse_statements(fp, Keyword::Done)?,
                ))
            }
            Keyword::Repeat => {
                let stmts = self.parse_statements(fp, Keyword::Until)?;
                Ok(Statement::RepeatLoop(self.parse_cond()?, stmts))
            }
            c => Err(ParseError::UnknownStatement(c).attach_pos(self.curr_pos())),
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
        self.set_ident_type(name, Identified::Path)?;
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
        Ok(Statement::PathCall(name, args))
    }

    fn parse_calc_stm(&mut self, mid: Keyword, op: BiOperator) -> PRes<Statement> {
        let var = self.parse_variable()?;
        self.expect_keyword(mid)?;
        let val = self.parse_expr()?;
        Ok(Statement::Calc { val, var, op })
    }

    fn parse_if(&mut self, begin: FilePos) -> PRes<Statement> {
        let cond = self.parse_cond()?;
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
                Ok(Variable::Global(id))
            }
            LexToken::PredefVar(pdv) => Ok(Variable::GlobalPreDef(pdv)),
            LexToken::Identifier(id) => {
                self.set_ident_type(id, Identified::LocalVar)?;
                Ok(Variable::Local(id))
            }
            t => Err(
                ParseError::UnexpectedToken(t, TokenExpectation::AnyIdentifier)
                    .attach_pos(self.curr_pos()),
            ),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, Pos<ParseError>> {
        let base_expr = self.parse_single_expr()?;
        let mut exprs = vec![('=', Some(base_expr))];
        while let Some(LexToken::Symbol(c)) = self.lookahead() {
            if c == '+' || c == '-' || c == '*' || c == '/' || c == '^' {
                self.pos += 1;
                exprs.push((c, Some(self.parse_single_expr()?)));
            } else {
                break;
            }
        }
        for i in (1..exprs.len()).rev() {
            if exprs[i].0 == '^' {
                exprs[i - 1].1 = Some(Expr::BiOperation(
                    Box::new(exprs[i - 1].1.take().unwrap()),
                    BiOperator::Exp,
                    Box::new(exprs[i].1.take().unwrap()),
                ));
                exprs[i].0 = '_';
            }
        }
        exprs.retain(|(op, _)| *op != '_');
        for i in 0..exprs.len() - 1 {
            let op = if exprs[i + 1].0 == '*' {
                BiOperator::Mul
            } else if exprs[i + 1].0 == '/' {
                BiOperator::Div
            } else {
                continue;
            };
            exprs[i + 1].1 = Some(Expr::BiOperation(
                Box::new(exprs[i].1.take().unwrap()),
                op,
                Box::new(exprs[i + 1].1.take().unwrap()),
            ));
            exprs[i + 1].0 = exprs[i].0;
            exprs[i].0 = '_';
        }
        exprs.retain(|(op, _)| *op != '_');
        for i in 0..exprs.len() - 1 {
            let op = if exprs[i + 1].0 == '+' {
                BiOperator::Add
            } else if exprs[i + 1].0 == '-' {
                BiOperator::Sub
            } else {
                continue;
            };
            exprs[i + 1].1 = Some(Expr::BiOperation(
                Box::new(exprs[i].1.take().unwrap()),
                op,
                Box::new(exprs[i + 1].1.take().unwrap()),
            ));
            exprs[i + 1].0 = exprs[i].0;
            exprs[i].0 = '_';
        }
        exprs.retain(|(op, _)| *op != '_');
        assert_eq!(exprs.len(), 1);
        assert_eq!(exprs[0].0, '=');
        Ok(exprs[0].1.take().unwrap())
    }

    fn parse_single_expr(&mut self) -> Result<Expr, Pos<ParseError>> {
        if self.match_symbol('(') {
            let expr = self.parse_expr()?;
            self.expect_symbol(')')?;
            Ok(Expr::Bracket(Box::new(expr)))
        } else if self.match_symbol('|') {
            let expr = self.parse_expr()?;
            self.expect_symbol('|')?;
            Ok(Expr::Absolute(Box::new(expr)))
        } else if self.match_symbol('-') {
            Ok(Expr::Negate(Box::new(self.parse_single_expr()?)))
        } else if let Some(LexToken::IntLiteral(i)) = self.lookahead() {
            self.pos += 1;
            Ok(Expr::Const(i as f64))
        } else if let Some(LexToken::FloatLiteral(f)) = self.lookahead() {
            self.pos += 1;
            Ok(Expr::Const(f))
        } else if let Some(LexToken::GlobalVar(id)) = self.lookahead() {
            self.pos += 1;
            Ok(Expr::Variable(Variable::Global(id)))
        } else if let Some(LexToken::PredefVar(pdv)) = self.lookahead() {
            self.pos += 1;
            Ok(Expr::Variable(Variable::GlobalPreDef(pdv)))
        } else if let Some(LexToken::Keyword(kw)) = self.lookahead() {
            self.pos += 1;
            let (id, anz) = match kw {
                Keyword::Sin => (PredefFunc::Sin, 1),
                Keyword::Cos => (PredefFunc::Cos, 1),
                Keyword::Tan => (PredefFunc::Tan, 1),
                Keyword::Sqrt => (PredefFunc::Sqrt, 1),
                Keyword::Rand => (PredefFunc::Rand, 2),
                _ => {
                    return Err(self.unexpected_token(TokenExpectation::PredefFunc));
                }
            };
            self.expect_symbol('(')?;
            let mut args = vec![self.parse_expr()?];
            for _ in 1..anz {
                self.expect_symbol(',')?;
                args.push(self.parse_expr()?);
            }
            self.expect_symbol(')')?;
            Ok(Expr::FuncCall(id, args))
        } else if let Some(LexToken::Identifier(id)) = self.lookahead() {
            self.pos += 1;
            if self.match_symbol('(') {
                self.set_ident_type(id, Identified::Calc)?;
                let mut args = Vec::new();
                if !self.match_symbol(')') {
                    args.push(self.parse_expr()?);
                    while !self.match_symbol(')') {
                        self.expect_symbol(',')?;
                        args.push(self.parse_expr()?);
                    }
                }
                Ok(Expr::CalcCall(id, args))
            } else {
                self.set_ident_type(id, Identified::LocalVar)?;
                Ok(Expr::Variable(Variable::Local(id)))
            }
        } else {
            Err(ParseError::UnexpectedEnd.attach_pos(self.curr_pos()))
        }
    }

    fn parse_cond(&mut self) -> Result<Cond, Pos<ParseError>> {
        #[derive(Debug, PartialEq, Clone, Copy)]
        enum BoolOp {
            And,
            Or,
            Eq,
            Rem,
        }

        let base_cond = self.parse_single_cond()?;
        let mut conds = vec![(BoolOp::Eq, Some(base_cond))];
        while let Some(LexToken::Keyword(kw)) = self.lookahead() {
            let op = match kw {
                Keyword::And => BoolOp::And,
                Keyword::Or => BoolOp::Or,
                _ => {
                    break;
                }
            };
            self.pos += 1;
            conds.push((op, Some(self.parse_single_cond()?)));
        }
        for i in 0..conds.len() - 1 {
            if conds[i + 1].0 != BoolOp::And {
                continue;
            };
            conds[i + 1].1 = Some(Cond::And(
                Box::new(conds[i].1.take().unwrap()),
                Box::new(conds[i + 1].1.take().unwrap()),
            ));
            conds[i + 1].0 = conds[i].0;
            conds[i].0 = BoolOp::Rem;
        }
        conds.retain(|(op, _)| *op != BoolOp::Rem);
        for i in 0..conds.len() - 1 {
            if conds[i + 1].0 != BoolOp::Or {
                continue;
            };
            conds[i + 1].1 = Some(Cond::Or(
                Box::new(conds[i].1.take().unwrap()),
                Box::new(conds[i + 1].1.take().unwrap()),
            ));
            conds[i + 1].0 = conds[i].0;
            conds[i].0 = BoolOp::Rem;
        }
        conds.retain(|(op, _)| *op != BoolOp::Rem);
        assert_eq!(conds.len(), 1);
        assert_eq!(conds[0].0, BoolOp::Eq);
        Ok(conds[0].1.take().unwrap())
    }

    fn parse_single_cond(&mut self) -> Result<Cond, Pos<ParseError>> {
        if self.match_keyword(Keyword::Not) {
            Ok(Cond::Not(Box::new(self.parse_single_cond()?)))
        } else if self.match_symbol('(') {
            let boxed = self.parse_single_cond()?;
            self.expect_symbol(')')?;
            Ok(Cond::Bracket(Box::new(boxed)))
        } else {
            let lhs = self.parse_expr()?;
            let cmp = self.parse_cmp_operator()?;
            let rhs = self.parse_expr()?;
            Ok(Cond::Cmp(Box::new(lhs), cmp, Box::new(rhs)))
        }
    }

    fn parse_cmp_operator(&mut self) -> Result<CmpOperator, Pos<ParseError>> {
        if self.match_symbol('<') {
            if self.match_symbol('>') {
                Ok(CmpOperator::UnEqual)
            } else if self.match_symbol('=') {
                Ok(CmpOperator::LessEqual)
            } else {
                Ok(CmpOperator::Less)
            }
        } else if self.match_symbol('>') {
            if self.match_symbol('=') {
                Ok(CmpOperator::GreaterEqual)
            } else {
                Ok(CmpOperator::Greater)
            }
        } else if self.match_symbol('=') {
            Ok(CmpOperator::Equal)
        } else {
            Err(self.unexpected_token(TokenExpectation::CmpOperator))
        }
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

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(LexToken, TokenExpectation),
    UnknownStatement(Keyword),
    UnexpectedEnd,
    MissingIdentifier(usize),
    ConflictingIdentifiers(usize, Identified, Identified),
    WriteToReadOnly(PredefVar),
}

#[derive(Debug, PartialEq)]
pub enum TokenExpectation {
    AnySymbol,
    AnyKeyword,
    BlockStart,
    ExactKeyword(LexToken),
    ExactSymbol(LexToken),
    AnyIdentifier,
    CmpOperator,
    PredefFunc,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn if_branch() {
        let fp = FilePos::new(0, 0);
        let mut ident = IndexMap::<String, Identified>::new();
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
                Cond::Cmp(
                    Box::new(Expr::Variable(Variable::Local(0))),
                    CmpOperator::Less,
                    Box::new(Expr::Const(2.0))
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
        let mut ident = IndexMap::<String, Identified>::new();
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
                Cond::Not(Box::new(Cond::Cmp(
                    Box::new(Expr::Variable(Variable::Local(0))),
                    CmpOperator::GreaterEqual,
                    Box::new(Expr::Const(2.0))
                ))),
                Block {
                    begin: fp,
                    statements: vec![Statement::Stop.attach_pos(fp)],
                },
                Block {
                    begin: fp,
                    statements: vec![Statement::MoveDist {
                        dist: Expr::Const(30.0),
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
        let mut ident = IndexMap::<String, Identified>::new();
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
        let mut ident = IndexMap::<String, Identified>::new();
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
                Box::new(Expr::Const(3.0)),
                BiOperator::Add,
                Box::new(Expr::BiOperation(
                    Box::new(Expr::Absolute(Box::new(Expr::BiOperation(
                        Box::new(Expr::Const(2.0)),
                        BiOperator::Sub,
                        Box::new(Expr::Bracket(Box::new(Expr::BiOperation(
                            Box::new(Expr::Const(5.0)),
                            BiOperator::Mul,
                            Box::new(Expr::Const(4.0))
                        ))))
                    )))),
                    BiOperator::Exp,
                    Box::new(Expr::Bracket(Box::new(Expr::BiOperation(
                        Box::new(Expr::Const(1.0)),
                        BiOperator::Add,
                        Box::new(Expr::BiOperation(
                            Box::new(Expr::Const(2.0)),
                            BiOperator::Mul,
                            Box::new(Expr::Const(3.0))
                        ))
                    ))))
                ))
            )
        );
    }
}
