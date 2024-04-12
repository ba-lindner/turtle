use crate::{lexer::LexToken, FilePos, Identified};
use indexmap::IndexMap;
use tokens::*;

pub mod tokens;

// Aufwand: bisher ~5h

type PResult = Result<ParseToken, (FilePos, ParseError)>;
pub type PInput = (FilePos, Option<LexToken>);

pub struct Parser<'a> {
    ltokens: Vec<PInput>,
    pos: usize,
    ident: &'a mut IndexMap<String, Identified>,
}

impl<'a> Parser<'a> {
    pub fn new(ident: &'a mut IndexMap<String, Identified>, ltokens: Vec<PInput>) -> Self {
        Self {
            ltokens,
            pos: 0,
            ident,
        }
    }

    pub fn parse_next(&mut self) -> Option<PResult> {
        if self.eof() {
            return None;
        }
        Some(if self.match_keyword("path") {
            self.parse_path()
        } else if self.match_keyword("calculation") {
            self.parse_calc()
        } else if self.match_keyword("begin") {
            self.parse_main()
        } else {
            Err(self.unexpected_token_kind(LexToken::Keyword(0)))
        })
    }

    fn lookahead(&self) -> Option<&LexToken> {
        if self.eof() {
            None
        } else {
            self.ltokens[self.pos].1.as_ref()
        }
    }

    fn next_token(&mut self) -> Option<LexToken> {
        if self.eof() {
            None
        } else {
            self.pos += 1;
            self.ltokens[self.pos - 1].1.take()
        }
    }

    /*fn put_back(&mut self, token: LexToken) {
        assert!(self.pos > 0);
        self.pos -= 1;
        assert!(self.ltokens[self.pos].token.is_none());
        self.ltokens[self.pos].token = Some(token);
    }*/

    fn keyword_index(kw: &str) -> Option<usize> {
        for (idx, &key) in crate::KEYWORDS.iter().enumerate() {
            if kw == key {
                return Some(idx);
            }
        }
        None
    }

    fn match_keyword(&mut self, kw: &str) -> bool {
        let Some(LexToken::Keyword(idx)) = self.lookahead() else {
            return false;
        };
        if kw != crate::KEYWORDS[*idx] {
            return false;
        }
        self.pos += 1;
        true
    }

    fn expect_keyword(&mut self, kw: &str) -> Result<(), (FilePos, ParseError)> {
        if !self.match_keyword(kw) {
            Err(self.unexpected_token_exact(LexToken::Keyword(Self::keyword_index(kw).unwrap())))
        } else {
            Ok(())
        }
    }

    fn match_identifier(&mut self) -> Result<usize, (FilePos, ParseError)> {
        let Some(LexToken::Identifier(name)) = self.lookahead() else {
            return Err(self.unexpected_token_kind(LexToken::Identifier(0)));
        };
        let name = *name;
        self.pos += 1;
        Ok(name)
    }

    fn match_symbol(&mut self, c: char) -> bool {
        let Some(LexToken::Symbol(sym)) = self.lookahead() else {
            return false;
        };
        if c != *sym {
            return false;
        }
        self.pos += 1;
        true
    }

    fn expect_symbol(&mut self, c: char) -> Result<(), (FilePos, ParseError)> {
        if !self.match_symbol(c) {
            Err(self.unexpected_token_exact(LexToken::Symbol(c)))
        } else {
            Ok(())
        }
    }

    fn set_ident_type(&mut self, ident: usize, kind: Identified) -> Result<(), (FilePos, ParseError)> {
        let pos = self.curr_pos();
        let old_kind = self.ident.get_index_mut(ident).ok_or((pos, ParseError::MissingIdentifier(ident)))?.1;
        if *old_kind != Identified::Unknown && *old_kind != kind {
            let old_kind = *old_kind;
            Err((pos, ParseError::ConflictingIdentifiers(ident, old_kind, kind)))
        } else {
            *old_kind = kind;
            Ok(())
        }
    }

    fn parse_path(&mut self) -> PResult {
        let name = self.match_identifier()?;
        self.set_ident_type(name, Identified::Path)?;
        let mut args = Vec::new();
        if self.match_symbol('(') {
            while let Some(LexToken::Identifier(arg)) = self.lookahead() {
                let arg = *arg;
                self.set_ident_type(arg, Identified::LocalVar)?;
                args.push(arg);
                self.pos += 1;
                if self.match_symbol(')') {
                    break;
                }
                self.expect_symbol(',')?;
            }
        }
        Ok(ParseToken::PathDef(name, args, self.parse_statements("endpath")?))
    }

    fn parse_calc(&mut self) -> PResult {
        let name = self.match_identifier()?;
        self.set_ident_type(name, Identified::Calc)?;
        let mut args = Vec::new();
        if self.match_symbol('(') {
            while let Some(LexToken::Identifier(arg)) = self.lookahead() {
                let arg = *arg;
                self.set_ident_type(arg, Identified::LocalVar)?;
                args.push(arg);
                self.pos += 1;
                if self.match_symbol(')') {
                    break;
                }
                self.expect_symbol(',')?;
            }
        }
        let stmts = self.parse_statements("returns")?;
        let ret = self.parse_expr()?;
        self.expect_keyword("endcalc")?;
        Ok(ParseToken::CalcDef(name, args, stmts, ret))
    }

    fn parse_main(&mut self) -> PResult {
        Ok(ParseToken::StartBlock(self.parse_statements("end")?))
    }

    fn parse_stm(&mut self) -> Result<Statement, (FilePos, ParseError)> {
        let Some(LexToken::Keyword(kw)) = self.lookahead() else {
            return Err(self.unexpected_token_kind(LexToken::Keyword(0)));
        };
        let kw = *kw;
        self.pos += 1;
        match crate::KEYWORDS[kw] {
            "walk" => self.parse_walk(),
            "jump" => self.parse_jump(),
            "turn" => self.parse_turn(),
            "direction" => Ok(Statement::Direction(self.parse_expr()?)),
            "color" => self.parse_color(),
            "clear" => Ok(Statement::Clear),
            "stop" => Ok(Statement::Stop),
            "finish" => Ok(Statement::Finish),
            "path" => self.parse_path_call(),
            "store" => {
                let expr = self.parse_expr()?;
                self.expect_keyword("in")?;
                let var = self.parse_variable()?;
                if let Variable::GlobalPreDef(id) = var {
                    if !crate::PREDEF_VARS[id].1 {
                        return Err((self.curr_pos(), ParseError::WriteToReadOnly(id)))
                    }
                }
                Ok(Statement::Store(expr, var))
            },
            "add" => {
                let expr = self.parse_expr()?;
                self.expect_keyword("to")?;
                let var = self.parse_variable()?;
                Ok(Statement::Add(expr, var))
            },
            "sub" => {
                let expr = self.parse_expr()?;
                self.expect_keyword("from")?;
                let var = self.parse_variable()?;
                Ok(Statement::Sub(expr, var))
            },
            "mul" => {
                let var = self.parse_variable()?;
                self.expect_keyword("by")?;
                let expr = self.parse_expr()?;
                Ok(Statement::Mul(expr, var))
            },
            "div" => {
                let var = self.parse_variable()?;
                self.expect_keyword("by")?;
                let expr = self.parse_expr()?;
                Ok(Statement::Div(expr, var))
            },
            "mark" => Ok(Statement::Mark),
            "if" => self.parse_if(),
            "do" => {
                let expr = self.parse_expr()?;
                self.expect_keyword("times")?;
                Ok(Statement::DoLoop(expr, self.parse_statements("done")?))
            },
            "counter" => self.parse_counter(),
            "while" => {
                let cond = self.parse_cond()?;
                self.expect_keyword("do")?;
                Ok(Statement::WhileLoop(cond, self.parse_statements("done")?))
            },
            "repeat" => {
                let stmts = self.parse_statements("until")?;
                Ok(Statement::RepeatLoop(self.parse_cond()?, stmts))
            },
            c => Err((self.curr_pos(), ParseError::UnknownStatement(c))),
        }
    }

    fn parse_walk(&mut self) -> Result<Statement, (FilePos, ParseError)> {
        if self.match_keyword("home") {
            Ok(Statement::WalkHome)
        } else if self.match_keyword("mark") {
            Ok(Statement::WalkMark)
        } else if self.match_keyword("back") {
            let expr = self.parse_expr()?;
            Ok(Statement::WalkBack(expr))
        } else {
            let expr = self.parse_expr()?;
            Ok(Statement::Walk(expr))
        }
    }

    fn parse_jump(&mut self) -> Result<Statement, (FilePos, ParseError)> {
        if self.match_keyword("home") {
            Ok(Statement::JumpHome)
        } else if self.match_keyword("mark") {
            Ok(Statement::JumpMark)
        } else if self.match_keyword("back") {
            Ok(Statement::JumpBack(self.parse_expr()?))
        } else {
            Ok(Statement::Jump(self.parse_expr()?))
        }
    }

    fn parse_turn(&mut self) -> Result<Statement, (FilePos, ParseError)> {
        if self.match_keyword("left") {
            Ok(Statement::TurnLeft(self.parse_expr()?))
        } else {
            let _ = self.match_keyword("right"); // ignore as it's optional
            Ok(Statement::TurnRight(self.parse_expr()?))
        }
    }

    fn parse_color(&mut self) -> Result<Statement, (FilePos, ParseError)> {
        let expr_red = self.parse_expr()?;
        self.expect_symbol(',')?;
        let expr_green = self.parse_expr()?;
        self.expect_symbol(',')?;
        let expr_blue = self.parse_expr()?;
        Ok(Statement::Color(expr_red, expr_green, expr_blue))
    }

    fn parse_path_call(&mut self) -> Result<Statement, (FilePos, ParseError)> {
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

    fn parse_if(&mut self) -> Result<Statement, (FilePos, ParseError)> {
        let cond = self.parse_cond()?;
        self.expect_keyword("then")?;
        let mut stmts = Vec::new();
        let has_else = loop {
            if self.match_keyword("else") {
                break true;
            }
            if self.match_keyword("endif") {
                break false;
            }
            stmts.push(self.parse_stm()?);
        };
        if has_else {
            Ok(Statement::IfElseBranch(cond, stmts, self.parse_statements("endif")?))
        } else {
            Ok(Statement::IfBranch(cond, stmts))
        }
    }

    fn parse_counter(&mut self) -> Result<Statement, (FilePos, ParseError)> {
        let var = self.parse_variable()?;
        self.expect_keyword("from")?;
        let init = self.parse_expr()?;
        let count_up = self.match_keyword("to");
        if !count_up {
            self.expect_keyword("downto")?;
        }
        let target = self.parse_expr()?;
        let step = if self.match_keyword("step") {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect_keyword("do")?;
        Ok(Statement::CounterLoop(var, init, count_up, target, step, self.parse_statements("done")?))
    }

    fn parse_statements(&mut self, end_key: &str) -> Result<Vec<Statement>, (FilePos, ParseError)> {
        let mut stmts = Vec::new();
        while !self.match_keyword(end_key) {
            stmts.push(self.parse_stm()?);
        }
        Ok(stmts)
    }

    fn parse_variable(&mut self) -> Result<Variable, (FilePos, ParseError)> {
        let nt = self.next_token().ok_or((self.curr_pos(), ParseError::UnexpectedEnd))?;
        match nt {
            LexToken::GlobalVar(id, predef) => if predef {
                Ok(Variable::GlobalPreDef(id))
            } else {
                self.set_ident_type(id, Identified::GlobalVar)?;
                Ok(Variable::Global(id))
            },
            LexToken::Identifier(id) => {
                self.set_ident_type(id, Identified::LocalVar)?;
                Ok(Variable::Local(id))
            },
            t => Err((self.curr_pos(), ParseError::UnexpectedTokenKind(t, LexToken::Identifier(0))))
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, (FilePos, ParseError)> {
        let base_expr = self.parse_single_expr()?;
        let mut exprs = vec![('=', Some(base_expr))];
        while let Some(LexToken::Symbol(c)) = self.lookahead() {
            let c = *c;
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
                    Box::new(exprs[i].1.take().unwrap())
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
                Box::new(exprs[i + 1].1.take().unwrap())
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
                Box::new(exprs[i + 1].1.take().unwrap())
            ));
            exprs[i + 1].0 = exprs[i].0;
            exprs[i].0 = '_';
        }
        exprs.retain(|(op, _)| *op != '_');
        assert_eq!(exprs.len(), 1);
        assert_eq!(exprs[0].0, '=');
        Ok(exprs[0].1.take().unwrap())
    }

    fn parse_single_expr(&mut self) -> Result<Expr, (FilePos, ParseError)> {
        if self.match_symbol('(') {
            let expr = self.parse_expr()?;
            self.expect_symbol(')')?;
            Ok(Expr::Bracket(Box::new(expr)))
        } else if self.match_symbol('|') {
            let expr = self.parse_expr()?;
            self.expect_symbol('|')?;
            Ok(Expr::Absolute(Box::new(expr)))
        } else if self.match_symbol('-') {
            Ok(Expr::UnOperation(UnOperator::Neg, Box::new(self.parse_single_expr()?)))
        } else if let Some(LexToken::IntLiteral(i)) = self.lookahead() {
            let f = *i as f64;
            self.pos += 1;
            Ok(Expr::Const(f))
        } else if let Some(LexToken::FloatLiteral(f)) = self.lookahead() {
            let f = *f;
            self.pos += 1;
            Ok(Expr::Const(f))
        } else if let Some(LexToken::GlobalVar(id, predef)) = self.lookahead() {
            let (id, predef) = (*id, *predef);
            self.pos += 1;
            Ok(Expr::Variable(if predef {
                Variable::GlobalPreDef(id)
            } else {
                Variable::Global(id)
            }))
        } else if let Some(LexToken::Keyword(id)) = self.lookahead() {
            let id = *id;
            self.pos += 1;
            let (id, anz) = match crate::KEYWORDS[id] {
                "sin" => (PredefFunc::Sin, 1),
                "cos" => (PredefFunc::Cos, 1),
                "tan" => (PredefFunc::Tan, 1),
                "sqrt" => (PredefFunc::Sqrt, 1),
                "rand" => (PredefFunc::Rand, 2),
                _ => {
                    return Err(self.unexpected_token_kind(LexToken::Keyword(0)));
                },
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
            let id = *id;
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
            Err((self.curr_pos(), ParseError::UnexpectedEnd))
        }
    }

    fn parse_cond(&mut self) -> Result<Cond, (FilePos, ParseError)> {
        let base_cond = self.parse_single_cond()?;
        let mut conds = vec![('=', Some(base_cond))];
        while let Some(LexToken::Keyword(id)) = self.lookahead() {
            let id = *id;
            if crate::KEYWORDS[id] == "and" || crate::KEYWORDS[id] == "or" {
                self.pos += 1;
                let c = crate::KEYWORDS[id].chars().next().unwrap();
                conds.push((c, Some(self.parse_single_cond()?)));
            } else {
                break;
            }
        }
        for i in 0..conds.len() - 1 {
            if conds[i + 1].0 != 'a' {
                continue;
            };
            conds[i + 1].1 = Some(Cond::And(
                Box::new(conds[i].1.take().unwrap()),
                Box::new(conds[i + 1].1.take().unwrap())
            ));
            conds[i + 1].0 = conds[i].0;
            conds[i].0 = '_';
        }
        conds.retain(|(op, _)| *op != '_');
        for i in 0..conds.len() - 1 {
            if conds[i + 1].0 != 'o' {
                continue;
            };
            conds[i + 1].1 = Some(Cond::Or(
                Box::new(conds[i].1.take().unwrap()),
                Box::new(conds[i + 1].1.take().unwrap())
            ));
            conds[i + 1].0 = conds[i].0;
            conds[i].0 = '_';
        }
        conds.retain(|(op, _)| *op != '_');
        assert_eq!(conds.len(), 1);
        assert_eq!(conds[0].0, '=');
        Ok(conds[0].1.take().unwrap())
    }

    fn parse_single_cond(&mut self) -> Result<Cond, (FilePos, ParseError)> {
        if self.match_keyword("not") {
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

    fn parse_cmp_operator(&mut self) -> Result<CmpOperator, (FilePos, ParseError)> {
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
            Err(self.unexpected_token_kind(LexToken::Symbol('_')))
        }
    }

    fn eof(&self) -> bool {
        self.pos == self.ltokens.len()
    }

    fn curr_pos(&self) -> FilePos {
        if self.eof() {
            return self.ltokens[self.ltokens.len() - 1].0;
        }
        self.ltokens[self.pos].0
    }

    fn unexpected_token_exact(&mut self, expected: LexToken) -> (FilePos, ParseError) {
        let pos = self.curr_pos();
        let Some(token) = self.next_token() else {
            return (pos, ParseError::UnexpectedEnd);
        };
        (pos, ParseError::UnexpectedTokenExact(token, expected))
    }

    fn unexpected_token_kind(&mut self, expected: LexToken) -> (FilePos, ParseError) {
        let pos = self.curr_pos();
        let Some(token) = self.next_token() else {
            return (pos, ParseError::UnexpectedEnd);
        };
        (pos, ParseError::UnexpectedTokenKind(token, expected))
    }
}

impl Iterator for Parser<'_> {
    type Item = PResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_next()
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedTokenExact(LexToken, LexToken),
    UnexpectedTokenKind(LexToken, LexToken),
    UnknownStatement(&'static str),
    UnexpectedEnd,
    MissingIdentifier(usize),
    ConflictingIdentifiers(usize, Identified, Identified),
    WriteToReadOnly(usize),
}

#[cfg(test)]
mod test {
    use super::*;

    fn keyword(kw: &str) -> PInput {
        (FilePos::new(0, 0), crate::KEYWORDS.iter().position(|&k| k == kw).map(|idx| LexToken::Keyword(idx)))
    }

    #[test]
    fn if_branch() {
        let fp = FilePos::new(0, 0);
        let mut ident = IndexMap::<String, Identified>::new();
        ident.insert(String::from("a"), Identified::LocalVar);
        let mut parse = Parser::new(&mut ident, vec![
            keyword("if"),
            (fp, Some(LexToken::Identifier(0))),
            (fp, Some(LexToken::Symbol('<'))),
            (fp, Some(LexToken::IntLiteral(2))),
            keyword("then"),
            keyword("stop"),
            keyword("endif")
        ]);
        let stmt = parse.parse_stm().unwrap();
        assert_eq!(stmt, Statement::IfBranch(
            Cond::Cmp(
                Box::new(Expr::Variable(Variable::Local(0))),
                CmpOperator::Less,
                Box::new(Expr::Const(2.0))
            ),
            vec![
                Statement::Stop
            ]
        ));
    }

    #[test]
    fn if_else_branch() {
        let fp = FilePos::new(0, 0);
        let mut ident = IndexMap::<String, Identified>::new();
        ident.insert(String::from("a"), Identified::LocalVar);
        let mut parse = Parser::new(&mut ident, vec![
            keyword("if"),
            keyword("not"),
            (fp, Some(LexToken::Identifier(0))),
            (fp, Some(LexToken::Symbol('>'))),
            (fp, Some(LexToken::Symbol('='))),
            (fp, Some(LexToken::IntLiteral(2))),
            keyword("then"),
            keyword("stop"),
            keyword("else"),
            keyword("walk"),
            (fp, Some(LexToken::IntLiteral(30))),
            keyword("endif")
        ]);
        let stmt = parse.parse_stm().unwrap();
        assert_eq!(stmt, Statement::IfElseBranch(
            Cond::Not(Box::new(Cond::Cmp(
                Box::new(Expr::Variable(Variable::Local(0))),
                CmpOperator::GreaterEqual,
                Box::new(Expr::Const(2.0))
            ))),
            vec![
                Statement::Stop
            ],
            vec![
                Statement::Walk(Expr::Const(30.0))
            ]
        ));
    }

    #[test]
    fn unfinished_expression() {
        let fp = FilePos::new(0, 0);
        let mut ident = IndexMap::<String, Identified>::new();
        let mut parse = Parser::new(&mut ident, vec![
            (fp, Some(LexToken::IntLiteral(3))),
            (fp, Some(LexToken::Symbol('+'))),
        ]);
        let res = parse.parse_expr();
        assert_eq!(res, Err((fp, ParseError::UnexpectedEnd)));
    }

    #[test]
    fn nested_expr() {
        let fp = FilePos::new(0, 0);
        let mut ident = IndexMap::<String, Identified>::new();
        let mut parse = Parser::new(&mut ident, vec![
            (fp, Some(LexToken::IntLiteral(3))),
            (fp, Some(LexToken::Symbol('+'))),
            (fp, Some(LexToken::Symbol('|'))),
            (fp, Some(LexToken::IntLiteral(2))),
            (fp, Some(LexToken::Symbol('-'))),
            (fp, Some(LexToken::Symbol('('))),
            (fp, Some(LexToken::IntLiteral(5))),
            (fp, Some(LexToken::Symbol('*'))),
            (fp, Some(LexToken::IntLiteral(4))),
            (fp, Some(LexToken::Symbol(')'))),
            (fp, Some(LexToken::Symbol('|'))),
            (fp, Some(LexToken::Symbol('^'))),
            (fp, Some(LexToken::Symbol('('))),
            (fp, Some(LexToken::IntLiteral(1))),
            (fp, Some(LexToken::Symbol('+'))),
            (fp, Some(LexToken::IntLiteral(2))),
            (fp, Some(LexToken::Symbol('*'))),
            (fp, Some(LexToken::IntLiteral(3))),
            (fp, Some(LexToken::Symbol(')'))),
        ]);//3+|2-(5*4)|^(1+2*3)
        let expr = parse.parse_expr().unwrap();
        assert_eq!(expr, Expr::BiOperation(
            Box::new(Expr::Const(3.0)),
            BiOperator::Add,
            Box::new(Expr::BiOperation(
                Box::new(Expr::Absolute(
                    Box::new(Expr::BiOperation(
                        Box::new(Expr::Const(2.0)),
                        BiOperator::Sub,
                        Box::new(Expr::Bracket(
                            Box::new(Expr::BiOperation(
                                Box::new(Expr::Const(5.0)),
                                BiOperator::Mul,
                                Box::new(Expr::Const(4.0))
                            ))
                        ))
                        
                    ))
                )),
                BiOperator::Exp,
                Box::new(Expr::Bracket(
                    Box::new(Expr::BiOperation(
                        Box::new(Expr::Const(1.0)),
                        BiOperator::Add,
                        Box::new(Expr::BiOperation(
                            Box::new(Expr::Const(2.0)), 
                            BiOperator::Mul,
                            Box::new(Expr::Const(3.0))
                        ))
                    ))
                ))
            ))
        ));
    }
}
