use super::{LexToken, PRes, ParseError, Parser, TokenExpectation};
use crate::{features::Feature, tokens::*, FilePos, Identified, Pos, Positionable};

impl Parser<'_, '_> {
    pub(super) fn parse_statements(&mut self, begin: FilePos, end_key: Keyword) -> PRes<Block> {
        let mut statements = Vec::new();
        while !self.match_keyword(end_key) {
            statements.push(self.parse_stm()?);
        }
        Ok(Block { begin, statements })
    }

    pub fn parse_stm(&mut self) -> PRes<Pos<Statement>> {
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
            Keyword::Path => self.parse_path_call(false),
            Keyword::Store => {
                let expr = self.parse_expr()?;
                self.expect_keyword(Keyword::In)?;
                let var = self.parse_variable()?;
                if let VariableKind::GlobalPreDef(pdv) = var.kind {
                    if !pdv.is_writeable() {
                        return Err(ParseError::WriteToReadOnly(pdv).attach_pos(var.pos));
                    }
                }
                Ok(Statement::Store(expr, var))
            }
            Keyword::Add => self.parse_calc_stm(Keyword::To, BiOperator::Add, false),
            Keyword::Sub => self.parse_calc_stm(Keyword::From, BiOperator::Sub, false),
            Keyword::Mul => self.parse_calc_stm(Keyword::By, BiOperator::Mul, true),
            Keyword::Div => self.parse_calc_stm(Keyword::By, BiOperator::Div, true),
            Keyword::Append => {
                self.expect_feature(Feature::Types)?;
                self.parse_calc_stm(Keyword::To, BiOperator::Add, false)
            }
            Keyword::Mark => Ok(Statement::Mark),
            Keyword::Print => {
                self.expect_feature(Feature::Types)?;
                Ok(Statement::Print(self.parse_expr()?))
            }
            Keyword::Split => {
                self.expect_feature(Feature::Multithreading)?;
                self.parse_path_call(true)
            }
            Keyword::Wait => {
                self.expect_feature(Feature::Multithreading)?;
                Ok(Statement::Wait)
            }
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
            c => Err(ParseError::UnknownStatement(c).attach_pos(fp)),
        }?
        .attach_pos(fp))
    }

    pub(super) fn parse_move(&mut self, draw: bool) -> PRes<Statement> {
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

    pub(super) fn parse_turn(&mut self) -> PRes<Statement> {
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

    pub(super) fn parse_color(&mut self) -> PRes<Statement> {
        let expr_red = self.parse_expr()?;
        self.expect_symbol(',')?;
        let expr_green = self.parse_expr()?;
        self.expect_symbol(',')?;
        let expr_blue = self.parse_expr()?;
        Ok(Statement::Color(expr_red, expr_green, expr_blue))
    }

    pub(super) fn parse_path_call(&mut self, split: bool) -> PRes<Statement> {
        let name = self.match_identifier()?;
        let args = if self.lookahead() == Some(LexToken::Symbol('(')) {
            self.parse_args(None)?
        } else {
            Vec::new()
        };
        self.set_ident_type(name, Identified::Path)?;
        if split {
            Ok(Statement::Split(name, args))
        } else {
            Ok(Statement::PathCall(name, args))
        }
    }

    pub(super) fn parse_calc_stm(
        &mut self,
        mid: Keyword,
        op: BiOperator,
        var_first: bool,
    ) -> PRes<Statement> {
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

    pub(super) fn parse_if(&mut self, begin: FilePos) -> PRes<Statement> {
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

    pub(super) fn parse_counter(&mut self, begin: FilePos) -> PRes<Statement> {
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
}
