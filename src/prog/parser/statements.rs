use super::{LexToken, PRes, ParseError, Parser, TokenExpectation};
use crate::{Identified, Positionable, Spanned, features::Feature, tokens::*};

impl Parser<'_> {
    pub(super) fn parse_statements(&mut self, end_key: Keyword) -> PRes<Block> {
        let begin = self.last_span();
        let mut statements = Vec::new();
        while !self.match_keyword(end_key) {
            statements.push(self.parse_stm()?);
        }
        Ok(Block {
            begin,
            full: begin.to(self.last_span()),
            statements,
        })
    }

    pub fn parse_stm(&mut self) -> PRes<Spanned<Statement>> {
        let Ok(LexToken::Keyword(kw)) = self.lookahead() else {
            return Err(self.unexpected_token(TokenExpectation::Statement));
        };
        let span = self.curr_span();
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
            Keyword::Path => self.parse_path_call(Statement::PathCall),
            Keyword::Store => {
                let expr = self.parse_expr()?;
                self.expect_keyword(Keyword::In)?;
                let var = self.parse_variable()?;
                if let VariableKind::GlobalPreDef(pdv) = *var.0 {
                    if !pdv.is_writeable() {
                        return Err(ParseError::WriteToReadOnly(pdv).with_span(var.0.get_span()));
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
                self.parse_path_call(Statement::Split)
            }
            Keyword::Wait => {
                self.expect_feature(Feature::Multithreading)?;
                Ok(Statement::Wait)
            }
            Keyword::If => self.parse_if(),
            Keyword::Do => {
                let expr = self.parse_expr()?;
                self.expect_keyword(Keyword::Times)?;
                Ok(Statement::DoLoop(
                    expr,
                    self.parse_statements(Keyword::Done)?,
                ))
            }
            Keyword::Counter => self.parse_counter(),
            Keyword::While => {
                let cond = self.parse_expr()?;
                self.expect_keyword(Keyword::Do)?;
                Ok(Statement::WhileLoop(
                    cond,
                    self.parse_statements(Keyword::Done)?,
                ))
            }
            Keyword::Repeat => {
                let stmts = self.parse_statements(Keyword::Until)?;
                Ok(Statement::RepeatLoop(self.parse_expr()?, stmts))
            }
            c => Err(ParseError::UnknownStatement(c).with_span(span)),
        }?
        .with_span(span.to(self.last_span())))
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

    pub(super) fn parse_path_call(
        &mut self,
        stmt: impl FnOnce(usize, ArgList) -> Statement,
    ) -> PRes<Statement> {
        let name = self.match_identifier()?;
        let args = if self.lookahead() == Ok(LexToken::Symbol('(')) {
            self.parse_args(None)?
        } else {
            Vec::new()
        };
        self.set_ident_type(name, Identified::Path)?;
        Ok(stmt(name, args))
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

    pub(super) fn parse_if(&mut self) -> PRes<Statement> {
        let cond = self.parse_expr()?;
        let if_start = self.curr_span();
        self.expect_keyword(Keyword::Then)?;
        let mut statements = Vec::new();
        let (has_else, if_end) = loop {
            let fp = self.curr_span();
            if self.match_keyword(Keyword::Else) {
                break (true, fp);
            }
            if self.match_keyword(Keyword::Endif) {
                break (false, fp);
            }
            statements.push(self.parse_stm()?);
        };
        let if_block = Block {
            begin: if_start,
            full: if_start.to(if_end),
            statements,
        };
        if has_else {
            Ok(Statement::IfElseBranch(
                cond,
                if_block,
                self.parse_statements(Keyword::Endif)?,
            ))
        } else {
            Ok(Statement::IfBranch(cond, if_block))
        }
    }

    pub(super) fn parse_counter(&mut self) -> PRes<Statement> {
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
            body: self.parse_statements(Keyword::Done)?,
        })
    }
}
