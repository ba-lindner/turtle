use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

use clap::ValueEnum;

use crate::{
    FilePos, Identified, Spanned,
    features::{Feature, FeatureConf},
    prog::Otherwise,
    tokens::{Keyword, PredefVar},
};
use crate::{Positionable, SymbolTable};

#[cfg(test)]
mod test;

pub type LResult = Result<Spanned<LexToken>, Spanned<LexError>>;

#[derive(PartialEq, Debug)]
pub struct Lexer<'p> {
    chars: Vec<char>,
    offset: usize,
    pub line: usize,
    column: usize,
    last_col: usize,
    start: bool,
    symbols: &'p mut SymbolTable,
    features: &'p mut FeatureConf,
}

impl<'p> Lexer<'p> {
    pub fn new(
        symbols: &'p mut SymbolTable,
        features: &'p mut FeatureConf,
        iter: impl Iterator<Item = char>,
    ) -> Self {
        Self {
            chars: iter.collect(),
            offset: 0,
            line: 1,
            column: 1,
            last_col: 1,
            start: true,
            symbols,
            features,
        }
    }

    pub fn collect_tokens(&mut self) -> Result<Vec<Spanned<LexToken>>, Vec<Spanned<LexError>>> {
        let mut res = Vec::new();
        let mut errs = Vec::new();
        for lres in self {
            match lres {
                Ok(token) => res.push(token),
                Err(err) => errs.push(err),
            }
        }
        errs.otherwise(res)
    }

    pub fn next_token(&mut self) -> Option<LResult> {
        self.skip_comment();
        self.start = false;
        let start = FilePos::new(self.line, self.column);
        let r = match self.next_char()? {
            '@' => self.match_glob_var(),
            '.' => {
                self.put_back();
                self.match_num_literal('0')
            }
            '\'' => self.match_string_literal(),
            c if c.is_ascii_digit() => self.match_num_literal(c),
            c if c.is_alphabetic() || c == '_' => self.match_identifier(),
            c => Ok(LexToken::Symbol(c)),
        };
        let span = start.to(self.last_pos());
        Some(r.map(|t| t.with_span(span)).map_err(|e| e.with_span(span)))
    }

    fn match_num_literal(&mut self, c: char) -> Result<LexToken, LexError> {
        #[derive(PartialEq, Eq)]
        enum NumState {
            Initial,
            Fraction,
            Exponent,
            OtherBase,
        }

        let (base, mut str) = self.get_base(c);
        let mut state = if base == 10 {
            NumState::Initial
        } else {
            NumState::OtherBase
        };
        while let Some(c) = self.next_char() {
            if c == '.' && state == NumState::Initial {
                state = NumState::Fraction;
            } else if (c == 'e' || c == 'E')
                && (state == NumState::Fraction || state == NumState::Initial)
            {
                state = NumState::Exponent;
                if let Some(next) = self.lookahead()
                    && (next == '+' || next == '-')
                {
                    str.push(next);
                    self.next_char();
                }
            } else if !c.is_alphanumeric() {
                self.put_back();
                break;
            }
            str.push(c);
        }
        match state {
            NumState::Initial => Ok(LexToken::IntLiteral(str.parse::<i64>()?)),
            NumState::OtherBase => Ok(LexToken::IntLiteral(i64::from_str_radix(&str, base)?)),
            _ => Ok(LexToken::FloatLiteral(str.parse::<f64>()?)),
        }
    }

    fn get_base(&mut self, c: char) -> (u32, String) {
        let mut str = String::from(c);
        let base = if c == '0'
            && let Some(c_next) = self.lookahead()
        {
            match c_next {
                'b' => 2,
                'o' => 8,
                'x' => 16,
                _ => 10,
            }
        } else {
            10
        };
        if base != 10 {
            str.clear();
            self.next_char();
        }
        (base, str)
    }

    fn match_glob_var(&mut self) -> Result<LexToken, LexError> {
        let str = self.get_identifier();
        if let Ok(var) = str.parse() {
            Ok(LexToken::PredefVar(var))
        } else if let Some(idx) = self.symbols.get_index_of(&str) {
            Ok(LexToken::GlobalVar(idx))
        } else {
            let idx = self.symbols.len();
            self.symbols.insert(str, Identified::GlobalVar);
            Ok(LexToken::GlobalVar(idx))
        }
    }

    fn match_string_literal(&mut self) -> Result<LexToken, LexError> {
        self.features
            .expect(Feature::Types)
            .map_err(LexError::MissingFeature)?;
        let mut acc = String::new();
        loop {
            match self.next_char().ok_or(LexError::UnclosedString)? {
                '\'' => return Ok(LexToken::StringLiteral(acc)),
                '\\' => acc.push(match self.next_char().ok_or(LexError::UnclosedString)? {
                    '\'' => '\'',
                    '\\' => '\\',
                    'n' => '\n',
                    't' => '\t',
                    c => return Err(LexError::UnknownEscapeChar(c)),
                }),
                c => acc.push(c),
            }
        }
    }

    fn match_identifier(&mut self) -> Result<LexToken, LexError> {
        self.put_back();
        let str = self.get_identifier();
        if let Ok(kw) = str.parse::<Keyword>()
            && kw.enabled(self.features)
        {
            return Ok(LexToken::Keyword(kw));
        }
        if let Some(idx) = self.symbols.get_index_of(&str) {
            Ok(LexToken::Identifier(idx))
        } else {
            let idx = self.symbols.len();
            self.symbols.insert(str, Identified::Unknown);
            Ok(LexToken::Identifier(idx))
        }
    }

    fn get_identifier(&mut self) -> String {
        let mut str = String::new();
        while let Some(c) = self.lookahead() {
            if c == '_' || c.is_alphanumeric() {
                str.push(self.next_char().unwrap());
            } else {
                break;
            }
        }
        str
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.lookahead()?;
        self.offset += 1;
        if c == '\n' {
            self.line += 1;
            self.last_col = self.column;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(c)
    }

    fn last_pos(&self) -> FilePos {
        FilePos::new(
            self.line,
            if self.column == 1 {
                self.last_col
            } else {
                self.column
            },
        )
    }

    fn lookahead(&self) -> Option<char> {
        self.chars.get(self.offset).cloned()
    }

    fn put_back(&mut self) {
        if self.offset > 0 {
            self.offset -= 1;
            if self.column > 1 {
                self.column -= 1;
            } else {
                self.column = self.last_col;
            }
        }
    }

    fn skip_comment(&mut self) {
        let mut comm = false;
        let mut line = String::new();
        while let Some(c) = self.next_char() {
            if !comm {
                if c == '"' {
                    comm = true
                } else if !c.is_whitespace() {
                    self.put_back();
                    return;
                }
            } else if c == '\n' {
                comm = false;
                let mut check_line = |prefix: &str, enabled| {
                    if let Some(feature) = line
                        .trim()
                        .strip_prefix(prefix)
                        .and_then(|f| f.split_whitespace().next())
                        .and_then(|f| Feature::from_str(f, true).ok())
                    {
                        self.features.set_auto(feature, enabled);
                    }
                };
                check_line("+feature ", true);
                check_line("-feature ", false);
                line.clear();
            } else if self.start {
                line.push(c);
            }
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = LResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexToken {
    Symbol(char),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Keyword(Keyword),
    GlobalVar(usize),
    PredefVar(PredefVar),
    Identifier(usize),
}

impl Display for LexToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexToken::Symbol(c) => write!(f, "symbol `{c}`"),
            LexToken::IntLiteral(i) => write!(f, "number `{i}`"),
            LexToken::FloatLiteral(val) => write!(f, "number `{val}`"),
            LexToken::StringLiteral(s) => write!(f, "string `{s}`"),
            LexToken::Keyword(kw) => write!(f, "keyword `{kw}`"),
            LexToken::GlobalVar(v) => write!(f, "global variable #{v}"),
            LexToken::PredefVar(pdv) => write!(f, "global variable `@{}`", pdv.get_str()),
            LexToken::Identifier(id) => write!(f, "identifier #{id}"),
        }
    }
}

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum LexError {
    #[error("{0}")]
    IntParseError(#[from] ParseIntError),
    #[error("{0}")]
    FloatParseError(#[from] ParseFloatError),
    #[error("unknown escape character \\{0}")]
    UnknownEscapeChar(char),
    #[error("string literal not closed")]
    UnclosedString,
    #[error("missing feature {0}")]
    MissingFeature(Feature),
}
