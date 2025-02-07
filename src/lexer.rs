use std::fmt::{Display, Formatter};
use std::io::Result as IOResult;
use std::num::{ParseFloatError, ParseIntError};
use std::path::Path;

use indexmap::IndexMap;

use crate::tokens::predef_vars::PredefVar;
use crate::Positionable;
use crate::{tokens::keywords::Keyword, FilePos, Identified, Pos};

pub type LResult = Pos<Result<LexToken, LexError>>;

// Aufwand: bisher ~8h

#[derive(PartialEq, Debug)]
pub struct Lexer<'a> {
    chars: Vec<char>,
    offset: usize,
    line: usize,
    column: usize,
    last_col: usize,
    identifiers: &'a mut IndexMap<String, Identified>,
}

impl<'a> Lexer<'a> {
    pub fn new(
        identifiers: &'a mut IndexMap<String, Identified>,
        iter: impl Iterator<Item = char>,
    ) -> Self {
        Self {
            chars: iter.collect(),
            offset: 0,
            line: 1,
            column: 1,
            last_col: 1,
            identifiers,
        }
    }

    pub fn from_file(
        identifiers: &'a mut IndexMap<String, Identified>,
        filename: &str,
    ) -> IOResult<Self> {
        let path = Path::new(filename);
        let src = std::fs::read_to_string(path)?;
        Ok(Self::new(identifiers, src.chars()))
    }

    pub fn next_token(&mut self) -> Option<LResult> {
        self.skip_comment();
        let (line, column) = (self.line, self.column);
        let r = match self.next_char()? {
            '@' => self.match_glob_var(),
            '.' => {
                self.put_back();
                self.match_num_literal('0')
            }
            c if c.is_ascii_digit() => self.match_num_literal(c),
            c if c.is_alphabetic() || c == '_' => self.match_identifier(),
            c => Ok(LexToken::Symbol(c)),
        };
        Some(r.attach_pos(FilePos::new(line, column)))
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
            } else if (c == 'e' || c == 'E') && state == NumState::Fraction {
                state = NumState::Exponent;
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
        let base = if c != '0' {
            10
        } else if let Some(c_next) = self.lookahead() {
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
        } else if let Some(idx) = self.identifiers.get_index_of(&str) {
            Ok(LexToken::GlobalVar(idx))
        } else {
            let idx = self.identifiers.len();
            self.identifiers.insert(str, Identified::GlobalVar);
            Ok(LexToken::GlobalVar(idx))
        }
    }

    fn match_identifier(&mut self) -> Result<LexToken, LexError> {
        self.put_back();
        let str = self.get_identifier();
        if let Ok(kw) = str.parse::<Keyword>() {
            Ok(LexToken::Keyword(kw))
        } else if let Some(idx) = self.identifiers.get_index_of(&str) {
            Ok(LexToken::Identifier(idx))
        } else {
            let idx = self.identifiers.len();
            self.identifiers.insert(str, Identified::Unknown);
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LexToken {
    Symbol(char),
    IntLiteral(i64),
    FloatLiteral(f64),
    Keyword(Keyword),
    GlobalVar(usize),
    PredefVar(PredefVar),
    Identifier(usize),
}

#[derive(Debug, PartialEq)]
pub enum LexError {
    EndOfInput,
    UnknownEscapeCharacter(char),
    CharLiteralNotClosed,
    IntParseError(ParseIntError),
    FloatParseError(ParseFloatError),
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::EndOfInput => write!(f, "End of input reached before token was completed"),
            Self::UnknownEscapeCharacter(c) => write!(f, "Unknown escape character '{c}'"),
            Self::CharLiteralNotClosed => write!(f, "Char literal not closed"),
            Self::IntParseError(err) => err.fmt(f),
            Self::FloatParseError(err) => err.fmt(f),
        }
    }
}

impl From<ParseIntError> for LexError {
    fn from(value: ParseIntError) -> Self {
        Self::IntParseError(value)
    }
}

impl From<ParseFloatError> for LexError {
    fn from(value: ParseFloatError) -> Self {
        Self::FloatParseError(value)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! lex_this {
        ($lex:ident, $code:expr) => {
            let mut ident = IndexMap::<String, Identified>::new();
            #[allow(unused_mut)]
            let mut $lex = Lexer::new(&mut ident, $code.chars());
        };
    }

    #[test]
    fn empty() {
        lex_this!(lex, "");
        assert_eq!(lex.lookahead(), None);
        assert_eq!(lex.next_char(), None);
        assert_eq!(lex.next_token(), None);
    }

    #[test]
    fn leading_whitespace() {
        lex_this!(lex, "     \n\n \t\t\n   \t\n  *");
        lex.skip_comment();
        assert_eq!(lex.next_char(), Some('*'));
    }

    #[test]
    fn symbols() {
        lex_this!(lex, "/");
        assert_eq!(*lex.next_token().unwrap(), Ok(LexToken::Symbol('/')));
    }

    #[test]
    fn skip_nothing() {
        lex_this!(lex1, "path");
        lex_this!(lex2, "path");
        lex2.skip_comment();
        assert_eq!(lex1, lex2);
    }

    #[test]
    fn num_literal() {
        lex_this!(lex, ".123");
        assert_eq!(*lex.next().unwrap(), Ok(LexToken::FloatLiteral(0.123)));
    }
}
