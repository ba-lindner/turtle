use std::fmt::{Display, Formatter};
use std::io::Result as IOResult;
use std::num::{ParseFloatError, ParseIntError};
use std::path::Path;

use indexmap::IndexMap;

use crate::{FilePos, Identified};

pub type LResult = (FilePos, Result<LexToken, LexError>);

// Aufwand: bisher ~8h

#[derive(PartialEq, Debug)]
pub struct Lexer<'a> {
    chars: Vec<char>,
    offset: usize,
    line: usize,
    column: usize,
    last_col: usize,
    keywords: IndexMap<&'static str, ()>,
    predef_vars: IndexMap<&'static str, ()>,
    identifiers: &'a mut IndexMap<String, Identified>,
}

impl<'a> Lexer<'a> {
    pub fn new(identifiers: &'a mut IndexMap<String, Identified>, iter: impl Iterator<Item = char>) -> Self {
        Self {
            chars: iter.collect(),
            offset: 0,
            line: 1,
            column: 1,
            last_col: 1,
            keywords: IndexMap::from_iter(crate::KEYWORDS.iter().map(|&k| (k, ()))),
            predef_vars: IndexMap::from_iter(crate::PREDEF_VARS.iter().map(|&(v, _)| (v, ()))),
            identifiers,
        }
    }

    pub fn from_file(identifiers: &'a mut IndexMap<String, Identified>, filename: &str) -> IOResult<Self> {
        let path = Path::new(filename);
        let src = std::fs::read_to_string(path)?;
        Ok(Self::new(identifiers, src.chars()))
    }

    pub fn next_token(&mut self) -> Option<LResult> {
        self.skip_comment();
        let (line, column) = (self.line, self.column);
        let r = match self.next_char()? {
            '\"' => self.match_str_literal(),
            '\'' => self.match_char_literal(),
            '@' => self.match_glob_var(),
            c if c.is_ascii_digit() => self.match_num_literal(c),
            c if c.is_alphabetic() || c == '_' => self.match_identifier(),
            c => Ok(LexToken::Symbol(c)),
        };
        Some((
            FilePos {
                line,
                column,
            },
            r
        ))
    }

    fn match_str_literal(&mut self) -> Result<LexToken, LexError> {
        let mut str = String::new();
        loop {
            let c = self.next_escaped()?;
            if c == '"' {
                break;
            }
            str.push(c);
        }
        Ok(LexToken::StringLiteral(str))
    }

    fn match_char_literal(&mut self) -> Result<LexToken, LexError> {
        let t = LexToken::CharLiteral(self.next_escaped()?);
        if self.next_char() != Some('\'') {
            Err(LexError::CharLiteralNotClosed)
        } else {
            Ok(t)
        }
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
        let mut state = if base == 10 { NumState::Initial } else { NumState::OtherBase };
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
        if let Some(idx) = self.predef_vars.get_index_of(&*str) {
            Ok(LexToken::GlobalVar(idx, true))
        } else if let Some(idx) = self.identifiers.get_index_of(&str) {
            Ok(LexToken::GlobalVar(idx, false))
        } else {
            let idx = self.identifiers.len();
            self.identifiers.insert(str, Identified::GlobalVar);
            Ok(LexToken::GlobalVar(idx, false))
        }
    }

    fn match_identifier(&mut self) -> Result<LexToken, LexError> {
        self.put_back();
        let str = self.get_identifier();
        if let Some(idx) = self.keywords.get_index_of(&*str) {
            Ok(LexToken::Keyword(idx))
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

    fn next_escaped(&mut self) -> Result<char, LexError> {
        let c = self.next_char().ok_or(LexError::EndOfInput)?;
        if c == '\\' {
            match self.next_char().ok_or(LexError::EndOfInput)? {
                'n' => Ok('\n'),
                'r' => Ok('\r'),
                't' => Ok('\t'),
                '\\' => Ok('\\'),
                '\"' => Ok('\"'),
                '\'' => Ok('\''),
                c => Err(LexError::UnknownEscapeCharacter(c)),
            }
        } else {
            Ok(c)
        }
    }

    fn lookahead(&self) -> Option<char> {
        if self.eof() {
            None
        } else {
            Some(self.chars[self.offset])
        }
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
        enum Comment {
            None,
            Line,
            Block,
        }

        let mut comm = Comment::None;
        while let Some(c) = self.next_char() {
            match comm {
                Comment::None => {
                    if c == '/' && self.lookahead() == Some('/') {
                        self.next_char();
                        comm = Comment::Line;
                    } else if c == '/' && self.lookahead() == Some('*') {
                        self.next_char();
                        comm = Comment::Block;
                    } else if !c.is_whitespace() {
                        self.put_back();
                        return;
                    }
                }
                Comment::Line => {
                    if c == '\n' {
                        comm = Comment::None;
                    }
                }
                Comment::Block => {
                    if c == '*' && self.lookahead() == Some('/') {
                        self.next_char();
                        comm = Comment::None;
                    }
                }
            }
        }
    }

    fn eof(&self) -> bool {
        self.offset == self.chars.len()
    }
}

impl Iterator for Lexer<'_> {
    type Item = LResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[derive(Debug)]
#[derive(PartialEq)]
pub enum LexToken {
    Symbol(char),
    IntLiteral(i64),
    FloatLiteral(f64),
    Keyword(usize),
    GlobalVar(usize, bool),
    Identifier(usize),
    StringLiteral(String),
    CharLiteral(char),
}

#[derive(Debug)]
#[derive(PartialEq)]
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
            Self::UnknownEscapeCharacter(c) => write!(f, "Unknown escape character '{}'", c),
            Self::CharLiteralNotClosed => write!(f, "Char literal not closed"),
            Self::IntParseError(err) => write!(f, "{}", err),
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

    #[test]
    fn empty() {
        let mut ident = IndexMap::<String, Identified>::new();
        let mut lex = Lexer::new(&mut ident, "".chars());
        assert_eq!(lex.lookahead(), None);
        assert_eq!(lex.next_char(), None);
        assert_eq!(lex.next_token(), None);
    }

    #[test]
    fn leading_whitespace() {
        let mut ident = IndexMap::<String, Identified>::new();
        let mut lex = Lexer::new(&mut ident, "     \n\n \t\t\n   \t\n  *".chars());
        lex.skip_comment();
        assert_eq!(lex.next_char(), Some('*'));
    }

    #[test]
    fn escaped_chars() {
        let mut ident = IndexMap::<String, Identified>::new();
        let mut lex = Lexer::new(&mut ident, "\\t\\n\\\'\\\"\\_".chars());
        assert_eq!(lex.next_escaped(), Ok('\t'));
        assert_eq!(lex.next_escaped(), Ok('\n'));
        assert_eq!(lex.next_escaped(), Ok('\''));
        assert_eq!(lex.next_escaped(), Ok('\"'));
        assert_eq!(lex.next_escaped(), Err(LexError::UnknownEscapeCharacter('_')));
    }

    #[test]
    fn symbols() {
        let mut ident = IndexMap::<String, Identified>::new();
        let mut lex = Lexer::new(&mut ident, "/".chars());
        assert_eq!(lex.next_token().unwrap().1, Ok(LexToken::Symbol('/')));
    }

    #[test]
    fn skip_nothing() {
        let mut ident1 = IndexMap::<String, Identified>::new();
        let mut ident2 = IndexMap::<String, Identified>::new();
        let lex1 = Lexer::new(&mut ident1, "path".chars());
        let mut lex2 = Lexer::new(&mut ident2, "path".chars());
        lex2.skip_comment();
        assert_eq!(lex1, lex2);
    }
}
