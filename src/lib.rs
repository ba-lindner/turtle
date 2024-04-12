use std::fmt::Display;

use indexmap::IndexMap;
use lexer::{LResult, LexError, Lexer};
use parser::{tokens::*, PInput, ParseError, Parser};
pub use interpreter::Interpreter;
pub use ccomp::CComp;

mod lexer;
mod parser;
mod interpreter;
mod ccomp;

// Aufwand: bisher ~2h

/// Keywords of the turtle language.
const KEYWORDS: [&str; 50] = [
    "walk", "back", "jump", "home", "turn", "left", "right", "direction",
    "color", "clear", "stop", "finish", "path", "store", "in", "add",
    "to", "sub", "from", "mul", "by", "div", "mark", "if",
    "then", "else", "endif", "do", "times", "done", "counter", "downto",
    "step", "while", "repeat", "until", "endpath", "calculation", "returns", "endcalc",
    "begin", "end", "sin", "cos", "tan", "sqrt", "rand", "and",
    "or", "not"
];

/// Predefined global variables of the turtle language.
/// 
/// The boolean indicates whether the variable is read-only (`false`) or writable (`true`).
const PREDEF_VARS: [(&str, bool); 20] = [("dir", false), ("dist", false), ("x", false), ("y", false), ("1", false),
("2", false), ("3", false), ("4", false), ("5", false), ("6", false), ("7", false), ("8", false), ("9", false), ("pi", false),
("max_x", true), ("max_y", true), ("delay", true), ("red", true), ("blue", true), ("green", true)];

#[derive(PartialEq, Debug)]
#[derive(Clone, Copy)]
pub enum Identified {
    Unknown,
    Path,
    Calc,
    GlobalVar,
    LocalVar,
}

#[derive(Debug)]
pub struct TProgram {
    pub paths: Vec<PathDef>,
    pub calcs: Vec<CalcDef>,
    pub main: Statements,
}

impl TProgram {
    pub fn new(mut parse: Parser) -> Result<Self, CompError> {
        let mut paths = Vec::new();
        let mut calcs = Vec::new();
        let mut main: Option<Statements> = None;
        while let Some(part) = parse.parse_next() {
            match part? {
                ParseToken::PathDef(name, args, body) => paths.push(PathDef{name, args, body}),
                ParseToken::CalcDef(name, args, body, ret) => calcs.push(CalcDef{name, args, body, ret}),
                ParseToken::StartBlock(stm) => {
                    if main.is_some() {
                        return Err(CompError::MultipleMains);
                    }
                    main = Some(stm)
                },
            }
        }
        let Some(main) = main else {
            return Err(CompError::MissingMain)
        };
        Ok(Self {
            paths,
            calcs,
            main,
        })
    }
}

/// Things that can go wrong.
/// 
/// * Reading the file might return an error
/// * The lexer might find several errors
/// * The parser might find an error. Even if multiple errors exist, only the first is returned.
/// * There might be multiple `begin..end` blocks in a programm
/// * There might be no `begin..end` block in a programm
#[derive(Debug)]
pub enum CompError {
    IOError(std::io::Error),
    LexErrors(Vec<(FilePos, LexError)>),
    ParseError((FilePos, ParseError)),
    MultipleMains,
    MissingMain,
}

impl From<std::io::Error> for CompError {
    fn from(value: std::io::Error) -> Self {
        Self::IOError(value)
    }
}

impl From<(FilePos, ParseError)> for CompError {
    fn from(value: (FilePos, ParseError)) -> Self {
        Self::ParseError(value)
    }
}

#[derive(Debug)]
pub struct PathDef {
    pub name: usize,
    pub args: ArgDefList,
    pub body: Statements,
}

#[derive(Debug)]
pub struct CalcDef {
    pub name: usize,
    pub args: ArgDefList,
    pub body: Statements,
    pub ret: Expr,
}

/// A position in a file.
/// 
/// Used when an error is found while compiling to tell the developer where to fix his code
#[derive(Debug, PartialEq)]
#[derive(Clone, Copy)]
pub struct FilePos {
    line: usize,
    column: usize,
}

impl FilePos {
    /// Create a new [`FilePos`] struct.
    /// 
    /// # Examples
    /// ```
    /// # use turtle::FilePos;
    /// let fp = FilePos::new(10, 20);
    /// ```
    pub fn new(line: usize, column: usize) -> Self {
        Self {
            line,
            column,
        }
    }
}

impl Display for FilePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

/// Parse the given file.
/// 
/// This function reads the file, feeds the contents to the lexer,
/// collects the lexems, and feeds those to the parser.
/// It then tries to construct a turtle programm from the parsed tokens.
/// 
/// # Examples
/// ```
/// # use turtle::parse_file;
/// let prog = parse_file("examples/circle.tg").unwrap();
/// ``` 
pub fn parse_file(filename: &str) -> Result<(TProgram, IndexMap<String, Identified>), CompError> {
    let mut ident = IndexMap::<String, Identified>::new();
    let lex = Lexer::from_file(&mut ident, filename)?;
    let ltokens: Vec<_> = lex.collect();
    let pinp = check_lexems(ltokens)?;
    let parse = Parser::new(&mut ident, pinp);
    Ok((TProgram::new(parse)?, ident))
}

/// Check the results of the lexer.
/// 
/// If at least one part of the input could not be parsed correctly, a [`Vec`] of all errors is returned.
/// Otherwise, a [`Vec`] of all lexems is returned. In both cases, all elements are coupled with a [`FilePos`]
/// to locate possible errors.
fn check_lexems(tokens: Vec<LResult>) -> Result<Vec<PInput>, CompError> {
    let mut errs = Vec::new();
    let mut res = Vec::new();
    for lres in tokens {
        match lres.1 {
            Ok(token) => res.push((lres.0, Some(token))),
            Err(why) => errs.push((lres.0, why)),
        }
    }
    if !errs.is_empty() {
        Err(CompError::LexErrors(errs))
    } else {
        Ok(res)
    }
}

#[cfg(test)]
mod test {
    use self::lexer::LexToken;

    use super::*;

    #[test]
    fn check_lexems_ok() {
        let lres = vec![(FilePos::new(0, 0), Ok(LexToken::Symbol('!')))];
        let checked = check_lexems(lres).unwrap();
        assert_eq!(checked.len(), 1);
        assert_eq!(checked[0].0, FilePos::new(0, 0));
        assert_eq!(checked[0].1, Some(LexToken::Symbol('!')));
    }

    #[test]
    fn check_lexems_err() {
        let fp = FilePos::new(0, 0);
        let lres = vec![(fp, Err(LexError::CharLiteralNotClosed))];
        let checked = check_lexems(lres).unwrap_err();
        let CompError::LexErrors(errs) = checked else {
            panic!("wrong error type");
        };
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].0, fp);
        assert_eq!(errs[0].1, LexError::CharLiteralNotClosed);
    }
}