use std::fmt::Display;

pub use ccomp::CComp;
use indexmap::IndexMap;
use interpreter::window::Window;
pub use interpreter::{debugger::{Debugger, StepVariant}, Interpreter};
use lexer::{LResult, LexError, LexToken, Lexer};
use parser::{ParseError, Parser};
use tokens::*;

mod ccomp;
mod interpreter;
mod lexer;
mod parser;
//mod sr_parser;
mod tokens;

/// Types of things that have an identifier
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Identified {
    Unknown,
    Path,
    Calc,
    GlobalVar,
    LocalVar,
}

/// A full and valid turtle program.
#[derive(Debug)]
pub struct TProgram {
    pub paths: Vec<PathDef>,
    pub calcs: Vec<CalcDef>,
    pub main: Statements,
    pub idents: IndexMap<String, Identified>,
}

impl TProgram {
    fn get_path(&self, name: usize) -> Result<&PathDef, TurtleError> {
        self.paths
            .iter()
            .find(|path| path.name == name)
            .ok_or(TurtleError::MissingDefinition(name))
    }

    fn get_calc(&self, name: usize) -> Result<&CalcDef, TurtleError> {
        self.calcs
            .iter()
            .find(|calc| calc.name == name)
            .ok_or(TurtleError::MissingDefinition(name))
    }

    pub fn interpret(&self, args: &[String]) {
        let window = Window::new("Turtle Interpreter");
        Interpreter::new(self, &window, args).run().unwrap();
    }

    pub fn debug(&self, args: &[String], step: StepVariant, bp: &[String]) {
        let window = Window::new("Turtle Debugger");
        let mut dbg = Debugger::new(self, &window, args);
        dbg.step(step);
        dbg.breakpoints(bp);
        dbg.run();
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
pub enum TurtleError {
    IOError(std::io::Error),
    LexErrors(Vec<Pos<LexError>>),
    ParseError(Pos<ParseError>),
    MultipleMains,
    MissingMain,
    UnidentifiedIdentifier(usize),
    MissingDefinition(usize),
}

impl From<std::io::Error> for TurtleError {
    fn from(value: std::io::Error) -> Self {
        Self::IOError(value)
    }
}

impl From<Pos<ParseError>> for TurtleError {
    fn from(value: Pos<ParseError>) -> Self {
        Self::ParseError(value)
    }
}

/// Path definition in turtle program
#[derive(Debug)]
pub struct PathDef {
    pub name: usize,
    pub args: ArgDefList,
    pub body: Statements,
}

/// Calc definition in turtle program
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
#[derive(Debug, Default, PartialEq, Clone, Copy, PartialOrd, Eq)]
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
        Self { line, column }
    }
}

impl Display for FilePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

impl Ord for FilePos {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.line
            .cmp(&other.line)
            .then(self.column.cmp(&other.column))
    }
}

/// Attach [`FilePos`] to any type `T`, mostly tokens
///
/// Implements [`Deref`](std::ops::Deref) to access inner value
#[derive(Debug, PartialEq)]
pub struct Pos<T> {
    pos: FilePos,
    token: T,
}

impl<T> Pos<T> {
    /// Create new [`Pos`] wrapper.
    pub fn new(token: T, pos: FilePos) -> Self {
        Self { pos, token }
    }

    /// Get attached [`FilePos`]
    pub fn get_pos(&self) -> FilePos {
        self.pos
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Pos<U> {
        let Self { pos, token } = self;
        Pos {
            pos,
            token: f(token),
        }
    }

    pub fn into_inner(self) -> T {
        self.token
    }
}

impl<T> std::ops::Deref for Pos<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

impl<T> std::ops::DerefMut for Pos<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.token
    }
}

pub trait Positionable: Sized {
    fn attach_pos(self, pos: FilePos) -> Pos<Self>;
}

impl<T> Positionable for T {
    fn attach_pos(self, pos: FilePos) -> Pos<Self> {
        Pos::new(self, pos)
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
pub fn parse_file(filename: &str) -> Result<TProgram, TurtleError> {
    let mut idents = IndexMap::<String, Identified>::new();
    let lex = Lexer::from_file(&mut idents, filename)?;
    let ltokens: Vec<_> = lex.collect();
    let pinp = check_lexems(ltokens)?;
    let parse = Parser::new(&mut idents, pinp);
    let (main, paths, calcs) = collect_tokens(parse)?;
    for (id, (_, kind)) in idents.iter().enumerate() {
        match kind {
            Identified::Unknown => {
                return Err(TurtleError::UnidentifiedIdentifier(id));
            }
            Identified::Path => {
                if paths.iter().find(|path| path.name == id).is_none() {
                    return Err(TurtleError::MissingDefinition(id));
                }
            }
            Identified::Calc => {
                if calcs.iter().find(|calc| calc.name == id).is_none() {
                    return Err(TurtleError::MissingDefinition(id));
                }
            }
            _ => {}
        }
    }
    Ok(TProgram {
        paths,
        calcs,
        main,
        idents,
    })
}

/// Check the results of the lexer.
///
/// If at least one part of the input could not be parsed correctly, a [`Vec`] of all errors is returned.
/// Otherwise, a [`Vec`] of all lexems is returned. In both cases, all elements are coupled with a [`FilePos`]
/// to locate possible errors.
fn check_lexems(tokens: Vec<LResult>) -> Result<Vec<Pos<LexToken>>, TurtleError> {
    let mut errs = Vec::new();
    let mut res = Vec::new();
    for lres in tokens {
        let pos = lres.get_pos();
        match lres.into_inner() {
            Ok(token) => res.push(token.attach_pos(pos)),
            Err(why) => errs.push(why.attach_pos(pos)),
        }
    }
    if !errs.is_empty() {
        Err(TurtleError::LexErrors(errs))
    } else {
        Ok(res)
    }
}

/// Collect top-level tokens from parser
///
/// Sorts out calc definitions, path definitions and main function
///
/// Returns error if no main or multiple mains were found
fn collect_tokens(
    mut parse: Parser<'_>,
) -> Result<(Statements, Vec<PathDef>, Vec<CalcDef>), TurtleError> {
    let mut paths = Vec::new();
    let mut calcs = Vec::new();
    let mut main: Option<Statements> = None;
    while let Some(part) = parse.parse_next() {
        match part? {
            ParseToken::PathDef(name, args, body) => paths.push(PathDef { name, args, body }),
            ParseToken::CalcDef(name, args, body, ret) => calcs.push(CalcDef {
                name,
                args,
                body,
                ret,
            }),
            ParseToken::StartBlock(stm) => {
                if main.is_some() {
                    return Err(TurtleError::MultipleMains);
                }
                main = Some(stm)
            }
        }
    }
    let Some(main) = main else {
        return Err(TurtleError::MissingMain);
    };
    Ok((main, paths, calcs))
}

#[cfg(test)]
mod test {
    use self::lexer::LexToken;

    use super::*;

    #[test]
    fn check_lexems_ok() {
        let lres = vec![Ok(LexToken::Symbol('!')).attach_pos(FilePos::default())];
        let checked = check_lexems(lres).unwrap();
        assert_eq!(checked.len(), 1);
        assert_eq!(checked[0].get_pos(), FilePos::new(0, 0));
        assert_eq!(*checked[0], LexToken::Symbol('!'));
    }

    #[test]
    fn check_lexems_err() {
        let fp = FilePos::new(0, 0);
        let lres = vec![Err(LexError::CharLiteralNotClosed).attach_pos(fp)];
        let checked = check_lexems(lres).unwrap_err();
        let TurtleError::LexErrors(errs) = checked else {
            panic!("wrong error type");
        };
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].get_pos(), fp);
        assert_eq!(*errs[0], LexError::CharLiteralNotClosed);
    }
}
