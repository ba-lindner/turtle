use indexmap::IndexMap;

use pos::*;
use prog::parser::ParseError;

pub use ccomp::CComp;
pub use debugger::Debugger;
pub use prog::TProgram;

mod ccomp;
mod debugger;
//mod sr_parser;
mod pos;
mod prog;
pub mod tokens;

pub type SymbolTable = IndexMap<String, Identified>;

/// Types of things that have an identifier
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Identified {
    Unknown,
    Path,
    Calc,
    GlobalVar,
    LocalVar,
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
    LexErrors(Vec<Pos<prog::lexer::LexError>>),
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
