use std::fmt::{Display, Write as _};

use indexmap::IndexMap;

use pos::*;
use prog::{parser::ParseError, TypeError};

pub use ccomp::CComp;
pub use prog::TProgram;
use tokens::{EventKind, ValType};

mod ccomp;
pub mod debugger;
pub mod features;
pub mod pos;
mod prog;
pub mod tokens;

pub type SymbolTable = IndexMap<String, Identified>;

/// Types of things that have an identifier
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Identified {
    Unknown,
    Path(usize),
    Calc(usize),
    GlobalVar,
    LocalVar,
}

impl Display for Identified {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identified::Unknown => write!(f, "unknown"),
            Identified::Path(args) => write!(f, "path with {args} arguments"),
            Identified::Calc(args) => write!(f, "calculation with {args} arguments"),
            Identified::GlobalVar => write!(f, "global variable"),
            Identified::LocalVar => write!(f, "local variable"),
        }
    }
}

/// Things that can go wrong.
///
/// * Reading the file might return an error
/// * The lexer might find several errors
/// * The parser might find an error. Even if multiple errors exist, only the first is returned.
/// * There might be multiple `begin..end` blocks in a programm
/// * There might be no `begin..end` block in a programm
#[derive(Debug, thiserror::Error)]
pub enum TurtleError {
    #[error("{0}")]
    IOError(#[from] std::io::Error),
    #[error("{}", .0.iter().fold(String::new(), |mut acc, e| {
        let _ = writeln!(acc, "{} at {}", **e, e.get_pos());
        acc
    }))]
    LexErrors(Vec<Pos<prog::lexer::LexError>>),
    #[error("{} at {}", **.0, .0.get_pos())]
    ParseError(#[from] Pos<ParseError>),
    #[error("multiple mains defined at {0} and {1}")]
    MultipleMains(FilePos, FilePos),
    #[error("missing main block")]
    MissingMain,
    #[error("identifier #{0} not used")]
    UnidentifiedIdentifier(usize),
    #[error("no function #{0}")]
    MissingDefinition(usize),
    #[error("type could not be inferred for global variables {}", .0.iter().fold(String::new(), |mut acc, idx| {
        let _ = write!(acc, "#{idx}, ");
        acc
    }))]
    UndefGlobals(Vec<usize>),
    #[error("type could not be inferred for some local variables")]
    UndefLocals,
    #[error("{0} at {1}")]
    TypeError(TypeError, FilePos),
    #[error("{0} at {1} - {2}")]
    TypeErrorSpan(TypeError, FilePos, FilePos),
    #[error("multiple handlers for {0} events defined at {1}, {2}")]
    MultipleEventHandler(EventKind, FilePos, FilePos),
    #[error("{0} event handler has incorrect number of arguments: got {1}, expected {2}")]
    EventArgsLength(EventKind, usize, usize),
    #[error("{0} event handler has wrong type of argument at index {1}: got {2}, expected {3}")]
    EventArgsType(EventKind, usize, ValType, ValType),
}
