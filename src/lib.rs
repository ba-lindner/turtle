use std::fmt::Display;

use indexmap::IndexMap;

use pos::*;
use prog::parser::ParseError;

pub use ccomp::CComp;
pub use debugger::runner::DebugRunner;
pub use prog::TProgram;

mod ccomp;
mod debugger;
mod pos;
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
    #[error("{}", .0.iter().map(|e| format!("{} at {}\n", **e, e.get_pos())).collect::<String>())]
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
}
