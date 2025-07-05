use std::fmt::{Display, Write as _};

use debugger::FuncType;
use indexmap::IndexMap;

use pos::*;
use prog::{TypeError, parser::ParseError};

pub use ccomp::CComp;
pub use prog::TProgram;
use tokens::{EventKind, ValType};
#[cfg(feature = "examples")]
pub use turtle_examples as examples;

mod ccomp;
pub mod debugger;
pub mod features;
pub mod pos;
mod prog;
pub mod tokens;

/// A map of all identifiers in a turtle program
pub type SymbolTable = IndexMap<String, Identified>;

/// Types of things that have an identifier
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Identified {
    /// Initial value
    Unknown,
    /// Path
    Path,
    /// Calculation
    Calc,
    /// Global variable
    GlobalVar,
    /// Local variable
    LocalVar,
}

impl Display for Identified {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identified::Unknown => write!(f, "unknown"),
            Identified::Path => write!(f, "path"),
            Identified::Calc => write!(f, "calculation"),
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
    #[error("type could not be inferred for local variables {} in {}", .1.iter().fold(String::new(), |mut acc, idx| {
        let _ = write!(acc, "#{idx}, ");
        acc
    }), .0)]
    UndefLocals(FuncType, Vec<usize>),
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

impl Disp for TurtleError {
    fn disp(&self, f: &mut std::fmt::Formatter<'_>, symbols: &SymbolTable) -> std::fmt::Result {
        let sym = |id| symbols.get_index(id).unzip().0.unwrap();
        match self {
            TurtleError::UnidentifiedIdentifier(id) => {
                write!(f, "identifier '{}' not used", sym(*id))
            }
            TurtleError::MissingDefinition(id) => write!(f, "no function '{}'", sym(*id)),
            TurtleError::UndefGlobals(vars) => {
                f.write_str("type could not be inferred for global variables ")?;
                for (idx, var) in vars.iter().enumerate() {
                    if idx > 0 {
                        f.write_str(", ")?;
                    }
                    f.write_str(sym(*var))?;
                }
                Ok(())
            }
            TurtleError::UndefLocals(func, vars) => {
                f.write_str("type could not be inferred for local variables ")?;
                for (idx, var) in vars.iter().enumerate() {
                    if idx > 0 {
                        f.write_str(", ")?;
                    }
                    f.write_str(sym(*var))?;
                }
                write!(f, " in {}", func.with_symbols(symbols))
            }
            _ => self.fmt(f),
        }
    }
}

pub trait Disp {
    fn disp(&self, f: &mut std::fmt::Formatter<'_>, symbols: &SymbolTable) -> std::fmt::Result;

    fn with_symbols<'s>(&'s self, symbols: &'s SymbolTable) -> Symbolized<'s, Self>
    where
        Self: Sized,
    {
        Symbolized(self, symbols)
    }
}

pub struct Symbolized<'s, T: Disp>(&'s T, &'s SymbolTable);

impl<'s, T: Disp> Display for Symbolized<'s, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.disp(f, self.1)
    }
}
