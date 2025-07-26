use std::fmt::Display;

use debugger::FuncType;
use indexmap::IndexMap;

use pos::*;
use prog::{TypeError, lexer::LexError, parser::ParseError};

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

#[derive(Debug, thiserror::Error)]
pub enum TurtleError {
    #[error("{0}")]
    IOError(#[from] std::io::Error),
    #[error("{}", Commas(.0, "\n"))]
    LexErrors(Vec<Spanned<LexError>>),
    #[error("{}", Commas(.0, "\n"))]
    ParseErrors(Vec<Spanned<ParseError>>),
    #[error("{}", Commas(.0, "\n"))]
    ProgErrors(Vec<ProgError>),
    #[error("{}", Commas(.0, "\n"))]
    TypeErrors(Vec<Spanned<TypeError>>),
}

#[derive(Debug, thiserror::Error)]
pub enum ProgError {
    #[error("multiple mains defined at {}", Commas(.0, ", "))]
    MultipleMains(Vec<Span>),
    #[error("missing main block")]
    MissingMain,
    #[error("identifier #{0} not used")]
    UnidentifiedIdentifier(usize),
    #[error("no function #{0}")]
    MissingDefinition(usize),
    #[error("multiple functions {} defined at: {}", .0, Commas(.1, ", "))]
    MultipleFuncs(FuncType, Vec<Span>),
    #[error("{0} event handler has incorrect number of arguments: got {1}, expected {2}")]
    EventArgsLength(EventKind, usize, usize),
    #[error("{0} event handler has wrong type of argument at index {1}: got {2}, expected {3}")]
    EventArgsType(EventKind, usize, ValType, ValType),
}

struct Commas<T>(pub T, pub &'static str);
impl<T> Display for Commas<T>
where
    T: IntoIterator + Copy,
    <T as IntoIterator>::Item: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, elem) in self.0.into_iter().enumerate() {
            if idx > 0 {
                f.write_str(self.1)?;
            }
            elem.fmt(f)?;
        }
        Ok(())
    }
}

impl Disp for ProgError {
    fn disp(&self, f: &mut std::fmt::Formatter<'_>, symbols: &SymbolTable) -> std::fmt::Result {
        let sym = |id| symbols.get_index(id).unzip().0.unwrap();
        match self {
            ProgError::UnidentifiedIdentifier(id) => {
                write!(f, "identifier '{}' not used", sym(*id))
            }
            ProgError::MissingDefinition(id) => write!(f, "no function '{}'", sym(*id)),
            // ProgError::UndefGlobal(var) => {
            //     write!(
            //         f,
            //         "type could not be inferred for global variable {}",
            //         sym(*var)
            //     )
            // }
            // ProgError::UndefLocal(func, var) => {
            //     write!(
            //         f,
            //         "type could not be inferred for local variable {} in {}",
            //         sym(*var),
            //         func.with_symbols(symbols)
            //     )
            // }
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

impl<T: Disp> Disp for &T {
    fn disp(&self, f: &mut std::fmt::Formatter<'_>, symbols: &SymbolTable) -> std::fmt::Result {
        (*self).disp(f, symbols)
    }
}

impl<T> Disp for Commas<T>
where
    T: IntoIterator + Copy,
    <T as IntoIterator>::Item: Disp,
{
    fn disp(&self, f: &mut std::fmt::Formatter<'_>, symbols: &SymbolTable) -> std::fmt::Result {
        for (idx, elem) in self.0.into_iter().enumerate() {
            if idx > 0 {
                f.write_str(self.1)?;
            }
            elem.disp(f, symbols)?;
        }
        Ok(())
    }
}

pub struct Symbolized<'s, T: Disp>(&'s T, &'s SymbolTable);

impl<'s, T: Disp> Display for Symbolized<'s, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.disp(f, self.1)
    }
}
