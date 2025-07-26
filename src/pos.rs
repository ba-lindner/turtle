use std::{fmt::Display, num::ParseIntError, str::FromStr};

/// A position in a file.
///
/// Used when an error is found while compiling to tell the developer where to fix his code
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct FilePos {
    pub line: usize,
    pub column: usize,
}

impl FilePos {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }

    pub fn is_empty(&self) -> bool {
        self.column == 0 && self.line == 0
    }

    pub fn to(self, other: FilePos) -> Span {
        Span::new(self, other)
    }
}

impl Display for FilePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FilePosParseErr {
    #[error("no delimiter")]
    NoDelimiter,
    #[error("{0}")]
    ParseError(#[from] ParseIntError),
}

impl FromStr for FilePos {
    type Err = FilePosParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (line, column) = s
            .split_once(|c: char| !c.is_ascii_digit())
            .ok_or(FilePosParseErr::NoDelimiter)?;
        Ok(Self::new(line.parse()?, column.parse()?))
    }
}

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: FilePos,
    pub end: FilePos,
}

impl Span {
    pub fn new(start: FilePos, end: FilePos) -> Self {
        Self { start, end }
    }

    pub fn to(&self, other: Span) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
}

impl From<(FilePos, FilePos)> for Span {
    fn from((start, end): (FilePos, FilePos)) -> Self {
        Self { start, end }
    }
}

impl<T> From<Spanned<T>> for Span {
    fn from(value: Spanned<T>) -> Self {
        value.get_span()
    }
}

impl<T> From<&'_ Spanned<T>> for Span {
    fn from(value: &'_ Spanned<T>) -> Self {
        value.get_span()
    }
}

impl<T> From<&'_ mut Spanned<T>> for Span {
    fn from(value: &'_ mut Spanned<T>) -> Self {
        value.get_span()
    }
}

// impl<T, U> From<(Spanned<T>, Spanned<U>)> for Span {
//     fn from(value: (Spanned<T>, Spanned<U>)) -> Self {
//         value.0.span.to(value.1.span)
//     }
// }

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} - {}", self.start, self.end)
    }
}

/// Attach [`FilePos`] to any type `T`, mostly tokens
///
/// Implements [`Deref`](std::ops::Deref) to access inner value
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spanned<T> {
    token: T,
    span: Span,
}

impl<T> Spanned<T> {
    /// Create new [`Pos`] wrapper.
    pub fn new(token: T, span: impl Into<Span>) -> Self {
        Self {
            token,
            span: span.into(),
        }
    }

    pub fn get_span(&self) -> Span {
        self.span
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        let Self { token, span } = self;
        Spanned {
            token: f(token),
            span,
        }
    }

    pub fn into_inner(self) -> T {
        self.token
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.token
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.token, self.span)
    }
}

pub trait Positionable: Sized {
    fn with_span(self, span: impl Into<Span>) -> Spanned<Self>;
}

impl<T> Positionable for T {
    fn with_span(self, span: impl Into<Span>) -> Spanned<Self> {
        Spanned::new(self, span)
    }
}
