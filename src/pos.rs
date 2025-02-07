use std::fmt::Display;

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