#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub source: String,
    pub line: usize,
    pub column: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            source: String::new(),
            line: 1,
            column: 0,
        }
    }
}

impl Position {
    pub fn linefeed(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    pub fn inc(&mut self) {
        self.column += 1;
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Option<Position>,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self {
            end: if end == start { None } else { Some(end) },
            start,
        }
    }

    pub fn inc(&mut self, b: Position) -> &mut Self {
        self.end = Some(b);
        self
    }

    pub fn append(&mut self, b: Span) {
        self.end = if b.end.is_some() {
            b.end
        } else {
            Some(b.start)
        };
    }

    pub fn join(mut self, b: Span) -> Self {
        self.end = if b.end.is_some() {
            b.end
        } else {
            Some(b.start)
        };
        self
    }
}

impl From<Position> for Span {
    fn from(p: Position) -> Self {
        Span {
            start: p,
            end: None,
        }
    }
}

pub trait ReadStream {
    fn get(&mut self) -> Result<Option<char>, std::io::Error>;
    fn peek(&mut self) -> Result<Option<char>, std::io::Error>;
    fn position(&self) -> Position;
}
