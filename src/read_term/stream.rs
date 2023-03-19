#[derive(Debug, Clone,PartialEq)]
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
            column: 1,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Option<Position>,
}

impl Span {
    pub fn new(start: &Position, end: &Position) -> Self {
        Self {
            start: start.clone(),
            end: if end == start {
                None
            } else {
                Some(end.clone())
            },
        }
    }

    pub fn concat(a: &Span, b: &Span) -> Span {
        if a == b {
            a.clone()
        } else {
            Span {
                start: a.start,
                end: match b.end {
                    Some(e) => Some(e),
                    None => Some(b.start)
                }
            }
        }
    }
}

impl From<&Position> for Span {
    fn from(p: &Position) -> Self {
        Span {
            start: p.clone(),
            end: None,
        }
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

#[derive(Debug)]
pub struct Error {
    pub error: std::io::Error,
    pub location: Position,
}

pub trait ReadStream {
    fn get(&mut self) -> Result<Option<char>, Error>;
    fn peek(&mut self) -> Result<Option<char>, Error>;
    fn position(&self) -> &Position;
}
