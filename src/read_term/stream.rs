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
            column: 1,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Option<Position>,
}

pub trait ReadStream {
    fn get(&mut self) -> Result<Option<char>, std::io::Error>;
    fn peek(&mut self) -> Result<Option<char>, std::io::Error>;
    fn position(&self) -> Option<Position>;
}
