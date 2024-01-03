#[derive(Debug, Clone)]
pub struct Position {
    pub source: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub struct Span {
    pub start: Position,
    pub end: Option<Position>,
}

pub trait ReadStream {
    fn get(&mut self) -> Result<Option<char>, std::io::Error>;
    fn peek(&mut self) -> Result<Option<char>, std::io::Error>;
    fn position(&self) -> Option<Position>;
}
