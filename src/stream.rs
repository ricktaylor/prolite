#[derive(Debug)]
pub enum Error {
    IOError(std::io::Error),
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IOError(e)
    }
}

pub struct Position {
    pub source: String,
    pub line: usize,
    pub column: usize,
}

pub trait Stream {
    fn get(&mut self) -> Result<Option<char>, Error>;
    fn peek(&mut self) -> Result<Option<char>, Error>;
}
