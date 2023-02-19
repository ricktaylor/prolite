use super::context::Context;

mod lexer;
mod parser;

pub enum StreamError {
    IOError(std::io::Error)
}

impl From<std::io::Error> for StreamError {
    fn from(e: std::io::Error) -> Self {
        StreamError::IOError(e)
    }
}

pub trait Stream {
	fn get(&self) -> Result<Option<char>,StreamError>;
	fn peek(&self) -> Result<Option<char>,StreamError>;
}
