mod parser;
mod consult;
mod lexer;
mod multistream;

enum StreamError {
    IOError(std::io::Error)
}

impl From<std::io::Error> for StreamError {
    fn from(e: std::io::Error) -> Self {
        StreamError::IOError(e)
    }
}

trait Stream {
	fn get(&mut self) -> Result<Option<char>,StreamError>;
	fn peek(&mut self) -> Result<Option<char>,StreamError>;
}
