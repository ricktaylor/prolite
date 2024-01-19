use super::*;
use lexer::*;
use stream::Span;

#[derive(Debug)]
pub(crate) enum ErrorKind {
    Missing(char),
    BadEscape(String),
    BadFloat(String),
    BadInteger(String),
    Unexpected(String),
    StreamError(std::io::Error),
    ExpectedChar(char),
    UnexpectedToken(TokenKind),
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
}

#[derive(Debug)]
pub(crate) struct Error {
    pub kind: ErrorKind,
    pub location: Option<Span>,
}

impl Error {
    pub fn new<T>(kind: ErrorKind, location: Option<Span>) -> Result<T, Box<Error>> {
        Err(Box::new(Self { kind, location }))
    }
}
