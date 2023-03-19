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
    StreamError(stream::Error),
    ExpectedChar(char),
    UnexpectedToken(Token),
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
}

#[derive(Debug)]
pub(crate) struct Error {
    pub kind: ErrorKind,
    pub location: Span,
}

impl Error {
    pub(super) fn new<T>(kind: ErrorKind, location: Span) -> Result<T, Error> {
        Err(Self { kind, location })
    }
}

impl From<stream::Error> for Error {
    fn from(e: stream::Error) -> Self {
        Error {
            kind: ErrorKind::StreamError(e),
            location: e.location.into(),
        }
    }
}
