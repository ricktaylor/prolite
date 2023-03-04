use super::*;
use token::*;
use term::*;


#[derive(Debug)]
pub enum ErrorKind {
	Missing(char),
	BadEscape(String),
	BadFloat(String),
	BadInteger(String),
	Unexpected(String),
    StreamError(StreamError),
    ExpectedToken(Token),
    UnexpectedToken(Token),
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
    NotCallableTerm(Term),
    Instantiation(Term),
    UnknownDirective(Term),
    BadStreamName(Term),
    IncludeLoop(String),
    StreamResolverError(StreamResolverError)
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind
}

impl Error {
    pub fn new<T>(kind: ErrorKind) -> Result<T, Error> {
        Err(Self {
            kind
        })
    }
}

impl From<StreamResolverError> for Error {
    fn from(e: StreamResolverError) -> Self {
        Error{ kind: ErrorKind::StreamResolverError(e) }
    }
}

impl From<StreamError> for Error {
    fn from(e: StreamError) -> Self {
        Error{ kind: ErrorKind::StreamError(e) }
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Self {
        Error{ kind: ErrorKind::ParseIntError(e) }
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(e: std::num::ParseFloatError) -> Self {
        Error{ kind: ErrorKind::ParseFloatError(e) }
    }
}
