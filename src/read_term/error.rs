use super::*;
use token::*;
use crate::error::ErrorInfo;

#[derive(Debug)]
pub(super) enum ErrorKind {
	Missing(char),
	BadEscape(String),
	BadFloat(String),
	BadInteger(String),
	Unexpected(String),
    StreamError(StreamError),
    ExpectedToken(Token),
    UnexpectedToken(Token),
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError)
}

#[derive(Debug)]
pub(super) struct Error {
    pub kind: ErrorKind,
    pub info: Option<ErrorInfo>
}

impl Error {
    pub(super) fn new<T>(kind: ErrorKind) -> Result<T, Error> {
        Err(Self {
            kind,
            info: Option::<ErrorInfo>::None
        })
    }
}

impl From<StreamError> for Error {
    fn from(e: StreamError) -> Self {
        Error{ kind: ErrorKind::StreamError(e), info: Option::<ErrorInfo>::None }
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Self {
        Error{ kind: ErrorKind::ParseIntError(e), info: Option::<ErrorInfo>::None }
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(e: std::num::ParseFloatError) -> Self {
        Error{ kind: ErrorKind::ParseFloatError(e), info: Option::<ErrorInfo>::None }
    }
}
