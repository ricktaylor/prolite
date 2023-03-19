use super::*;
use operators::Operator;
use read_term::term::Term;

#[derive(Debug)]
pub(super) enum ErrorKind {
    ReadTerm(read_term::error::ErrorKind),
    StreamResolverError(StreamResolverError),
    NotCallableTerm(Term),
    UnknownDirective(Term),
    BadStreamName(Term),
    IncludeLoop(String),
    InvalidOperator(Term),
    InvalidOpPriority(Term),
    InvalidOpSpecifier(Term),
    InvalidOpCombo(Term, Operator, Operator),
    InvalidFlag(Term),
    InvalidFlagValue(Term, Term),
}

#[derive(Debug)]
pub(super) struct Error {
    pub kind: ErrorKind,
}

impl Error {
    pub(super) fn new<T>(kind: ErrorKind) -> Result<T, Error> {
        Err(Self { kind })
    }
}

impl From<StreamResolverError> for Error {
    fn from(e: StreamResolverError) -> Self {
        Error {
            kind: ErrorKind::StreamResolverError(e),
        }
    }
}

impl From<stream::Error> for Error {
    fn from(e: stream::Error) -> Self {
        Error {
            kind: ErrorKind::ReadTerm(read_term::error::ErrorKind::StreamError(e)),
        }
    }
}

impl From<read_term::error::Error> for Error {
    fn from(e: read_term::error::Error) -> Self {
        Error {
            kind: ErrorKind::ReadTerm(e.kind),
        }
    }
}
