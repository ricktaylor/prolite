use super::*;
use operators::Operator;
use read_term::term::Term;

#[derive(Debug)]
pub(super) enum ErrorKind {
    ReadTerm(read_term::error::Error),
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
    InvalidCharacter(Term),
}

#[derive(Debug)]
pub(super) struct Error {
    pub kind: ErrorKind,
}

impl Error {
    pub(super) fn new<T>(kind: ErrorKind) -> Result<T, Box<Error>> {
        Err(Box::new(Self { kind }))
    }
}

impl From<read_term::error::Error> for Error {
    fn from(e: read_term::error::Error) -> Self {
        Error {
            kind: ErrorKind::ReadTerm(e),
        }
    }
}

impl From<Box<read_term::error::Error>> for Box<Error> {
    fn from(e: Box<read_term::error::Error>) -> Self {
        Box::new(Error::from(*e))
    }
}

impl From<StreamResolverError> for Box<Error> {
    fn from(e: StreamResolverError) -> Self {
        Box::new(Error {
            kind: ErrorKind::StreamResolverError(e),
        })
    }
}
