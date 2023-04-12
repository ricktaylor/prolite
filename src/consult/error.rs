use super::*;
use operators::Operator;
use read_term::term::Term;
use stream::{Position, Span};

#[derive(Debug)]
pub(super) enum ErrorKind {
    ReadTerm(read_term::error::Error),
    StreamResolverError(StreamResolverError),
    NotCallableTerm(Term),
    UnknownDirective(Term),
    BadStreamName(Term),
    NotDiscontiguous(String),
    NotMultifile(String),
    IncludeLoop(String),
    InvalidOperator(Term),
    InvalidOpPriority(Term),
    InvalidOpSpecifier(Term),
    InvalidOpCombo(Term, Operator, Operator),
    InvalidFlag(Term),
    InvalidFlagValue(Term, Term),
    InvalidCharacter(Term),
    InvalidPredicateIndicator(Term),
}

#[derive(Debug)]
pub(super) struct Error {
    pub kind: ErrorKind,
    pub location: Span,
}

impl Error {
    pub(super) fn new<T>(location: Span, kind: ErrorKind) -> Result<T, Box<Error>> {
        Err(Box::new(Self { kind, location }))
    }
}

impl From<read_term::error::Error> for Error {
    fn from(e: read_term::error::Error) -> Self {
        Error {
            location: e.location.clone(),
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
        let s = e.path.clone();
        Box::new(Error {
            kind: ErrorKind::StreamResolverError(e),
            location: Span::from(&Position {
                source: s,
                ..Default::default()
            }),
        })
    }
}
