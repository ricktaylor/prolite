use super::*;
use operators::Operator;
use read_term::term::Term;
use stream::{Position, Span};

#[derive(Debug)]
pub(super) enum Error {
    ReadTerm(read_term::error::Error),
    StreamResolver(Term, std::io::Error),
    InvalidHead(Term),
    UnknownDirective(Term),
    BadStreamName(Term),
    AlterBuiltin(Term),
    AlreadyNotDynamic(Term, stream::Span),
    AlreadyNotDiscontiguous(Term, stream::Span),
    AlreadyNotMultifile(Term, stream::Span),
    NotDiscontiguous(Term, stream::Span),
    NotMultifile(Term, stream::Span),
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

impl Error {
    pub(super) fn new<T>(e: Error) -> Result<T, Box<Error>> {
        Err(Box::new(e))
    }
}

impl From<read_term::error::Error> for Error {
    fn from(e: read_term::error::Error) -> Self {
        Error::ReadTerm(e)
    }
}

impl From<Box<read_term::error::Error>> for Box<Error> {
    fn from(e: Box<read_term::error::Error>) -> Self {
        Box::new(Error::from(*e))
    }
}
