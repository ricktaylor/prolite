use super::*;
use operators::Operator;
use read_term::term::Term;

#[derive(Debug)]
pub(crate) enum Error {
    ReadTerm(read_term::error::Error),
    StreamResolver(Term, std::io::Error),
    InvalidHead(Term),
    NotCallable(Term),
    UnknownDirective(Term),
    BadStreamName(Term),
    AlterBuiltin(Term),
    AlreadyNotDirective(Term, stream::Span),
    AlreadyNotDynamic(Term, stream::Span),
    AlreadyNotDiscontiguous(Term, stream::Span),
    AlreadyNotMultifile(Term, stream::Span),
    NotDiscontiguous(Term, stream::Span),
    NotMultifile(Term, stream::Span, String),
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
