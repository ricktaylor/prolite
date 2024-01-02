use super::*;
use operators::Operator;
use read_term::term::Term;
use std::rc::Rc;

#[derive(Debug)]
pub(crate) enum Error {
    ReadTerm(read_term::error::Error),
    StreamResolver(Rc<Term>, std::io::Error),
    InvalidHead(Rc<Term>),
    NotCallable(Rc<Term>),
    UnknownDirective(Rc<Term>),
    BadStreamName(Rc<Term>),
    AlterBuiltin(Rc<Term>),
    AlreadyNotDirective(Rc<Term>, Option<stream::Span>),
    AlreadyNotDynamic(Rc<Term>, Option<stream::Span>),
    AlreadyNotDiscontiguous(Rc<Term>, Option<stream::Span>),
    AlreadyNotMultifile(Rc<Term>, Option<stream::Span>),
    NotDiscontiguous(Rc<Term>, Option<stream::Span>),
    NotMultifile(Rc<Term>, Option<stream::Span>, String),
    IncludeLoop(String),
    InvalidOperator(Rc<Term>),
    InvalidOpPriority(Rc<Term>),
    InvalidOpSpecifier(Rc<Term>),
    InvalidOpCombo(Rc<Term>, Operator, Operator),
    InvalidFlag(Rc<Term>),
    InvalidFlagValue(Rc<Term>, Rc<Term>),
    InvalidCharacter(Rc<Term>),
    InvalidPredicateIndicator(Rc<Term>),
}

impl Error {
    pub(super) fn new<T>(e: Error) -> Result<T, Box<Error>> {
        Err(Box::new(e))
    }
}
