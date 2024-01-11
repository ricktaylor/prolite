use std::rc::Rc;

use super::*;

#[derive(Debug)]
pub(super) struct Compound {
    pub compound: Rc<read_term::Term>,
    pub args: Vec<usize>,
}

impl Compound {
    pub fn functor(&self) -> &String {
        &self.as_compound().functor
    }

    pub fn as_compound(&self) -> &read_term::Compound {
        if let read_term::TermKind::Compound(c) = &self.compound.kind {
            c
        } else {
            panic!("Compound isn't an instantiated compound ?!?!")
        }
    }
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug)]
pub(super) enum Term {
    Term(Rc<read_term::Term>),
    Var(usize),
    Compound(Compound),
}
