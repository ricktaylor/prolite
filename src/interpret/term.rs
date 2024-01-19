use std::rc::Rc;

use super::*;

#[derive(Debug)]
pub struct Compound {
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
            unreachable!()
        }
    }
}

#[derive(Debug)]
pub enum Term {
    Atomic(Rc<read_term::Term>),
    Var(usize),
    Compound(Compound),
}
