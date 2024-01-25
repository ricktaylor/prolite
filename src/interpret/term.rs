use std::rc::Rc;

use super::*;

#[derive(Debug)]
pub enum TermKind {
    Atomic,
    Var(usize),
    Compound(Vec<usize>),
}

#[derive(Debug)]
pub struct Term {
    pub kind: TermKind,
    pub source: Rc<read_term::Term>,
}
