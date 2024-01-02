use super::stream::Span;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct VarInfo {
    pub name: String,
    pub refcount: usize,
    pub anon: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct Compound {
    pub functor: String,
    pub args: Vec<Rc<Term>>,
}

#[derive(Debug, Clone)]
pub(crate) enum TermKind {
    Integer(i64),
    Float(f64),
    Var(usize),
    Atom(String),
    Compound(Compound),
}

#[derive(Debug, Clone)]
pub(crate) struct Term {
    pub kind: TermKind,
    pub location: Option<Span>,
}

impl Term {
    pub(crate) fn new_atom(s: String, location: Option<Span>) -> Rc<Term> {
        Rc::new(Term {
            kind: TermKind::Atom(s),
            location,
        })
    }

    pub(crate) fn new_compound(
        functor: String,
        location: Option<Span>,
        args: Vec<Rc<Term>>,
    ) -> Rc<Term> {
        Rc::new(Term {
            kind: TermKind::Compound(Compound { functor, args }),
            location,
        })
    }
}
