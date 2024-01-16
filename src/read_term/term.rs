use super::stream::Span;
use std::rc::Rc;

#[derive(Debug)]
pub(crate) struct VarInfo {
    pub name: String,
    pub refcount: usize,
    pub anon: bool,
}

#[derive(Debug)]
pub(crate) struct Compound {
    pub functor: String,
    pub args: Vec<Rc<Term>>,
}

#[derive(Debug)]
pub(crate) enum TermKind {
    Integer(i64),
    Float(f64),
    Var(usize),
    Atom(String),
    Compound(Compound),
}

#[derive(Debug)]
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

    pub(crate) fn new_integer(i: i64, location: Option<Span>) -> Rc<Term> {
        Rc::new(Term {
            kind: TermKind::Integer(i),
            location,
        })
    }

    pub(crate) fn as_pi(&self) -> Rc<Term> {
        let (functor, arity) = match &self.kind {
            TermKind::Atom(s) => (Term::new_atom(s.clone(), None), Term::new_integer(0, None)),
            TermKind::Compound(c) => (
                Term::new_atom(c.functor.clone(), None),
                Term::new_integer(c.args.len() as i64, None),
            ),
            _ => panic!("Invalid PI request"),
        };
        Term::new_compound("/".to_string(), None, vec![functor, arity])
    }
}
