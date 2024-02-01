use super::stream::Span;
use std::rc::Rc;

#[derive(Debug)]
pub struct VarInfo {
    pub name: String,
    pub refcount: usize,
    pub anon: bool,
}

#[derive(Debug)]
pub struct Compound {
    pub functor: String,
    pub args: Vec<Rc<Term>>,
}

#[derive(Debug)]
pub enum TermKind {
    Integer(i64),
    Float(f64),
    Var(usize),
    Atom(String),
    Compound(Compound),
}

#[derive(Debug)]
pub struct Term {
    pub kind: TermKind,
    pub location: Option<Span>,
}

impl Term {
    pub fn new_atom(s: String, location: Option<Span>) -> Rc<Term> {
        Rc::new(Term {
            kind: TermKind::Atom(s),
            location,
        })
    }

    pub fn new_compound(functor: String, location: Option<Span>, args: Vec<Rc<Term>>) -> Rc<Term> {
        Rc::new(Term {
            kind: TermKind::Compound(Compound { functor, args }),
            location,
        })
    }

    pub fn new_integer(i: i64, location: Option<Span>) -> Rc<Term> {
        Rc::new(Term {
            kind: TermKind::Integer(i),
            location,
        })
    }

    pub fn new_float(d: f64, location: Option<Span>) -> Rc<Term> {
        Rc::new(Term {
            kind: TermKind::Float(d),
            location,
        })
    }

    pub fn as_pi(&self) -> Option<Rc<Term>> {
        let (functor, arity) = match &self.kind {
            TermKind::Atom(s) => (Term::new_atom(s.clone(), None), Term::new_integer(0, None)),
            TermKind::Compound(c) => (
                Term::new_atom(c.functor.clone(), None),
                Term::new_integer(c.args.len() as i64, None),
            ),
            _ => return None,
        };
        Some(Term::new_compound(
            "/".to_string(),
            None,
            vec![functor, arity],
        ))
    }
}
