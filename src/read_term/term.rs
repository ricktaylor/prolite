use super::stream::Span;

#[derive(Debug, Clone)]
pub(crate) struct Compound {
    pub functor: String,
    pub args: Vec<Term>,
}

#[derive(Debug, Clone)]
pub(crate) enum TermKind {
    Integer(i64),
    Float(f64),
    Var(String),
    Atom(String),
    Compound(Compound),
}

#[derive(Debug, Clone)]
pub(crate) struct Term {
    pub kind: TermKind,
    pub location: Span,
}

impl Term {
    pub(crate) fn new_atom(s: String, location: Span) -> Self {
        Term {
            kind: TermKind::Atom(s),
            location,
        }
    }

    pub(crate) fn new_compound(functor: String, location: Span, args: Vec<Term>) -> Self {
        Term {
            kind: TermKind::Compound(Compound { functor, args }),
            location,
        }
    }
}
