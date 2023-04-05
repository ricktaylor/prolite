use std::mem;

use super::stream::Span;

#[derive(Debug, Clone)]
pub(crate) struct Compound {
    pub functor: String,
    pub args: Vec<Term>,
}

impl Compound {
    pub(crate) fn predicate_indicator(&self) -> String {
        format!("{}/{}", self.functor, self.args.len())
    }
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
    pub(super) fn new_atom(s: String, sp: Span) -> Self {
        Term {
            kind: TermKind::Atom(s),
            location: sp,
        }
    }

    pub(super) fn new_compound(functor: String, s: Span, args: Vec<Term>) -> Self {
        let mut span = s;
        for a in &args {
            span = Span::concat(&span, &a.location)
        }
        Term {
            kind: TermKind::Compound(Compound { functor, args }),
            location: span,
        }
    }
}

impl<'a> Term {
    pub(crate) fn list_iter(&'a self) -> Option<ListIterator<'a>> {
        match &self.kind {
            TermKind::Compound(c) if c.functor == "." => Some(ListIterator { next: Some(self) }),
            TermKind::Atom(s) if s == "[]" => Some(ListIterator { next: None }),
            _ => None,
        }
    }
}

pub(crate) struct ListIterator<'a> {
    next: Option<&'a Term>,
}

impl<'a> Iterator for ListIterator<'a> {
    type Item = &'a Term;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.next {
            None => None,
            Some(t) => match &t.kind {
                TermKind::Compound(c) if c.functor == "." && c.args.len() == 2 => {
                    self.next = Some(&c.args[1]);
                    Some(&c.args[0])
                }
                TermKind::Atom(s) if s == "[]" => {
                    self.next = None;
                    None
                }
                _ => mem::take(&mut self.next),
            },
        }
    }
}
