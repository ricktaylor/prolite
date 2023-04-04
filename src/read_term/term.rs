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
pub(crate) enum Term {
    Integer(i64, Span),
    Float(f64, Span),
    Var(String, Span),
    Atom(String, Span),
    Compound(Compound, Span),
}

impl Term {
    pub fn span(&self) -> Span {
        match self {
            Term::Integer(_, s)
            | Term::Float(_, s)
            | Term::Var(_, s)
            | Term::Atom(_, s)
            | Term::Compound(_, s) => s.clone(),
        }
    }

    pub(super) fn new_compound(functor: &str, s: Span, args: Vec<Term>) -> Self {
        let mut span = s;
        for a in &args {
            span = Span::concat(&span, &a.span())
        }
        Term::Compound(
            Compound {
                functor: functor.to_string(),
                args,
            },
            span,
        )
    }
}

impl<'a> Term {
    pub(crate) fn list_iter(&'a self) -> Option<ListIterator<'a>> {
        match self {
            Term::Compound(c, _) if c.functor == "." => Some(ListIterator { next: Some(self) }),
            Term::Atom(s, _) if s == "[]" => Some(ListIterator { next: None }),
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
            Some(Term::Compound(c, _)) if c.functor == "." && c.args.len() == 2 => {
                self.next = Some(&c.args[1]);
                Some(&c.args[0])
            }
            Some(Term::Atom(s, _)) if s == "[]" => {
                self.next = None;
                None
            }
            Some(t) => {
                self.next = None;
                Some(t)
            }
            None => None,
        }
    }
}
