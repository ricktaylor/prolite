
#[derive(Debug,Clone)]
pub(super) struct Compound {
    pub functor: String,
    pub args: Vec<Term>
}

#[derive(Debug,Clone)]
pub(super) enum Term {
    Integer(i64),
    Float(f64),
    Var(String),
    Atom(String),
    Compound(Compound)
}

impl Term {
    pub(super) fn new_compound(functor: &str, args: Vec<Term>) -> Self {
        Term::Compound(Compound { functor: functor.to_string(), args } )
    }
}

impl<'a> Term {
    pub(super) fn list_iter(&'a self) -> Option<ListIterator<'a>> {
        match self {
            Term::Compound(c) if c.functor == "." => {
                Some(ListIterator {
                    next: Some(self)
                })
            },
            Term::Atom(s) if s == "[]" => {
                Some(ListIterator {
                    next: None
                })
            }
            _ => None
        }
    }
}

pub(super) struct ListIterator<'a> {
    next: Option<&'a Term>
}

impl<'a> Iterator for ListIterator<'a> {
    type Item = &'a Term;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.next {
            Some(Term::Compound(c)) if c.functor == "." && c.args.len() == 2 => {
                self.next = Some(&c.args[1]);
                Some(&c.args[0])
            },
            Some(Term::Atom(s)) if s == "[]" => {
                self.next = None;
                None
            },
            Some(t) => {
                self.next = None;
                Some(t)
            },
            None => None
        }
    }
}
