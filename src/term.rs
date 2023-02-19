
#[derive(Debug)]
pub struct Compound {
    pub functor: String,
    pub args: Vec<Term>
}

#[derive(Debug)]
pub enum Term {
    Integer(i64),
    Float(f64),
    Var(String),
    Atom(String),
    Compound(Compound)
}

impl Term {
    pub fn new_compound(functor: &str, args: Vec<Term>) -> Self {
        Term::Compound(Compound { functor: functor.to_string(), args } )
    }
}
