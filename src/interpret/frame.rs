use std::collections::HashSet;

use super::*;
use term::*;

pub struct Frame<'a> {
    pub context: &'a mut Context,
    cache: &'a mut Vec<Term>,
    cache_base: usize,
    substs: &'a mut Vec<Option<usize>>,
    substs_base: usize,
    undo: HashSet<usize>,
}

impl<'a> Frame<'a> {
    pub fn new(
        context: &'a mut Context,
        cache: &'a mut Vec<Term>,
        substs: &'a mut Vec<Option<usize>>,
    ) -> Self {
        Self {
            context,
            cache_base: cache.len(),
            cache,
            substs_base: substs.len(),
            substs,
            undo: HashSet::new(),
        }
    }

    pub fn new_term(&mut self, term: &Rc<read_term::Term>) -> usize {
        self.new_term_indexed(term, &mut HashMap::new())
    }

    pub fn new_term_indexed(
        &mut self,
        term: &Rc<read_term::Term>,
        var_index: &mut HashMap<usize, usize>,
    ) -> usize {
        match &term.kind {
            read_term::TermKind::Var(idx) => {
                if let Some(i) = var_index.get(idx) {
                    self.deref(*i)
                } else {
                    let i = self.substs.len();
                    self.substs.push(None);

                    let term = Term {
                        kind: TermKind::Var(i),
                        source: term.clone(),
                    };
                    let t = self.add_term(term);
                    var_index.insert(*idx, t);
                    t
                }
            }
            read_term::TermKind::Compound(c) => {
                let term = Term {
                    kind: TermKind::Compound(
                        c.args
                            .iter()
                            .map(|arg| self.new_term_indexed(arg, var_index))
                            .collect(),
                    ),
                    source: term.clone(),
                };
                self.add_term(term)
            }
            _ => {
                let term = Term {
                    kind: TermKind::Atomic,
                    source: term.clone(),
                };
                self.add_term(term)
            }
        }
    }

    fn add_term(&mut self, t: Term) -> usize {
        self.cache.push(t);
        self.cache.len() - 1
    }

    fn deref(&self, term: usize) -> usize {
        match &self.cache[term].kind {
            TermKind::Var(idx) => {
                if let Some(term) = self.substs[*idx] {
                    self.deref(term)
                } else {
                    term
                }
            }
            _ => term,
        }
    }

    pub fn get_term(&self, mut term: usize) -> (&Term, usize) {
        term = self.deref(term);
        (&self.cache[term], term)
    }

    pub fn sub_frame<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(Frame) -> R,
    {
        f(Frame::new(self.context, self.cache, self.substs))
    }

    pub fn unify(&mut self, t1: usize, t2: usize) -> bool {
        let (term1, t1) = self.get_term(t1);
        let (term2, t2) = self.get_term(t2);
        if t1 == t2 {
            return true;
        }
        match (
            &term1.kind,
            &term1.source.kind,
            &term2.kind,
            &term2.source.kind,
        ) {
            (
                TermKind::Atomic,
                read_term::TermKind::Integer(i1),
                TermKind::Atomic,
                read_term::TermKind::Integer(i2),
            ) if *i1 == *i2 => true,
            (
                TermKind::Atomic,
                read_term::TermKind::Float(f1),
                TermKind::Atomic,
                read_term::TermKind::Float(f2),
            ) if *f1 == *f2 => true,
            (
                TermKind::Atomic,
                read_term::TermKind::Atom(s1),
                TermKind::Atomic,
                read_term::TermKind::Atom(s2),
            ) if *s1 == *s2 => true,
            (
                TermKind::Compound(args1),
                read_term::TermKind::Compound(c1),
                TermKind::Compound(args2),
                read_term::TermKind::Compound(c2),
            ) if c1.functor == c2.functor && args1.len() == args2.len() => {
                for (t1, t2) in args1.to_vec().iter().zip(&args2.to_vec()) {
                    if !self.unify(*t1, *t2) {
                        return false;
                    }
                }
                true
            }
            (_, _, TermKind::Var(idx), _) => {
                //eprintln!("assign {} <- _{}", write::write_term(self, t1), *idx);
                let i = *idx;
                if i < self.substs_base {
                    self.undo.insert(i);
                }
                self.substs[i] = Some(t1);
                true
            }
            (TermKind::Var(idx), _, _, _) => {
                //eprintln!("assign _{} -> {}", *idx, write::write_term(self, t2));
                let i = *idx;
                if i < self.substs_base {
                    self.undo.insert(i);
                }
                self.substs[i] = Some(t2);
                true
            }
            _ => false,
        }
    }

    pub fn occurs_check(&self, t1: usize, t2: usize) -> bool {
        let (term1, t1) = self.get_term(t1);
        let (term2, t2) = self.get_term(t2);
        match (
            &term1.kind,
            &term1.source.kind,
            &term2.kind,
            &term2.source.kind,
        ) {
            (TermKind::Var(idx1), _, TermKind::Var(idx2), _) => idx1 == idx2,
            (_, _, TermKind::Var(_), _) => self.occurs_check(t2, t1),
            (TermKind::Var(_), _, TermKind::Compound(args), _) => {
                for a in args {
                    if self.occurs_check(t1, *a) {
                        return true;
                    }
                }
                false
            }
            (
                TermKind::Compound(args1),
                read_term::TermKind::Compound(c1),
                TermKind::Compound(args2),
                read_term::TermKind::Compound(c2),
            ) if t1 != t2 && c1.functor == c2.functor && args1.len() == args2.len() => {
                for (t1, t2) in args1.iter().zip(args2) {
                    if self.occurs_check(*t1, *t2) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    pub fn not_unifiable(&self, t1: usize, t2: usize) -> bool {
        let (term1, t1) = self.get_term(t1);
        let (term2, t2) = self.get_term(t2);
        if t1 == t2 {
            return false;
        }
        match (
            &term1.kind,
            &term1.source.kind,
            &term2.kind,
            &term2.source.kind,
        ) {
            (
                TermKind::Atomic,
                read_term::TermKind::Integer(i1),
                TermKind::Atomic,
                read_term::TermKind::Integer(i2),
            ) if *i1 == *i2 => false,
            (
                TermKind::Atomic,
                read_term::TermKind::Float(f1),
                TermKind::Atomic,
                read_term::TermKind::Float(f2),
            ) if *f1 == *f2 => false,
            (
                TermKind::Atomic,
                read_term::TermKind::Atom(s1),
                TermKind::Atomic,
                read_term::TermKind::Atom(s2),
            ) if *s1 == *s2 => false,
            (
                TermKind::Compound(args1),
                read_term::TermKind::Compound(c1),
                TermKind::Compound(args2),
                read_term::TermKind::Compound(c2),
            ) if c1.functor == c2.functor && args1.len() == args2.len() => {
                for (t1, t2) in args1.iter().zip(args2) {
                    if !self.not_unifiable(*t1, *t2) {
                        return false;
                    }
                }
                true
            }
            (_, _, TermKind::Var(_), _) | (TermKind::Var(_), _, _, _) => false,
            _ => true,
        }
    }

    pub fn copy_term(&mut self, t: usize) -> usize {
        self.copy_term_inner(t, &mut HashMap::new())
    }

    fn copy_term_inner(&mut self, t: usize, var_index: &mut HashMap<usize, usize>) -> usize {
        let (term, t) = self.get_term(t);
        //eprintln!("copy_term_inner {}",write::write_term(self, t));
        match &term.kind {
            TermKind::Compound(args) => {
                let args = args.to_vec();
                let term = Term {
                    source: term.source.clone(),
                    kind: TermKind::Compound(
                        args.iter()
                            .map(|a| self.copy_term_inner(*a, var_index))
                            .collect(),
                    ),
                };
                self.add_term(term)
            }
            TermKind::Var(idx) => {
                if let Some(i) = var_index.get(idx) {
                    self.deref(*i)
                } else {
                    let idx = *idx;
                    let term = Term {
                        source: term.source.clone(),
                        kind: TermKind::Var(self.substs.len()),
                    };
                    self.substs.push(None);
                    let t = self.add_term(term);
                    var_index.insert(idx, t);
                    t
                }
            }
            _ => t,
        }
    }

    pub fn list_from_slice(&mut self, list: &[usize]) -> usize {
        list.iter().rev().fold(
            self.new_term(&read_term::Term::new_atom("[]".to_string(), None)),
            |list, t| {
                let (term, t) = self.get_term(*t);
                let (list_term, list) = self.get_term(list);
                self.add_term(Term {
                    kind: TermKind::Compound(vec![t, list]),
                    source: read_term::Term::new_compound(
                        ".".to_string(),
                        None,
                        vec![term.source.clone(), list_term.source.clone()],
                    ),
                })
            },
        )
    }

    pub fn term_from_slice(&mut self, list: &[usize]) -> Option<usize> {
        if list.len() == 1 {
            return Some(self.deref(list[0]));
        }

        let (functor, location) = {
            let (functor, _) = self.get_term(list[0]);
            match (&functor.kind, &functor.source.kind) {
                (TermKind::Atomic, read_term::TermKind::Atom(s)) => {
                    (s.clone(), functor.source.location.clone())
                }
                _ => return None,
            }
        };

        let mut t_args = Vec::new();
        let mut r_args = Vec::new();
        for t in &list[1..] {
            let (term, t) = self.get_term(*t);
            r_args.push(term.source.clone());
            t_args.push(t);
        }

        Some(self.add_term(Term {
            kind: TermKind::Compound(t_args),
            source: read_term::Term::new_compound(functor, location, r_args),
        }))
    }
}

impl<'a> Drop for Frame<'a> {
    fn drop(&mut self) {
        for v in self.undo.iter() {
            self.substs[*v] = None;
        }
        self.substs.truncate(self.substs_base);
        self.cache.truncate(self.cache_base);
    }
}
