use std::rc::Rc;

use super::*;
use frame::Frame;

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

impl Term {
    pub fn compare_index(frame: &Frame, t1: usize, t2: usize) -> Option<core::cmp::Ordering> {
        if t1 == t2 {
            Some(core::cmp::Ordering::Equal)
        } else {
            let (term1, t1) = frame.get_term(t1);
            let (term2, t2) = frame.get_term(t2);
            if t1 == t2 {
                Some(core::cmp::Ordering::Equal)
            } else {
                Term::compare(frame, term1, term2)
            }
        }
    }

    pub fn compare(frame: &Frame, term1: &Term, term2: &Term) -> Option<core::cmp::Ordering> {
        match (
            &term1.kind,
            &term1.source.kind,
            &term2.kind,
            &term2.source.kind,
        ) {
            (TermKind::Var(idx1), _, TermKind::Var(idx2), _) => {
                core::cmp::PartialOrd::partial_cmp(idx1, idx2)
            }
            (TermKind::Var(_), _, _, _) => Some(core::cmp::Ordering::Less),
            (
                TermKind::Atomic,
                read_term::TermKind::Float(f1),
                TermKind::Atomic,
                read_term::TermKind::Float(f2),
            ) => core::cmp::PartialOrd::partial_cmp(f1, f2),
            (TermKind::Atomic, read_term::TermKind::Float(_), _, _) => {
                Some(core::cmp::Ordering::Less)
            }
            (
                TermKind::Atomic,
                read_term::TermKind::Integer(i1),
                TermKind::Atomic,
                read_term::TermKind::Integer(i2),
            ) => core::cmp::PartialOrd::partial_cmp(i1, i2),
            (TermKind::Atomic, read_term::TermKind::Integer(_), _, _) => {
                Some(core::cmp::Ordering::Less)
            }
            (
                TermKind::Atomic,
                read_term::TermKind::Atom(s1),
                TermKind::Atomic,
                read_term::TermKind::Atom(s2),
            ) => core::cmp::PartialOrd::partial_cmp(s1, s2),
            (TermKind::Atomic, _, _, _) => Some(core::cmp::Ordering::Less),
            (
                TermKind::Compound(args1),
                read_term::TermKind::Compound(c1),
                TermKind::Compound(args2),
                read_term::TermKind::Compound(c2),
            ) => {
                let mut r = core::cmp::PartialOrd::partial_cmp(&args1.len(), &args2.len());
                if let Some(core::cmp::Ordering::Equal) = r {
                    r = core::cmp::PartialOrd::partial_cmp(&c1.functor, &c2.functor);
                    if let Some(core::cmp::Ordering::Equal) = r {
                        for (a1, a2) in args1.iter().zip(args2) {
                            match Term::compare_index(frame, *a1, *a2) {
                                Some(core::cmp::Ordering::Equal) => {}
                                r => return r,
                            }
                        }
                    }
                }
                r
            }
            (TermKind::Compound(_), _, _, _) => Some(core::cmp::Ordering::Greater),
        }
    }
}
