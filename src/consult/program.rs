use std::collections::HashMap;

use super::*;
use read_term::term::Term;

#[derive(Default, Debug, Clone)]
pub(super) struct Flags {
    pub public: bool,
    pub dynamic: bool,
    pub multifile: bool,
    pub discontiguous: bool,
}

pub(super) struct Predicate {
    pub head: Term,
    pub tail: Term,
}

#[derive(Default)]
pub(super) struct Procedure {
    pub flags: Flags,
    pub predicates: Vec<Predicate>,
}

pub(super) struct Program {
    pub predicates: HashMap<String, Procedure>,
}

impl Program {
    pub(super) fn new() -> Self {
        Program {
            predicates: HashMap::new(),
        }
    }
}
