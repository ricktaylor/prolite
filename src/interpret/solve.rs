use core::panic;
use std::collections::HashSet;

use super::*;
use builtins::*;
use term::*;

pub struct Frame<'a> {
    ctx: &'a mut Context,
    cache: &'a mut Vec<Term>,
    cache_base: usize,
    substs: &'a mut Vec<Option<usize>>,
    substs_base: usize,
    undo: HashSet<usize>,
    location: Option<stream::Span>,
}

impl<'a> Frame<'a> {
    fn new(
        ctx: &'a mut Context,
        cache: &'a mut Vec<Term>,
        substs: &'a mut Vec<Option<usize>>,
        location: Option<stream::Span>,
    ) -> Self {
        Self {
            ctx,
            cache_base: cache.len(),
            cache,
            substs_base: substs.len(),
            substs,
            undo: HashSet::new(),
            location,
        }
    }

    pub fn new_term(&mut self, term: Rc<read_term::Term>) -> usize {
        self.new_term_indexed(term, &mut HashMap::new())
    }

    pub fn new_term_indexed(
        &mut self,
        term: Rc<read_term::Term>,
        var_index: &mut HashMap<usize, usize>,
    ) -> usize {
        let t = match &term.kind {
            read_term::TermKind::Var(v) => Term::Var(self.add_var(*v, var_index)),
            read_term::TermKind::Compound(c) => Term::Compound(Compound {
                args: c
                    .args
                    .iter()
                    .map(|arg| self.new_term_indexed(arg.clone(), var_index))
                    .collect(),
                compound: term,
            }),
            _ => Term::Atomic(term),
        };
        self.add_term(t)
    }

    fn add_term(&mut self, t: Term) -> usize {
        self.cache.push(t);
        self.cache.len() - 1
    }

    pub fn get_term_shallow(&self, term: usize) -> &Term {
        &self.cache[term]
    }

    fn deref(&self, term: usize) -> usize {
        match self.get_term_shallow(term) {
            Term::Var(idx) => {
                if let Some(term) = self.get_var(*idx) {
                    self.deref(term)
                } else {
                    term
                }
            }
            _ => term,
        }
    }

    pub fn get_term(&self, term: usize) -> &Term {
        self.get_term_shallow(self.deref(term))
    }

    pub fn get_var(&self, idx: usize) -> Option<usize> {
        self.substs[idx]
    }

    fn add_var(&mut self, idx: usize, var_index: &mut HashMap<usize, usize>) -> usize {
        if let Some(i) = var_index.get(&idx) {
            *i
        } else {
            let i = self.substs.len();
            self.substs.push(None);
            var_index.insert(idx, i);
            i
        }
    }

    pub fn sub_frame<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(Frame) -> R,
    {
        f(Frame::new(
            self.ctx,
            self.cache,
            self.substs,
            self.location.clone(),
        ))
    }

    pub fn get_location(&self) -> &Option<stream::Span> {
        &self.location
    }

    pub fn get_context(&self) -> &Context {
        self.ctx
    }

    pub fn get_context_mut(&mut self) -> &mut Context {
        self.ctx
    }

    pub fn unify(&mut self, a: usize, b: usize) -> bool {
        self.unify_fold(a, b).is_ok()
    }

    pub fn unify_fold(&mut self, a: usize, b: usize) -> Result<(), Response> {
        match (self.get_term(a), self.get_term(b)) {
            (Term::Compound(c1), Term::Compound(c2))
                if c1.functor() == c2.functor() && c1.args.len() == c2.args.len() =>
            {
                let args1 = c1.args.to_vec();
                let args2 = c2.args.to_vec();
                args1
                    .iter()
                    .zip(&args2)
                    .try_fold((), |_, (a, b)| self.unify_fold(*a, *b))
            }
            (Term::Var(idx), _) => {
                //eprintln!("assign _{} -> {}", *idx, write::write_term(self, b));
                let i = *idx;
                if i < self.substs_base {
                    self.undo.insert(i);
                }
                self.substs[i] = Some(b);
                Ok(())
            }
            (_, Term::Var(idx)) => {
                //eprintln!("assign _{} -> {}", *idx, write::write_term(self, a));
                let i = *idx;
                if i < self.substs_base {
                    self.undo.insert(i);
                }
                self.substs[i] = Some(a);
                Ok(())
            }
            (Term::Atomic(t1), Term::Atomic(t2)) => match (&t1.kind, &t2.kind) {
                (read_term::TermKind::Integer(i1), read_term::TermKind::Integer(i2))
                    if *i1 == *i2 =>
                {
                    Ok(())
                }
                (read_term::TermKind::Integer(i), read_term::TermKind::Float(f)) => {
                    let f1 = *i as f64;
                    if f1 == *f && *i == f1 as i64 {
                        Ok(())
                    } else {
                        Err(Response::Fail)
                    }
                }
                (read_term::TermKind::Float(f1), read_term::TermKind::Float(f2)) if *f1 == *f2 => {
                    Ok(())
                }
                (read_term::TermKind::Float(f), read_term::TermKind::Integer(i)) => {
                    let f1 = *i as f64;
                    if f1 == *f && *i == f1 as i64 {
                        Ok(())
                    } else {
                        Err(Response::Fail)
                    }
                }
                (read_term::TermKind::Atom(s1), read_term::TermKind::Atom(s2)) if *s1 == *s2 => {
                    Ok(())
                }
                _ => Err(Response::Fail),
            },
            _ => Err(Response::Fail),
        }
    }

    pub fn unify_copy(&mut self, a: usize, b: usize) -> bool {
        self.unify_copy_inner(a, b, &mut HashMap::new()).is_ok()
    }

    fn unify_copy_inner(
        &mut self,
        a: usize,
        b: usize,
        var_index: &mut HashMap<usize, usize>,
    ) -> Result<(), Response> {
        match (self.get_term(a), self.get_term(b)) {
            (Term::Compound(c1), Term::Compound(c2))
                if c1.functor() == c2.functor() && c1.args.len() == c2.args.len() =>
            {
                let args1 = c1.args.to_vec();
                let args2 = c2.args.to_vec();
                args1
                    .iter()
                    .zip(&args2)
                    .try_fold((), |_, (a, b)| self.unify_copy_inner(*a, *b, var_index))
            }
            (Term::Var(_), _) => {
                let a = self.copy_term_inner(a, var_index);
                self.unify_fold(a, b)
            }
            _ => self.unify_fold(a, b),
        }
    }

    pub fn copy_term(&mut self, t: usize) -> usize {
        self.copy_term_inner(t, &mut HashMap::new())
    }

    fn copy_term_inner(&mut self, t: usize, var_index: &mut HashMap<usize, usize>) -> usize {
        match self.get_term(t) {
            Term::Compound(c1) => {
                let args = c1.args.to_vec();
                let t = Term::Compound(Compound {
                    compound: c1.compound.clone(),
                    args: args
                        .iter()
                        .map(|a| self.copy_term_inner(*a, var_index))
                        .collect(),
                });
                self.add_term(t)
            }
            Term::Var(idx) => {
                let t = Term::Var(self.add_var(*idx, var_index));
                self.add_term(t)
            }
            _ => t,
        }
    }

    pub fn list_from_slice(&mut self, list: &[usize]) -> usize {
        list.iter().rev().fold(
            self.new_term(read_term::Term::new_atom("[]".to_string(), None)),
            |list, t| {
                let t = self.deref(*t);
                self.add_term(Term::Compound(Compound {
                    compound: read_term::Term::new_compound(
                        ".".to_string(),
                        None,
                        vec![
                            match self.get_term(t) {
                                Term::Atomic(t2) => t2.clone(),
                                Term::Var(idx) => Rc::new(read_term::Term {
                                    kind: read_term::TermKind::Var(*idx),
                                    location: None,
                                }),
                                Term::Compound(c) => c.compound.clone(),
                            },
                            match self.get_term(list) {
                                Term::Atomic(t1) => t1.clone(),
                                Term::Compound(c) => c.compound.clone(),
                                _ => unreachable!(),
                            },
                        ],
                    ),
                    args: vec![t, list],
                }))
            },
        )
    }

    pub fn term_from_slice(&mut self, list: &[usize]) -> usize {
        if list.len() == 1 {
            return self.deref(list[0]);
        }

        let (functor, location) = match self.get_term(list[0]) {
            Term::Atomic(t) => match &t.kind {
                read_term::TermKind::Atom(s) => (s, &t.location),
                _ => panic!("Frame::term_from_slice passed nonsense!"),
            },
            _ => panic!("Frame::term_from_slice passed nonsense!"),
        };

        let mut r_args = Vec::new();
        let mut t_args = Vec::new();
        for a in &list[1..] {
            let a = self.deref(*a);
            r_args.push(match self.get_term(a) {
                Term::Atomic(t) => t.clone(),
                Term::Var(idx) => Rc::new(read_term::Term {
                    kind: read_term::TermKind::Var(*idx),
                    location: None,
                }),
                Term::Compound(c) => c.compound.clone(),
            });
            t_args.push(a);
        }

        self.add_term(Term::Compound(Compound {
            compound: read_term::Term::new_compound(functor.clone(), location.clone(), r_args),
            args: t_args,
        }))
    }

    fn solve(mut self, deep: bool, goal: usize, next: &mut dyn Solver) -> Response {
        match if deep {
            self.get_term(goal)
        } else {
            self.get_term_shallow(goal)
        } {
            Term::Atomic(t) => {
                if let read_term::TermKind::Atom(s) = &t.kind {
                    let pi = format!("{}/0", s);
                    self.location = t.location.clone();
                    match get_builtin(&pi) {
                        Some((f, _)) => (f)(self, &[], next),
                        None => user_defined::solve(self, &pi, goal, next),
                    }
                } else {
                    throw::error(
                        read_term::Term::new_compound(
                            "type_error".to_string(),
                            None,
                            vec![
                                read_term::Term::new_atom("callable".to_string(), None),
                                t.clone(),
                            ],
                        ),
                        t.location.clone(),
                    )
                }
            }
            Term::Var(idx) if !deep => {
                if let Some(goal) = self.get_var(*idx) {
                    call(self, goal, next)
                } else {
                    throw::instantiation_error(&self)
                }
            }
            Term::Var(_) => throw::instantiation_error(&self),
            Term::Compound(c) => {
                let pi = &format!("{}/{}", c.functor(), c.args.len());
                let args = c.args.to_vec();
                self.location = c.compound.location.clone();
                match get_builtin(&pi) {
                    Some((f, _)) => (f)(self, &args, next),
                    None => user_defined::solve(self, &pi, goal, next),
                }
            }
        }
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

pub fn solve(frame: Frame, goal: usize, next: &mut dyn Solver) -> Response {
    frame.solve(false, goal, next)
}

pub fn call(frame: Frame, goal: usize, next: &mut dyn Solver) -> Response {
    // call/1 is "not transparent" to cut, so use a continuation to record the response from next
    let mut next_cut = false;
    frame
        .solve(
            true,
            goal,
            &mut Continuation::new(|frame| {
                next.solve(frame).map_cut(|| {
                    next_cut = true;
                    Response::Cut
                })
            }),
        )
        .map_cut(|| {
            if !next_cut {
                Response::Fail
            } else {
                Response::Cut
            }
        })
}

pub trait Solver {
    fn solve(&mut self, frame: Frame) -> Response;
}

pub struct Continuation<F: FnMut(Frame) -> Response> {
    solve: F,
}

impl<F> Solver for Continuation<F>
where
    F: FnMut(Frame) -> Response,
{
    fn solve(&mut self, frame: Frame) -> Response {
        (self.solve)(frame)
    }
}

impl<F> Continuation<F>
where
    F: FnMut(Frame) -> Response,
{
    pub fn new(f: F) -> Self {
        Self { solve: f }
    }
}

struct CallbackSolver<F: FnMut() -> bool> {
    callback: F,
}

impl<F> Solver for CallbackSolver<F>
where
    F: FnMut() -> bool,
{
    fn solve(&mut self, _: Frame) -> Response {
        if (self.callback)() {
            Response::Fail
        } else {
            Response::Cut
        }
    }
}

pub(crate) fn eval<F: FnMut() -> bool>(
    ctx: &mut Context,
    goal: Rc<read_term::Term>,
    callback: F,
) -> Response {
    let mut cache = Vec::new();
    let mut substs = Vec::new();
    let mut frame = solve::Frame::new(ctx, &mut cache, &mut substs, goal.location.clone());
    let goal = frame.new_term(goal);
    call(frame, goal, &mut CallbackSolver { callback })
}
