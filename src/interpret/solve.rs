use std::todo;

use super::*;
use builtins::*;
use term::*;

pub(super) struct Frame<'a> {
    ctx: &'a mut Context,
    cache: &'a mut Vec<Term>,
    cache_base: usize,
    substs: &'a mut Vec<Option<usize>>,
    substs_base: usize,
    undo: Vec<usize>,
}

impl<'a> Frame<'a> {
    pub fn new(
        ctx: &'a mut Context,
        cache: &'a mut Vec<Term>,
        substs: &'a mut Vec<Option<usize>>,
    ) -> Self {
        Self {
            ctx,
            cache_base: cache.len(),
            cache,
            substs_base: substs.len(),
            substs,
            undo: Vec::new(),
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
        let t = match &term.kind {
            read_term::TermKind::Var(v) => Term::Var(self.add_var(*v, var_index)),
            read_term::TermKind::Compound(c) => Term::Compound(Compound {
                args: c
                    .args
                    .iter()
                    .map(|arg| self.new_term_indexed(arg, var_index))
                    .collect(),
                compound: term.clone(),
            }),
            _ => Term::Term(term.clone()),
        };
        self.add_term(t)
    }

    fn add_term(&mut self, t: Term) -> usize {
        self.cache.push(t);
        self.cache.len() - 1
    }

    pub fn get_term(&self, idx: usize) -> &Term {
        &self.cache[idx]
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
        f(Frame::new(self.ctx, self.cache, self.substs))
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
                if let Some(a) = self.substs[*idx] {
                    self.unify_fold(a, b)
                } else {
                    eprintln!("assign _{} -> {}", *idx, write::write_term(self, b));
                    let i = *idx;
                    self.substs[i] = Some(b);
                    Ok(())
                }
            }
            (_, Term::Var(idx)) => {
                if let Some(b) = self.substs[*idx] {
                    self.unify_fold(a, b)
                } else {
                    eprintln!("assign _{} -> {}", *idx, write::write_term(self, a));
                    let i = *idx;
                    self.substs[i] = Some(a);
                    Ok(())
                }
            }
            (Term::Term(t1), Term::Term(t2)) => match (&t1.kind, &t2.kind) {
                (read_term::TermKind::Integer(i1), read_term::TermKind::Integer(i2))
                    if *i1 == *i2 =>
                {
                    Ok(())
                }
                (read_term::TermKind::Integer(i1), read_term::TermKind::Float(f))
                    if *i1 as f64 == *f =>
                {
                    Ok(())
                }
                (read_term::TermKind::Float(f1), read_term::TermKind::Float(f2)) if *f1 == *f2 => {
                    Ok(())
                }
                (read_term::TermKind::Float(f1), read_term::TermKind::Integer(i))
                    if *f1 == *i as f64 =>
                {
                    Ok(())
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
            (Term::Var(idx), _) => {
                if let Some(a) = self.substs[*idx] {
                    self.unify_copy_inner(a, b, var_index)
                } else {
                    let a = self.copy_term_inner(a, var_index);
                    self.unify_fold(a, b)
                }
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
                if let Some(t) = self.substs[*idx] {
                    self.copy_term_inner(t, var_index)
                } else {
                    let t = Term::Var(self.add_var(*idx, var_index));
                    self.add_term(t)
                }
            }
            _ => t,
        }
    }

    pub fn as_list(&mut self, list: &[usize]) -> usize {
        list.iter().rev().fold(
            self.new_term(&read_term::Term::new_atom("[]".to_string(), None)),
            |list, t| {
                self.add_term(Term::Compound(Compound {
                    compound: read_term::Term::new_compound(
                        ".".to_string(),
                        None,
                        vec![
                            match self.get_term(*t) {
                                Term::Term(t2) => t2.clone(),
                                Term::Var(idx) => Rc::new(read_term::Term {
                                    kind: read_term::TermKind::Var(*idx),
                                    location: None,
                                }),
                                Term::Compound(c) => c.compound.clone(),
                            },
                            match self.get_term(list) {
                                Term::Term(t1) => t1.clone(),
                                Term::Compound(c) => c.compound.clone(),
                                _ => panic!("Var in list!"),
                            },
                        ],
                    ),
                    args: vec![*t, list],
                }))
            },
        )
    }
}

impl<'a> Drop for Frame<'a> {
    fn drop(&mut self) {
        for v in self.undo.iter() {
            if *v > self.substs_base {
                self.substs[*v] = None;
            }
        }
        self.substs.truncate(self.substs_base);
        self.cache.truncate(self.cache_base);
    }
}

pub(super) fn solve(frame: Frame, goal: usize, next: &mut dyn Solver) -> Response {
    eprintln!("solving {}", write::write_term(&frame,goal));

    match frame.get_term(goal) {
        Term::Term(t) => {
            if let read_term::TermKind::Atom(s) = &t.kind {
                match is_builtin(&format!("{}/0", s)) {
                    Some(f) => (f)(frame, &mut [], next),
                    None => user_defined::solve(frame, goal, next),
                }
            } else {
                todo!()
            }
        }
        Term::Var(idx) => {
            if let Some(goal) = frame.get_var(*idx) {
                call(frame, goal, next)
            } else {
                Response::Throw(read_term::Term::new_atom(
                    "existence_error".to_string(),
                    None,
                ))
            }
        }
        Term::Compound(c) => {
            match is_builtin(&format!("{}/{}", c.functor(), c.args.len())) {
                Some(f) => {
                    let args = c.args.to_vec();
                    (f)(frame, &args, next)
                }
                None => user_defined::solve(frame, goal, next),
            }
        }
        _ => todo!(),
    }
}

pub(super) fn call(frame: Frame, goal: usize, next: &mut dyn Solver) -> Response {
    // call/1 is "not transparent" to cut, so use a continuation to record the response from next
    let mut next_cut = false;
    match solve(
        frame,
        goal,
        &mut Continuation::new(|frame| {
            next.solve(frame).map_cut(|| {
                next_cut = true;
                Response::Cut
            })
        }),
    ) {
        Response::Cut if !next_cut => Response::Fail,
        /*Response::Throw(_) => {
            // Catch callable error!
        }*/
        r => r,
    }
}

pub(super) fn unify(mut frame: Frame, a: usize, b: usize, next: &mut dyn Solver) -> Response {
    frame.sub_frame(|mut frame| {
        if frame.unify(a, b) {
            next.solve(frame)
        } else {
            Response::Fail
        }
    })
}

pub(super) trait Solver {
    fn solve(&mut self, frame: Frame) -> Response;
}

pub(super) struct Continuation<F: FnMut(Frame) -> Response> {
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

struct CallbackSolver<'a, F: FnMut(&'a [read_term::VarInfo]) -> bool> {
    callback: F,
    var_info: &'a [read_term::VarInfo],
}

impl<'a, F> Solver for CallbackSolver<'a, F>
where
    F: FnMut(&'a [read_term::VarInfo]) -> bool,
{
    fn solve(&mut self, _: Frame) -> Response {
        if (self.callback)(self.var_info) {
            Response::Fail
        } else {
            Response::Cut
        }
    }
}

pub(crate) fn eval<F: FnMut(&[read_term::VarInfo]) -> bool>(
    ctx: &mut Context,
    goal: &Rc<read_term::Term>,
    var_info: &[read_term::VarInfo],
    callback: F,
) -> Response {
    let mut cache = Vec::new();
    let mut substs = Vec::new();
    let mut frame = solve::Frame::new(ctx, &mut cache, &mut substs);
    let goal = frame.new_term(goal);
    solve(frame, goal, &mut CallbackSolver { callback, var_info })
}
