mod builtins;
mod solve;
mod user_defined;

use std::collections::HashMap;

use super::consult::text::*;
use super::read_term::*;

#[derive(Debug)]
pub(crate) enum Response {
    Fail,
    Cut,
    Throw(term::Term),
    Halt(isize),
}

impl Response {
    fn map_failed<F: FnOnce() -> Response>(self, op: F) -> Response {
        match self {
            Response::Fail => op(),
            _ => self,
        }
    }

    fn map_cut<F: FnOnce() -> Response>(self, op: F) -> Response {
        match self {
            Response::Cut => op(),
            _ => self,
        }
    }
}

type Var<'a> = Option<&'a term::Term>;

fn deref_var<'a>(mut term: &'a term::Term, substs: &'a [Var<'a>]) -> &'a term::Term {
    while let term::TermKind::Var(idx) = &term.kind {
        match substs[*idx] {
            None => break,
            Some(t) => term = t,
        }
    }
    term
}

#[derive(Default)]
pub(super) struct Context {
    pub procedures: HashMap<String, Procedure>,
}

trait Solver {
    fn solve(&mut self, ctx: &mut Context, substs: &[Var]) -> Response;
}

struct Continuation<F: FnMut(&mut Context, &[Var]) -> Response> {
    solve: F,
}

impl<F> Solver for Continuation<F>
where
    F: FnMut(&mut Context, &[Var]) -> Response,
{
    fn solve(&mut self, ctx: &mut Context, substs: &[Var]) -> Response {
        (self.solve)(ctx, substs)
    }
}

impl<F> Continuation<F>
where
    F: FnMut(&mut Context, &[Var]) -> Response,
{
    fn new(f: F) -> Self {
        Self { solve: f }
    }
}

#[cfg(test)]
use super::consult;

#[cfg(test)]
mod test;
