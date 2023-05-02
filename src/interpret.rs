mod builtins;
mod solve;

use super::read_term::*;
use super::consult;

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

#[derive(Clone)]
pub(crate) struct Var<'a> {
    pub name: String,
    pub value: Option<&'a term::Term>,
}

pub(super) struct Context {}

impl Default for Context {
    fn default() -> Self {
        Self {}
    }
}

trait Solver {
    fn solve(&mut self, ctx: &mut Context, substs: &[Var]) -> Response;
}

#[cfg(test)]
mod test;
