mod builtins;
mod copy_term;
mod findall;
mod solve;
mod user_defined;
mod write;

use std::collections::HashMap;
use std::rc::Rc;

use super::consult::text::*;
use super::read_term::*;

#[derive(Debug)]
pub(crate) enum Response {
    Fail,
    Cut,
    Throw(Rc<term::Term>),
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

type Var<'a> = Option<&'a Rc<term::Term>>;

#[derive(Default)]
pub(super) struct Context {
    pub procedures: HashMap<String, Procedure>,
}

#[cfg(test)]
use super::consult;

#[cfg(test)]
mod test;
