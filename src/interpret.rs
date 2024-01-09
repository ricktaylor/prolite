mod builtins;
mod findall;
mod setof;
mod solve;
mod term;
mod user_defined;
mod write;

use std::collections::HashMap;
use std::rc::Rc;

use super::consult::text::*;
use super::read_term::term as read_term;

#[derive(Debug)]
pub(crate) enum Response {
    Fail,
    Cut,
    Throw(Rc<read_term::Term>),
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

pub(super) struct Context {
    pub procedures: HashMap<String, Procedure>,
}

#[cfg(test)]
use super::consult;

#[cfg(test)]
mod test;
