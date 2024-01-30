mod arithmetic;
mod builtins;
mod char_codes;
mod findall;
mod flags;
mod frame;
mod functor;
mod solve;
mod term;
mod throw;
mod univ;
mod user_defined;
mod write;

use std::collections::HashMap;
use std::rc::Rc;

use super::consult::text::*;
use super::read_term::stream;
use super::read_term::term as read_term;
use super::*;

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

pub struct Context {
    pub procedures: HashMap<String, Procedure>,
    pub flags: crate::flags::Flags,
    pub operators: operators::OperatorTable,
    pub char_conversion: HashMap<char, char>,
}

#[cfg(test)]
use super::consult;

#[cfg(test)]
mod test;
