use std::collections::HashMap;
use std::rc::Rc;

use super::read_term::stream;
use super::*;
use read_term::term as read_term;

pub mod builtins;

mod arithmetic;
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

#[cfg(test)]
mod test;

#[derive(Debug)]
pub enum Response {
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
    procedures: HashMap<String, user_defined::Procedure>,
    flags: crate::flags::Flags,
    operators: operators::OperatorTable,
    char_conversion: HashMap<char, char>,
}

impl Context {
    pub fn consult(
        resolver: &mut dyn consult::StreamResolver,
        source: &str,
    ) -> Result<Self, Response> {
        match consult::text::consult(resolver, source, |_| false) {
            Err(_e) => todo!(),
            Ok(None) => todo!(),
            Ok(Some(text)) => {
                let mut ctx = Self {
                    procedures: HashMap::new(),
                    char_conversion: text.char_conversion,
                    flags: text.flags,
                    operators: text.operators,
                };
                for (pi, p) in text.procedures {
                    user_defined::import(&mut ctx, pi, p)?;
                }
                for (t, _) in text.initialization {
                    let mut success = false;
                    match solve::eval(&mut ctx, &t, || {
                        match &t.kind {
                            read_term::TermKind::Atom(s) => {
                                eprintln!("initialization {} ... ok", s)
                            }
                            read_term::TermKind::Compound(c) => {
                                eprintln!("initialization {}/{} ... ok", c.functor, c.args.len())
                            }
                            _ => unreachable!(),
                        };
                        success = true;
                        false
                    }) {
                        Response::Fail | Response::Cut => {
                            if !success {
                                return Err(Response::Fail);
                            }
                        }
                        r => return Err(r),
                    }
                }
                Ok(ctx)
            }
        }
    }

    pub fn eval<F: FnMut() -> bool>(&mut self, goal: &str, callback: F) -> Response {
        todo!()
    }
}
