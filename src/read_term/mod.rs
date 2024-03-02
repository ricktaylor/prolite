pub mod error;
pub mod parser;
pub mod stream;
pub mod term;
pub mod utf8reader;

mod lexer;

use super::{flags, operators};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Context<'a> {
    pub flags: &'a flags::Flags,
    pub operators: &'a operators::OperatorTable,
    pub char_conversion: &'a HashMap<char, char>,
    pub greedy: bool,
}

pub type Response = Result<Option<(Rc<term::Term>, Vec<term::VarInfo>)>, Box<error::Error>>;

impl Context<'_> {
    pub fn read_term(&self, s: &str) -> Response {
        let stream = &mut crate::read_term::utf8reader::Utf8Reader::new(s.as_bytes(), None);
        let mut var_info = Vec::new();
        match parser::next(self, &mut var_info, stream) {
            Err(e) => {
                if self.greedy {
                    while let Err(e) = parser::skip_to_end(self, stream) {
                        if let error::ErrorKind::StreamError(_) = e.kind {
                            break;
                        }
                    }
                }
                Err(e)
            }
            Ok(Some(t)) => Ok(Some((t, var_info))),
            Ok(None) => Ok(None),
        }
    }
}

#[cfg(test)]
pub mod test;
