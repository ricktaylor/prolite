pub(super) mod error;
pub(super) mod parser;
pub(super) mod stream;
pub(super) mod term;
pub(super) mod utf8reader;

mod lexer;

use super::{flags, operators};
use std::collections::HashMap;
use std::default::Default;

#[derive(Debug, Clone)]
pub(super) struct Context {
    pub flags: flags::Flags,
    pub operators: operators::OperatorTable,
    pub char_conversion: HashMap<char, char>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            flags: Default::default(),
            operators: operators::Operator::default_table(),
            char_conversion: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod test;
