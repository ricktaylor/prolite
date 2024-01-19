pub(super) mod error;
pub(super) mod parser;
pub(super) mod stream;
pub(super) mod term;
pub(super) mod utf8reader;

mod lexer;

use super::{flags, operators};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Context<'a> {
    pub flags: &'a flags::Flags,
    pub operators: &'a operators::OperatorTable,
    pub char_conversion: &'a HashMap<char, char>,
    pub greedy: bool,
}

#[cfg(test)]
pub mod test;
