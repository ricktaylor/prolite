pub mod error;
pub mod parser;
pub mod stream;
pub mod term;
pub mod utf8reader;

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
