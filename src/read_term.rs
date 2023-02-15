use std::collections::HashMap;
use std::default::Default;

pub struct Context {
    flags: super::prolog_flags::Flags,
    operators: super::operators::OperatorTable,
    char_conversion: HashMap<char,char>
}

impl Default for Context {
    fn default() -> Self {
        Self {
            flags: Default::default(),
            operators: super::operators::Operator::default_table(),
            char_conversion: HashMap::new()
        }
    }
}

mod lexer;
mod parser;
