use super::*;
use std::collections::HashMap;
use std::default::Default;

pub struct Context {
    flags: prolog_flags::Flags,
    operators: operators::OperatorTable,
    char_conversion: HashMap<char,char>
}

impl Default for Context {
    fn default() -> Self {
        Context {
            flags: Default::default(),
            operators: operators::Operator::default_table(),
            char_conversion: HashMap::new()
        }
    }
}
