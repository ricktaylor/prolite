use std::collections::HashMap;
use std::default::Default;

#[derive(Debug,Clone)]
pub struct Context {
    pub flags: super::prolog_flags::Flags,
    pub operators: super::operators::OperatorTable,
    pub char_conversion: HashMap<char,char>
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
