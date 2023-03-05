pub(super) mod parser;
pub(super) mod error;
pub(super) mod term;

mod lexer;

use std::collections::HashMap;
use std::default::Default;
use super::{operators,flags,stream};

#[derive(Debug,Clone)]
pub(super) struct Context {
    pub flags: flags::Flags,
    pub operators: operators::OperatorTable,
    pub char_conversion: HashMap<char,char>
}

impl Default for Context {
    fn default() -> Self {
        Self {
            flags: Default::default(),
            operators: operators::Operator::default_table(),
            char_conversion: HashMap::new()
        }
    }
}

impl Context {
    fn lookup_op(&self, name: &str) -> Option<&operators::Operator> {
        let r = self.operators.get(name)?;
        for o in r.iter() {
            if let operators::Operator::fx(_) | operators::Operator::fy(_) = o {
                continue
            } else {
                return Some(o)
            }
        }
        r.first()
    }

    fn lookup_prefix_op(&self, name: &str) -> Option<&operators::Operator> {
        let r = self.operators.get(name)?;
        for o in r.iter() {
            if let operators::Operator::fx(_) | operators::Operator::fy(_) = o {
                return Some(o)
            } else {
                continue
            }
        }
        r.first()
    }
}
