mod parser;
mod consult;
mod token;
mod multistream;
mod error;
mod term;

use std::collections::HashMap;
use std::default::Default;
use super::*;

#[derive(Debug)]
pub enum StreamError {
    IOError(std::io::Error)
}

impl From<std::io::Error> for StreamError {
    fn from(e: std::io::Error) -> Self {
        StreamError::IOError(e)
    }
}

pub trait Stream {
	fn get(&mut self) -> Result<Option<char>,StreamError>;
	fn peek(&mut self) -> Result<Option<char>,StreamError>;
}

#[derive(Debug)]
pub enum StreamResolverError {
    IOError(std::io::Error)
}

pub trait StreamResolver {
	fn open(&mut self, name: &str) -> Result<(String,Box<dyn Stream>),StreamResolverError>;
	fn full_path(&mut self, name: &str) -> Result<String,StreamResolverError>;
}

#[derive(Debug,Clone)]
pub struct Context {
    pub flags: prolog_flags::Flags,
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