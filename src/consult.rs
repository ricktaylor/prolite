pub(super) mod text;
pub(super) mod error;

mod multistream;

use crate::{stream,read_term};
use stream::*;

#[derive(Debug)]
pub enum StreamResolverError {
    IOError(std::io::Error)
}

pub trait StreamResolver {
	fn open(&mut self, name: &str) -> Result<(String,Box<dyn Stream>),StreamResolverError>;
	fn full_path(&mut self, name: &str) -> Result<String,StreamResolverError>;
}

type ErrorSinkFn = fn(e: &error::Error) -> bool;
