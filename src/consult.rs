pub(super) mod error;
pub(super) mod text;

mod multistream;

use std::ffi::OsString;

use crate::{read_term, stream};
use stream::*;

#[derive(Debug)]
pub struct StreamResolverError {
    pub error: std::io::Error,
    pub path: String,
}

pub trait StreamResolver {
    fn open(&mut self, name: &str) -> Result<(String, Box<dyn Stream>), StreamResolverError>;
}

type ErrorSinkFn = fn(e: &error::Error) -> bool;
