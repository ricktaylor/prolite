pub(super) mod error;
pub(super) mod program;
pub(super) mod text;

mod multireader;

use super::{flags, operators, read_term};
use read_term::stream;

#[derive(Debug)]
pub struct StreamResolverError {
    pub error: std::io::Error,
    pub path: String,
}

pub trait StreamResolver {
    fn open(
        &mut self,
        name: &str,
    ) -> Result<(String, Box<dyn stream::ReadStream>), StreamResolverError>;
}

type ErrorSinkFn = fn(e: &error::Error) -> bool;

#[cfg(test)]
mod test;
