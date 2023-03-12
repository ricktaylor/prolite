pub(super) mod error;
pub(super) mod text;

mod multistream;

use super::{flags, operators, read_term, stream};

#[derive(Debug)]
pub struct StreamResolverError {
    pub error: std::io::Error,
    pub path: String,
}

pub trait StreamResolver {
    fn open(
        &mut self,
        name: &str,
    ) -> Result<(String, Box<dyn stream::Stream>), StreamResolverError>;
}

type ErrorSinkFn = fn(e: &error::Error) -> bool;
