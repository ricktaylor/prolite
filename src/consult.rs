pub(super) mod error;
pub(super) mod text;

mod multireader;

use super::{flags, operators, read_term};
use read_term::stream;

pub trait StreamResolver {
    fn open(&mut self, name: &str)
        -> Result<(String, Box<dyn stream::ReadStream>), std::io::Error>;
}

type ErrorSinkFn = fn(e: &error::Error) -> bool;

#[cfg(test)]
mod test;
