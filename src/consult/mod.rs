pub mod error;
pub mod text;

mod multireader;

use super::*;
use read_term::stream;

pub trait StreamResolver {
    fn open(&mut self, name: &str)
        -> Result<(String, Box<dyn stream::ReadStream>), std::io::Error>;
}

type ErrorSinkFn = fn(e: &error::Error) -> bool;

#[cfg(test)]
pub mod test;
