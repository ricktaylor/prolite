use super::*;
use error::*;
use stream::ReadStream;

pub(super) struct MultiReader {
    streams: Vec<Box<dyn ReadStream>>,
}

impl MultiReader {
    pub(super) fn new(stream: Box<dyn ReadStream>) -> Self {
        Self {
            streams: vec![stream],
        }
    }

    pub(super) fn include(&mut self, stream: Box<dyn ReadStream>) -> Result<(), Box<Error>> {
        if self
            .streams
            .iter()
            .any(|s| s.position().source == stream.position().source)
        {
            return Error::new(Error::IncludeLoop(stream.position().source));
        }
        self.streams.push(stream);
        Ok(())
    }
}

impl ReadStream for MultiReader {
    fn get(&mut self) -> Result<Option<char>, std::io::Error> {
        while let Some(s) = self.streams.last_mut() {
            if let Some(c) = s.get()? {
                return Ok(Some(c));
            }
            self.streams.pop();
        }
        Ok(None)
    }

    fn peek(&mut self) -> Result<Option<char>, std::io::Error> {
        self.streams.last_mut().map_or(Ok(None), |s| s.peek())
    }

    fn position(&self) -> stream::Position {
        self.streams
            .first()
            .map_or(stream::Position::default(), |s| s.position())
    }
}
