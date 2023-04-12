use super::*;
use error::*;
use stream::{ReadStream, Span};

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
        for s in self.streams.iter() {
            if s.position().source == stream.position().source {
                return Error::new(
                    Span::from(&stream.position()),
                    ErrorKind::IncludeLoop(stream.position().source),
                );
            }
        }
        self.streams.push(stream);
        Ok(())
    }
}

impl ReadStream for MultiReader {
    fn get(&mut self) -> Result<Option<char>, stream::Error> {
        while let Some(s) = self.streams.last_mut() {
            if let Some(c) = s.get()? {
                return Ok(Some(c));
            }
            self.streams.pop();
        }
        Ok(None)
    }

    fn peek(&mut self) -> Result<Option<char>, stream::Error> {
        for s in self.streams.iter_mut().rev() {
            if let Some(c) = s.peek()? {
                return Ok(Some(c));
            }
        }
        Ok(None)
    }

    fn position(&self) -> stream::Position {
        if let Some(s) = self.streams.first() {
            s.position()
        } else {
            stream::Position::default()
        }
    }
}
