use super::*;

pub struct MultiStream<'a> {
    streams: Vec<&'a mut dyn Stream>,
}

impl<'a> MultiStream<'a> {

    pub fn new(stream: &'a mut dyn Stream) -> Self {
		Self {
			streams: vec![stream]
		}
	}

    pub fn include(&mut self, stream: &'a mut dyn Stream) {
		self.streams.push(stream);
	}
}

impl Stream for MultiStream<'_> {
    fn get(&mut self) -> Result<Option<char>,StreamError> {
        while let Some(s)= self.streams.last_mut() {
			if let Some(c) = s.get()? {
				return Ok(Some(c));
			}
			self.streams.pop();
		}
		Ok(None)
    }

	fn peek(&mut self) -> Result<Option<char>,StreamError> {
        for s in self.streams.iter_mut().rev() {
			if let Some(c) = s.peek()? {
				return Ok(Some(c));
			}
		}
		Ok(None)
    }
}
