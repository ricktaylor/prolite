use super::*;

#[derive(Debug)]
pub enum StreamResolverError {
    IOError(std::io::Error)
}

pub trait StreamResolver {
	fn open(&mut self, name: &str) -> Result<(String,Box<dyn Stream>),StreamResolverError>;
	fn full_path(&mut self, name: &str) -> Result<String,StreamResolverError>;
}

struct StreamItem {
	name: String,
	stream: Box<dyn Stream>
}

pub struct MultiStream {
    streams: Vec<StreamItem>,
}

impl MultiStream {

    pub fn new(name: &str, stream: Box<dyn Stream>) -> Self {
		Self {
			streams: vec![StreamItem{name: name.to_string(),stream}]
		}
	}

    pub fn include(&mut self, name: &str, stream: Box<dyn Stream>) -> Result<(),consult::Error> {
		for s in self.streams.iter() {
			if s.name == name {
				return Err(consult::Error::new(consult::ErrorKind::IncludeLoop(name.to_string())));
			}
		}
		self.streams.push(StreamItem{name: name.to_string(),stream});
		Ok(())
	}
}

impl Stream for MultiStream {
    fn get(&mut self) -> Result<Option<char>,StreamError> {
        while let Some(s)= self.streams.last_mut() {
			if let Some(c) = s.stream.get()? {
				return Ok(Some(c));
			}
			self.streams.pop();
		}
		Ok(None)
    }

	fn peek(&mut self) -> Result<Option<char>,StreamError> {
        for s in self.streams.iter_mut().rev() {
			if let Some(c) = s.stream.peek()? {
				return Ok(Some(c));
			}
		}
		Ok(None)
    }
}
