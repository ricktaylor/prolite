pub enum Error {
	Missing(char),
	BadEscape(String),
	BadFloat(String),
	InvalidChar(char),
	Unexpected(char),
	MissingDigit,
    IOError(std::io::Error)
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IOError(e)
    }
}

#[derive(Debug)]
pub enum Token {
    Eof,
	Name(String),
	Var(String),
	Int(String,u32),
	CharCode(char),
	Float(String),
	DoubleQuotedList(String),
	BackQuotedString(String),
	Open,
	OpenCt,
	Close,
	OpenL,
	CloseL,
	OpenC,
	CloseC,
	Bar,
	Comma,
	End
}

pub trait Utf8Stream {
	fn get(&self) -> Result<Option<char>,Error>;
	fn peek(&self) -> Result<Option<char>,Error>;
}

pub struct Lexer<'a> {
	stream: &'a dyn Utf8Stream,
	context: &'a super::Context
}

enum Char {
	Layout(char),
	Solo(char),
	Meta(char),
	Digit(char),
	Underscore,
	CapitalLetter(char),
	SmallLetter(char),
	Graphic(char),
	Eof
}

impl<'a> Lexer<'a> {
	pub fn new(stream: &'a dyn Utf8Stream, context: &'a super::Context) -> Self {
		Self {
			stream,
			context
		}
	}

	pub fn next(&mut self) -> Result<Token,Error> {

		let mut c = self.next_char()?;

		// open ct (* 6.4 *)
		if let Char::Solo('(') = c {
			return Ok(Token::OpenCt)
		}

		loop {
			match c {
				Char::Eof => return Ok(Token::Eof),

				// layout text sequence (* 6.4.1 *)
				Char::Layout(_) => { c = self.next_char()?; },

				// single line comment (* 6.4.1 *)
				Char::Solo('%') => {
					loop {
						match self.next_char()? {
							Char::Eof => return Ok(Token::Eof),
							Char::Layout('\n') => {
								c = self.next_char()?;
								break;
							},
							_ => {}
						}
					}
				},

				// name (* 6.4 *)
				// letter digit token (* 6.4.2 *)
				Char::SmallLetter(c) => return Ok(Token::Name(self.alpha_numeric(c)?)),

				// graphic token (* 6.4.2 *)
				Char::Graphic('.') => {
					let mut t = String::from('.');
					match self.peek_char()? {
						Char::Solo('%') |
						Char::Layout(_) |
						Char::Eof => return Ok(Token::End),
						Char::Graphic(c) => t.push(c),
						Char::Meta('\\') => t.push('\\'),
						_ => return Ok(Token::Name(t))
					}
					self.next_char()?;

					loop {
						match self.peek_char()? {
							Char::Graphic(c) => t.push(c),
							Char::Meta('\\') => t.push('\\'),
							_ => return Ok(Token::Name(t))
						}
						self.next_char()?;
					}
				},
				Char::Graphic('/') => {
					// bracketed comment (* 6.4.1 *)
					c = self.peek_char()?;
					if let Char::Graphic('*') = c {
						self.next_char()?;
						c = self.multiline_comment()?;
					}
					else {
						let mut t = String::from('/');
						loop {
							match c {
								Char::Graphic(c) => t.push(c),
								Char::Meta('\\') => t.push('\\'),
								_ => return Ok(Token::Name(t))
							}
							self.next_char()?;
							c = self.peek_char()?;
						}
					}
				},
				Char::Graphic(c) => {
					let mut t = c.to_string();
					loop {
						match self.peek_char()? {
							Char::Graphic(c) => t.push(c),
							Char::Meta('\\') => t.push('\\'),
							_ => return Ok(Token::Name(t))
						}
						self.next_char()?;
					}
				},

				// quoted token (* 6.4.2 *)
				Char::Meta('\'') => return Ok(Token::Name(self.quoted('\'')?)),

				// semicolon token (* 6.4.2 *)
				Char::Solo(';') => return Ok(Token::Name(String::from(';'))),

				// cut token (* 6.4.2 *)
				Char::Solo('!') => return Ok(Token::Name(String::from('!'))),

				// variable (* 6.4 *)
				Char::Underscore => return Ok(Token::Var(self.alpha_numeric('_')?)),
				Char::CapitalLetter(c) => return Ok(Token::Var(self.alpha_numeric(c)?)),

				// integer (* 6.4 *)
				// float number (* 6.4 *)
				Char::Digit('0') => {
					match self.peek_char()? {
						Char::Meta('\'') => todo!(),
						Char::SmallLetter('b') => {
							self.next_char()?;
							return self.integral('1',2);
						},
						Char::SmallLetter('o') => {
							self.next_char()?;
							return self.integral('7',8);
						},
						Char::SmallLetter('x') => {
							self.next_char()?;
							return self.integral('9',16);
						},
						Char::Digit(c) => {
							self.next_char()?;
							return self.numeric(c);
						},
						_ => return Ok(Token::Int('0'.to_string(),10))
					}
				},
				Char::Digit(c) => return self.numeric(c),

				// double quoted list (* 6.4 *)
				Char::Meta('"') => return Ok(Token::DoubleQuotedList(self.quoted('"')?)),

				// back quoted string (* 6.4.7 *)
				Char::Meta('`') => return Ok(Token::BackQuotedString(self.quoted('`')?)),

				// open (* 6.4 *)
				Char::Solo('(') => return Ok(Token::Open),

				// close (* 6.4 *)
				Char::Solo(')') => return Ok(Token::Close),

				// open list (* 6.4 *)
				Char::Solo('[') => return Ok(Token::OpenL),

				// close list (* 6.4 *)
				Char::Solo(']') => return Ok(Token::CloseL),

				// open curly (* 6.4 *)
				Char::Solo('{') => return Ok(Token::OpenC),

				// close curly (* 6.4 *)
				Char::Solo('}') => return Ok(Token::CloseC),

				// ht sep (* 6.4 *)
				Char::Solo('|') => return Ok(Token::Bar),

				// comma (* 6.4 *)
				Char::Solo(',') => return Ok(Token::Comma),

				Char::Solo(c) => return Err(Error::Unexpected(c)),
				Char::Meta(c) => return Err(Error::Unexpected(c))
			}
		}
	}

	fn convert_char(&self, c: Option<char>) -> Result<Option<char>,Error> {
		if let Some(c) = c {
			match self.context.char_conversion.get(&c) {
				Some(c) => return Ok::<Option<char>,Error>(Some(*c)),
				None => return Ok(Some(c))
			};
		}
		Ok(c)
	}

	fn classify_char(&self, c: Option<char>) -> Result<Char,Error> {
		match c {
			None => Ok(Char::Eof),
			Some(c) => match c {
				' ' |
				'\t' |
				'\n' => Ok(Char::Layout(c)),
				'!' |
				'(' |
				')' |
				',' |
				';' |
				'[' |
				']' |
				'{' |
				'}' |
				'|' |
				'%' => Ok(Char::Solo(c)),
				'\\' |
				'\'' |
				'"' |
				'`' => Ok(Char::Meta(c)),
				'0' ..= '9' => Ok(Char::Digit(c)),
				'_' => Ok(Char::Underscore),
				'A' ..= 'Z' => Ok(Char::CapitalLetter(c)),
				'a' ..= 'z' => Ok(Char::SmallLetter(c)),
				'#' |
				'$' |
				'&' |
				'*' |
				'+' |
				'-' |
				'.' |
				'/' |
				':' |
				'<' |
				'=' |
				'>' |
				'?' |
				'@' |
				'^' |
				'~' => Ok(Char::Graphic(c)),
				_ => Err(Error::InvalidChar(c))
			}
		}
	}

	fn next_char(&mut self) -> Result<Char,Error> {
		self.classify_char(self.convert_char(self.stream.get()?)?)
	}

	fn peek_char(&mut self) -> Result<Char,Error> {
		self.classify_char(self.convert_char(self.stream.peek()?)?)
	}

	fn multiline_comment(&mut self) -> Result<Char,Error> {
		loop {
			match self.next_char()? {
				Char::Eof => return Ok(Char::Eof),
				Char::Graphic('*') => {
					match self.next_char()? {
						Char::Eof => return Ok(Char::Eof),
						Char::Graphic('/') => return self.next_char(),
						_ => {}
						}
				},
				_ => {}
			}
		}
	}

	fn integral(&mut self, max: char, radix: u32) -> Result<Token,Error> {
		let mut t = String::new();
		loop {
			match self.peek_char()? {
				Char::Digit(c) if c <= max => t.push(c),
				Char::SmallLetter(c) if radix == 16 && ('a'..='f').contains(&c) => t.push(c),
				_ => break
			}
			self.next_char()?;
		}

		if t.is_empty() {
			Err(Error::MissingDigit)
		} else {
			Ok(Token::Int(t,radix))
		}
	}

	fn numeric(&mut self, c: char) -> Result<Token,Error> {
		let mut t = c.to_string();
		loop {
			match self.peek_char()? {
				Char::Digit(c) => t.push(c),
				Char::Graphic('.') => {
					match self.peek_char()? {
						Char::Solo('%') |
						Char::Layout(_) |
						Char::Eof => return Ok(Token::Int(t,10)),
						_ => {}
					}
					self.next_char()?;
					t.push('.');

					match self.next_char()? {
						Char::Digit(c) => t.push(c),
						Char::Underscore => { t.push('_'); return Err(Error::BadFloat(t)) },
						Char::Meta(c) |
						Char::CapitalLetter(c) |
						Char::SmallLetter(c) |
						Char::Graphic(c) |
						Char::Solo(c) => { t.push(c);  return Err(Error::BadFloat(t)) },
						_ => return Err(Error::BadFloat(t))
					}

					loop {
						match self.peek_char()? {
							Char::Digit(c) => t.push(c),
							Char::CapitalLetter('E') => {
								self.next_char()?;
								t.push('E');
								break;
							},
							Char::SmallLetter('e') => {
								self.next_char()?;
								t.push('e');
								break;
							}
							_ => return Ok(Token::Float(t))
						}
						self.next_char()?;
					}

					match self.next_char()? {
						Char::Graphic('+') => t.push('+'),
						Char::Graphic('-') => t.push('-'),
						Char::Underscore => { t.push('_'); return Err(Error::BadFloat(t)) },
						Char::Digit(c) |
						Char::Meta(c) |
						Char::CapitalLetter(c) |
						Char::SmallLetter(c) |
						Char::Graphic(c) |
						Char::Solo(c) |
						Char::Layout(c) => { t.push(c);  return Err(Error::BadFloat(t)) },
						_ => return Err(Error::BadFloat(t))
					}

					loop {
						match self.peek_char()? {
							Char::Digit(c) => t.push(c),
							_ => return Ok(Token::Float(t))
						}
						self.next_char()?;
					}
				},
				_ => return Ok(Token::Int(t,10))
			}
			self.next_char()?;
		}
	}

	fn alpha_numeric(&mut self, c: char) -> Result<String,Error> {
		let mut t = c.to_string();
		loop {
			match self.peek_char()? {
				Char::Underscore => t.push('_'),
				Char::SmallLetter(c) |
				Char::CapitalLetter(c) |
				Char::Digit(c) => t.push(c),
				_ => return Ok(t)
			}
			self.next_char()?;
		}
	}

	fn quoted(&mut self, quote: char) -> Result<String,Error> {
		let mut t = String::new();
		loop {
			match self.stream.get()? {
				None => return Err(Error::Missing(quote)),
				Some('\\') => {
					match self.stream.get()? {
						Some('\n') => {},
						Some('\\') => t.push('\\'),
						Some('\'') => t.push('\''),
						Some('"') => t.push('"'),
						Some('`') => t.push('`'),
						Some('a') => t.push('\x07'),
						Some('b') => t.push('\x08'),
						Some('r') => t.push('\r'),
						Some('f') => t.push('\x0C'),
						Some('n') => t.push('\n'),
						Some('v') => t.push('\x0B'),
						Some('x') => todo!(),
						Some(c) if ('0'..='7').contains(&c) => todo!(),
						Some(c) => return Err(Error::BadEscape(String::from_iter(vec!['\\',c]))),
						None => return Err(Error::BadEscape("\\".to_string()))
					}
				},
				Some(c) if c == quote => {
					match self.stream.peek()? {
						Some(c) if c == quote => {
							t.push(c);
							self.stream.get()?;
						},
    					_ => return Ok(t)
					}
				},
    			Some(c) => t.push(c)
			}
		}
	}


}
