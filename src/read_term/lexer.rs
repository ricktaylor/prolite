use super::super::context::Context;
use super::*;

pub enum Error {
	Missing(char),
	BadEscape(String),
	BadFloat(String),
	BadInteger(String),
	Unexpected(String),
    StreamError(StreamError)
}

impl From<StreamError> for Error {
    fn from(e: StreamError) -> Self {
        Error::StreamError(e)
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

pub struct Lexer<'a> {
	pub stream: &'a dyn Stream,
	context: &'a Context,
	greedy: bool
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
	Invalid(char),
	Eof
}

impl<'a> Lexer<'a> {
	pub fn new(stream: &'a dyn Stream, context: &'a Context, greedy: bool) -> Self {
		Self {
			stream,
			context,
			greedy
		}
	}

	pub fn next(&mut self) -> Result<Token,Error> {
		let n = self.next_token();
		match n {
			Err(Error::BadEscape(s)) if self.greedy => Err(Error::BadEscape(self.skip_to_whitespace(s)?)),
			Err(Error::BadInteger(s)) if self.greedy => Err(Error::BadInteger(self.skip_to_whitespace(s)?)),
			Err(Error::BadFloat(s)) if self.greedy => Err(Error::BadFloat(self.skip_to_whitespace(s)?)),
			Err(Error::Unexpected(s)) if self.greedy => Err(Error::Unexpected(self.skip_to_whitespace(s)?)),
			_ => n
		}
	}

	fn next_token(&mut self) -> Result<Token,Error> {

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
					self.eat_char()?;

					loop {
						match self.peek_char()? {
							Char::Graphic(c) => t.push(c),
							Char::Meta('\\') => t.push('\\'),
							_ => return Ok(Token::Name(t))
						}
						self.eat_char()?;
					}
				},
				Char::Graphic('/') => {
					// bracketed comment (* 6.4.1 *)
					c = self.peek_char()?;
					if let Char::Graphic('*') = c {
						self.eat_char()?;
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
							self.eat_char()?;
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
						self.eat_char()?;
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
							self.eat_char()?;
							return self.integral('1',2);
						},
						Char::SmallLetter('o') => {
							self.eat_char()?;
							return self.integral('7',8);
						},
						Char::SmallLetter('x') => {
							self.eat_char()?;
							return self.integral('9',16);
						},
						Char::Digit(c) => {
							self.eat_char()?;
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

				Char::Solo(c) |
				Char::Invalid(c) |
				Char::Meta(c) => return Err(Error::Unexpected(c.to_string())),
			}
		}
	}

	fn skip_to_whitespace(&mut self, mut s: String) -> Result<String,Error> {
		loop {
			match self.peek_char_raw()? {
				None |
				Some(' ') |
				Some('\t') |
				Some('\n') => break Ok(s),
				Some(c) => { s.push(c); self.eat_char()?; }
			}
		}
	}

	fn convert_char(&self, c: Option<char>) -> Option<char> {
		if let Some(c) = c {
			match self.context.char_conversion.get(&c) {
				Some(c) => return Some(*c),
				None => return Some(c)
			};
		}
		c
	}

	fn classify_char(&self, c: Option<char>) -> Char {
		match c {
			None => Char::Eof,
			Some(c) => match c {
				' ' |
				'\t' |
				'\n' => Char::Layout(c),
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
				'%' => Char::Solo(c),
				'\\' |
				'\'' |
				'"' |
				'`' => Char::Meta(c),
				'0' ..= '9' => Char::Digit(c),
				'_' => Char::Underscore,
				'A' ..= 'Z' => Char::CapitalLetter(c),
				'a' ..= 'z' => Char::SmallLetter(c),
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
				'~' => Char::Graphic(c),
				_ => Char::Invalid(c)
			}
		}
	}

	fn next_char(&mut self) -> Result<Char,Error> {
		let c = self.next_char_raw()?;
		Ok(self.classify_char(self.convert_char(c)))
	}

	fn next_char_raw(&mut self) -> Result<Option<char>,Error> {
		Ok(self.stream.get()?)
	}

	fn peek_char(&mut self) -> Result<Char,Error> {
		let c = self.peek_char_raw()?;
		Ok(self.classify_char(self.convert_char(c)))
	}

	fn peek_char_raw(&mut self) -> Result<Option<char>,Error> {
		Ok(self.stream.peek()?)
	}

	fn eat_char(&mut self) -> Result<(),Error> {
		self.next_char_raw()?;
		Ok(())
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
			self.eat_char()?;
		}

		if t.is_empty() {
			match radix {
				2 => Err(Error::BadInteger("0b".to_string())),
				8 => Err(Error::BadInteger("0o".to_string())),
				_ => Err(Error::BadInteger("0x".to_string()))
			}
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
					self.eat_char()?;
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
								self.eat_char()?;
								t.push('E');
								break;
							},
							Char::SmallLetter('e') => {
								self.eat_char()?;
								t.push('e');
								break;
							}
							_ => return Ok(Token::Float(t))
						}
						self.eat_char()?;
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
						self.eat_char()?;
					}
				},
				_ => return Ok(Token::Int(t,10))
			}
			self.eat_char()?;
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
			self.eat_char()?;
		}
	}

	fn quoted(&mut self, quote: char) -> Result<String,Error> {
		let mut t = String::new();
		loop {
			match self.next_char_raw()? {
				None => return Err(Error::Missing(quote)),
				Some('\\') => {
					match self.next_char_raw()? {
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
					match self.peek_char_raw()? {
						Some(c) if c == quote => {
							t.push(c);
							self.eat_char()?;
						},
    					_ => return Ok(t)
					}
				},
    			Some(c) => t.push(c)
			}
		}
	}


}
