use super::*;
use error::*;
use stream::*;

#[derive(Debug)]
pub(crate) enum Token {
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

fn next_char_raw(stream: &mut dyn Stream) -> Result<Option<char>,Error> {
	Ok(stream.get()?)
}

fn peek_char_raw(stream: &mut dyn Stream) -> Result<Option<char>,Error> {
	Ok(stream.peek()?)
}

fn eat_char(stream: &mut dyn Stream) -> Result<(),Error> {
	next_char_raw(stream)?;
	Ok(())
}

fn convert_char(ctx: &Context, c: Option<char>) -> Option<char> {
	if let Some(c) = c {
		match ctx.char_conversion.get(&c) {
			Some(c) => return Some(*c),
			None => return Some(c)
		};
	}
	c
}

fn classify_char(c: Option<char>) -> Char {
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

fn next_char(ctx: &Context, stream: &mut dyn Stream) -> Result<Char,Error> {
	let c = next_char_raw(stream)?;
	Ok(classify_char(convert_char(ctx,c)))
}

fn peek_char(ctx: &Context, stream: &mut dyn Stream) -> Result<Char,Error> {
	let c = peek_char_raw(stream)?;
	Ok(classify_char(convert_char(ctx,c)))
}

fn multiline_comment(ctx: &Context, stream: &mut dyn Stream) -> Result<Char,Error> {
	loop {
		match next_char(ctx,stream)? {
			Char::Eof => return Ok(Char::Eof),
			Char::Graphic('*') => {
				match next_char(ctx,stream)? {
					Char::Eof => return Ok(Char::Eof),
					Char::Graphic('/') => return next_char(ctx,stream),
					_ => {}
					}
			},
			_ => {}
		}
	}
}

fn integral(ctx: &Context, stream: &mut dyn Stream, max: char, radix: u32) -> Result<Token,Error> {
	let mut t = String::new();
	loop {
		match peek_char(ctx,stream)? {
			Char::Digit(c) if c <= max => t.push(c),
			Char::SmallLetter(c) if radix == 16 && ('a'..='f').contains(&c) => t.push(c),
			_ => break
		}
		eat_char(stream)?;
	}

	if t.is_empty() {
		match radix {
			2 => Error::new(ErrorKind::BadInteger("0b".to_string())),
			8 => Error::new(ErrorKind::BadInteger("0o".to_string())),
			_ => Error::new(ErrorKind::BadInteger("0x".to_string()))
		}
	} else {
		Ok(Token::Int(t,radix))
	}
}

fn numeric(ctx: &Context, stream: &mut dyn Stream, c: char) -> Result<Token,Error> {
	let mut t = c.to_string();
	loop {
		match peek_char(ctx,stream)? {
			Char::Digit(c) => t.push(c),
			Char::Graphic('.') => {
				match peek_char(ctx,stream)? {
					Char::Solo('%') |
					Char::Layout(_) |
					Char::Eof => return Ok(Token::Int(t,10)),
					_ => {}
				}
				eat_char(stream)?;
				t.push('.');

				match next_char(ctx,stream)? {
					Char::Digit(c) => t.push(c),
					Char::Underscore => { t.push('_'); return Error::new(ErrorKind::BadFloat(t)) },
					Char::Meta(c) |
					Char::CapitalLetter(c) |
					Char::SmallLetter(c) |
					Char::Graphic(c) |
					Char::Solo(c) => { t.push(c);  return Error::new(ErrorKind::BadFloat(t)) },
					_ => return Error::new(ErrorKind::BadFloat(t))
				}

				loop {
					match peek_char(ctx,stream)? {
						Char::Digit(c) => t.push(c),
						Char::CapitalLetter('E') => {
							eat_char(stream)?;
							t.push('E');
							break;
						},
						Char::SmallLetter('e') => {
							eat_char(stream)?;
							t.push('e');
							break;
						}
						_ => return Ok(Token::Float(t))
					}
					eat_char(stream)?;
				}

				match next_char(ctx,stream)? {
					Char::Graphic('+') => t.push('+'),
					Char::Graphic('-') => t.push('-'),
					Char::Underscore => { t.push('_'); return Error::new(ErrorKind::BadFloat(t)) },
					Char::Digit(c) |
					Char::Meta(c) |
					Char::CapitalLetter(c) |
					Char::SmallLetter(c) |
					Char::Graphic(c) |
					Char::Solo(c) |
					Char::Layout(c) => { t.push(c);  return Error::new(ErrorKind::BadFloat(t)) },
					_ => return Error::new(ErrorKind::BadFloat(t))
				}

				loop {
					match peek_char(ctx,stream)? {
						Char::Digit(c) => t.push(c),
						_ => return Ok(Token::Float(t))
					}
					eat_char(stream)?;
				}
			},
			_ => return Ok(Token::Int(t,10))
		}
		eat_char(stream)?;
	}
}

fn alpha_numeric(ctx: &Context, stream: &mut dyn Stream, c: char) -> Result<String,Error> {
	let mut t = c.to_string();
	loop {
		match peek_char(ctx,stream)? {
			Char::Underscore => t.push('_'),
			Char::SmallLetter(c) |
			Char::CapitalLetter(c) |
			Char::Digit(c) => t.push(c),
			_ => return Ok(t)
		}
		eat_char(stream)?;
	}
}

fn quoted(stream: &mut dyn Stream, quote: char, greedy: bool) -> Result<String,Error> {
	let mut t = String::new();
	loop {
		match next_char_raw(stream)? {
			None => return Error::new(ErrorKind::Missing(quote)),
			Some('\\') => {
				match next_char_raw(stream)? {
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
					Some(c) if ('0'..='7').contains(&c) => {
						if greedy {

						}
						todo!()
					},
					Some(c) => return Error::new(ErrorKind::BadEscape(String::from_iter(vec!['\\',c]))),
					None => return Error::new(ErrorKind::BadEscape("\\".to_string()))
				}
			},
			Some(c) if c == quote => {
				match peek_char_raw(stream)? {
					Some(c) if c == quote => {
						t.push(c);
						eat_char(stream)?;
					},
					_ => return Ok(t)
				}
			},
			Some(c) => t.push(c)
		}
	}
}

pub(super) fn next(ctx: &Context, stream: &mut dyn Stream, greedy: bool) -> Result<Token,Error> {
	let mut c = next_char(ctx,stream)?;

	// open ct (* 6.4 *)
	if let Char::Solo('(') = c {
		return Ok(Token::OpenCt)
	}

	loop {
		match c {
			Char::Eof => return Ok(Token::Eof),

			// layout text sequence (* 6.4.1 *)
			Char::Layout(_) => { c = next_char(ctx,stream)?; },

			// single line comment (* 6.4.1 *)
			Char::Solo('%') => {
				loop {
					match next_char(ctx,stream)? {
						Char::Eof => return Ok(Token::Eof),
						Char::Layout('\n') => {
							c = next_char(ctx,stream)?;
							break;
						},
						_ => {}
					}
				}
			},

			// name (* 6.4 *)
			// letter digit token (* 6.4.2 *)
			Char::SmallLetter(c) => return Ok(Token::Name(alpha_numeric(ctx,stream,c)?)),

			// graphic token (* 6.4.2 *)
			Char::Graphic('.') => {
				let mut t = String::from('.');
				match peek_char(ctx,stream)? {
					Char::Solo('%') |
					Char::Layout(_) |
					Char::Eof => return Ok(Token::End),
					Char::Graphic(c) => t.push(c),
					Char::Meta('\\') => t.push('\\'),
					_ => return Ok(Token::Name(t))
				}
				eat_char(stream)?;

				loop {
					match peek_char(ctx,stream)? {
						Char::Graphic(c) => t.push(c),
						Char::Meta('\\') => t.push('\\'),
						_ => return Ok(Token::Name(t))
					}
					eat_char(stream)?;
				}
			},
			Char::Graphic('/') => {
				// bracketed comment (* 6.4.1 *)
				c = peek_char(ctx,stream)?;
				if let Char::Graphic('*') = c {
					eat_char(stream)?;
					c = multiline_comment(ctx,stream)?;
				}
				else {
					let mut t = String::from('/');
					loop {
						match c {
							Char::Graphic(c) => t.push(c),
							Char::Meta('\\') => t.push('\\'),
							_ => return Ok(Token::Name(t))
						}
						eat_char(stream)?;
						c = peek_char(ctx,stream)?;
					}
				}
			},
			Char::Graphic(c) => {
				let mut t = c.to_string();
				loop {
					match peek_char(ctx,stream)? {
						Char::Graphic(c) => t.push(c),
						Char::Meta('\\') => t.push('\\'),
						_ => return Ok(Token::Name(t))
					}
					eat_char(stream)?;
				}
			},

			// quoted token (* 6.4.2 *)
			Char::Meta('\'') => return Ok(Token::Name(quoted(stream,'\'',greedy)?)),

			// semicolon token (* 6.4.2 *)
			Char::Solo(';') => return Ok(Token::Name(String::from(';'))),

			// cut token (* 6.4.2 *)
			Char::Solo('!') => return Ok(Token::Name(String::from('!'))),

			// variable (* 6.4 *)
			Char::Underscore => return Ok(Token::Var(alpha_numeric(ctx,stream,'_')?)),
			Char::CapitalLetter(c) => return Ok(Token::Var(alpha_numeric(ctx,stream,c)?)),

			// integer (* 6.4 *)
			// float number (* 6.4 *)
			Char::Digit('0') => {
				match peek_char(ctx,stream)? {
					Char::Meta('\'') => todo!(),
					Char::SmallLetter('b') => {
						eat_char(stream)?;
						return integral(ctx,stream,'1',2);
					},
					Char::SmallLetter('o') => {
						eat_char(stream)?;
						return integral(ctx,stream,'7',8);
					},
					Char::SmallLetter('x') => {
						eat_char(stream)?;
						return integral(ctx,stream,'9',16);
					},
					Char::Digit(c) => {
						eat_char(stream)?;
						return numeric(ctx,stream,c);
					},
					_ => return Ok(Token::Int('0'.to_string(),10))
				}
			},
			Char::Digit(c) => return numeric(ctx,stream,c),

			// double quoted list (* 6.4 *)
			Char::Meta('"') => return Ok(Token::DoubleQuotedList(quoted(stream,'"',greedy)?)),

			// back quoted string (* 6.4.7 *)
			Char::Meta('`') => return Ok(Token::BackQuotedString(quoted(stream,'`',greedy)?)),

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
			Char::Meta(c) |
			Char::Invalid(c) => {
				let mut s = c.to_string();
				if greedy {
					while let Char::Invalid(c) = peek_char(ctx,stream)? {
						s.push(c);
						eat_char(stream)?;
					}
				}
				return Error::new(ErrorKind::Unexpected(s))
			}
		}
	}
}
