use super::lexer::*;
use super::super::operators;
use super::super::operators::Operator;

#[derive(Debug)]
pub struct Compound {
    functor: String,
    params: Vec<Term>
}

impl Compound {
    pub fn new(functor: &str, term: Term) -> Self {
		Self {
			functor: functor.to_string(),
            params: vec![term]
		}
	}
}

#[derive(Debug)]
pub enum Term {
    Integer(i64),
    Float(f64),
    Var(String),
    Atom(String),
    Compound(Compound),
    Chars(String),
    CharCodes(String)
}

pub enum Error {
	LexErr(super::lexer::Error),
    MissingDot,
    UnexpectedEof,
    MissingClose,
    UnexpectedToken
}

impl From<super::lexer::Error> for Error {
    fn from(e: super::lexer::Error) -> Self {
        Error::LexErr(e)
    }
}

pub struct Parser<'a> {
	context: &'a super::Context,
    lexer: Lexer<'a>
}

impl<'a> Parser<'a> {
	pub fn new(stream: &'a dyn Utf8Stream, context: &'a super::Context) -> Self {
		Self {
			context,
            lexer: Lexer::new(stream,context)
		}
	}

    fn number(&self, s: &str, negative: bool) -> Result<(Term,Token,u16),Error> {
        todo!()
    }

    fn arg(&mut self, token: Token) -> Result<(Term,Token),Error> {
        if let Token::Name(s) = &token {
            match operators::lookup_prefix_op(&self.context.operators,s) {
                Some(&Operator::fx(p)) |
                Some(&Operator::fy(p)) |
                Some(&Operator::xfx(p)) |
                Some(&Operator::xfy(p)) |
                Some(&Operator::yfx(p)) |
                Some(&Operator::xf(p)) |
                Some(&Operator::yf(p)) if p > 999 => {
                    let next = self.lexer.next()?;
                    match next {
                        Token::OpenCt => {
                            let (term,next,p) = self.compound(s)?;
                            Ok((term,next))
                        },
                        Token::Int(_) |
                        Token::BinaryInt(_) |
                        Token::OctalInt(_) |
                        Token::HexInt(_) |
                        Token::CharCode(_) |
                        Token::Float(_) if s == "-" => {
                            let (term,next,p) = self.number(s,true)?;
                            Ok((term,next))
                        },
                        _ => Ok((Term::Atom(s.clone()),next))
                    }
                },
                _ => self.next(token,999)
            }
        } else {
            self.next(token,999)
        }
    }

    fn compound(&mut self, s: &str) -> Result<(Term,Token,u16),Error> {
        let mut c = Compound{
            functor: s.to_string(),
            params: Vec::new()
        };

        let mut next = self.lexer.next()?;
        loop {
            next = {
                let (term,next) = self.arg(next)?;
                c.params.push(term);
                next
            };

            match next {
                Token::Comma => {},
                Token::Close => return Ok((Term::Compound(c),self.lexer.next()?,0)),
                _ => return Err(Error::MissingClose)
            }
        }
    }

    fn name(&mut self, s: &str, max_precedence: u16) -> Result<(Term,Token,u16),Error> {
        let next = self.lexer.next()?;
        match next {
            Token::OpenCt => self.compound(s),
            Token::Int(_) |
            Token::BinaryInt(_) |
            Token::OctalInt(_) |
            Token::HexInt(_) |
            Token::CharCode(_) |
            Token::Float(_) if s == "-" => self.number(s,true),
            Token::Comma |
            Token::Close => Ok((Term::Atom(s.to_string()),next,0)),
            _ => {
                match operators::lookup_prefix_op(&self.context.operators,s) {
                    Some(&Operator::fx(p)) if p <= max_precedence => {
                        let (term,next) = self.next(next,p-1)?;
                        Ok((Term::Compound(Compound::new(s,term)),next,p))
                    },
                    Some(&Operator::fy(p)) if p <= max_precedence => {
                        let (term,next) = self.next(next,p)?;
                        Ok((Term::Compound(Compound::new(s,term)),next,p))
                    },
                    _ => Ok((Term::Atom(s.to_string()),next,0))
                }
            }
        }

    }

    fn lookup_op(&self, s: &str, max_precedence: u16) -> (u16,u16,usize,bool) {
        match operators::lookup_op(&self.context.operators,s) {
            Some(&Operator::fx(p)) |
            Some(&Operator::fy(p)) => (0,0,1,p > max_precedence),
            Some(&Operator::xfx(p)) => (p-1,p-1,2,p > max_precedence),
            Some(&Operator::xfy(p)) => (p-1,p,2,p > max_precedence),
            Some(&Operator::yfx(p)) => (p,p-1,2,p > max_precedence),
            Some(&Operator::xf(p)) => (p-1,0,1,p > max_precedence),
            Some(&Operator::yf(p)) => (p-1,0,1,p > max_precedence),
            _ => (0,0,0,true)
        }
    }

    fn next(&mut self, token: Token, max_precedence: u16) -> Result<(Term,Token),Error> {
        let (mut term,mut next,precedence) = match token {
            Token::Eof => Err(Error::UnexpectedEof),
            Token::Name(s) => self.name(&s,max_precedence),
            Token::Var(s) => Ok((Term::Var(s),self.lexer.next()?,0)),
            Token::Int(_) => todo!(),
            Token::BinaryInt(_) => todo!(),
            Token::OctalInt(_) => todo!(),
            Token::HexInt(_) => todo!(),
            Token::CharCode(_) => todo!(),
            Token::Float(_) => todo!(),
            Token::DoubleQuotedList(s) if matches!(self.context.flags.double_quotes,super::super::prolog_flags::QuoteFlags::Atom) => self.name(&s,max_precedence), /* ISO/IEC 13211-1:1995/Cor.1:2007 */
            Token::DoubleQuotedList(_) => todo!(),
            Token::BackQuotedString(s) if matches!(self.context.flags.back_quotes,super::super::prolog_flags::QuoteFlags::Atom) => self.name(&s,max_precedence),
            Token::BackQuotedString(_) => todo!(),
            Token::Open |
            Token::OpenCt => {
                let next = self.lexer.next()?;
                let (term,next) = self.next(next,1201)?;
                match next {
                    Token::Close => Ok((term,next,0)),
                    Token::Eof => Err(Error::UnexpectedEof),
                    _ => Err(Error::MissingClose)
                }
            },
            Token::OpenC => {
                let next = self.lexer.next()?;
                let (term,next) = self.next(next,1201)?;
                match next {
                    Token::CloseC => Ok((Term::Compound(Compound::new("{}",term)),next,0)),
                    Token::Eof => Err(Error::UnexpectedEof),
                    _ => Err(Error::MissingClose)
                }
            },
            Token::OpenL => todo!(),
            _ => Err(Error::UnexpectedToken)
        }?;

        /* This is precedence climbing, if you're interested */
        loop {
            let (r,l,arity,do_break) = match &next {
                Token::Name(s) => self.lookup_op(s,max_precedence),
                Token::Bar => self.lookup_op("|",max_precedence),  /* ISO/IEC 13211-1:1995/Cor.2:2012 */
                Token::Comma => (999,1000,2,1000 > max_precedence),
                _ => return Ok((term,next))
            };

            if do_break || precedence > l {
                return Ok((term,next));
            }

            let mut c = Compound{
                functor: match &next {
                    Token::Name(s) => s.to_string(),
                    Token::Comma => ",".to_string(),
                    Token::Bar => "|".to_string(),
                    _ => panic!("Operator name is bogus: {:?}",next)
                },
                params: vec![term]
            };

            next = if arity == 2 {
                let (term,next) = self.next(next,r)?;
                c.params.push(term);
                next
            } else {
                next
            };

            term = Term::Compound(c);
        }
    }

    pub fn next_term(&mut self) -> Result<Option<Term>,Error> {
        let t = self.lexer.next()?;
        if let Token::Eof = t {
            return Ok(None);
        }

        let (term,next) = self.next(t,1201)?;
        match next {
            Token::End => Ok(Some(term)),
            _ => Err(Error::MissingDot)
        }
    }
}

