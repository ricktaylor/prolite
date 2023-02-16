use std::str::FromStr;
use super::lexer::*;
use super::super::operators;
use super::super::operators::Operator;
use super::super::prolog_flags::QuoteFlags;

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

#[allow(clippy::enum_variant_names)]
pub enum Error {
	LexerError(super::lexer::Error),
    MissingDot,
    UnexpectedEof,
    MissingClose(char),
    UnexpectedToken(Token),
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError)
}

impl From<super::lexer::Error> for Error {
    fn from(e: super::lexer::Error) -> Self {
        Error::LexerError(e)
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Self {
        Error::ParseIntError(e)
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(e: std::num::ParseFloatError) -> Self {
        Error::ParseFloatError(e)
    }
}

pub struct Parser<'a> {
	context: &'a super::Context,
    lexer: Lexer<'a>
}

fn integer(s: String, radix: u32) -> Result<Term,Error> {
    Ok(Term::Integer(i64::from_str_radix(&s,radix)?))
}

fn float(s: String) -> Result<Term,Error> {
    Ok(Term::Float(f64::from_str(&s)?))
}

impl<'a> Parser<'a> {
	pub fn new(stream: &'a dyn Utf8Stream, context: &'a super::Context) -> Self {
		Self {
			context,
            lexer: Lexer::new(stream,context)
		}
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
                        Token::OpenCt => Ok((self.compound(s)?,self.lexer.next()?)),
                        Token::Int(t,r) if s == "-" => Ok((integer(format!("{}{}",s,t),r)?,self.lexer.next()?)),
                        Token::CharCode(c) if s == "-" => Ok((Term::Integer(-(c as i64)),self.lexer.next()?)),
                        Token::Float(t) if s == "-" => Ok((float(format!("{}{}",s,t))?,self.lexer.next()?)),
                        _ => Ok((Term::Atom(s.clone()),next))
                    }
                },
                _ => self.next(token,999)
            }
        } else {
            self.next(token,999)
        }
    }

    fn compound(&mut self, s: &str) -> Result<Term,Error> {
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
                Token::Close => return Ok(Term::Compound(c)),
                _ => return Err(Error::MissingClose(')'))
            }
        }
    }

    fn name(&mut self, s: &str, max_precedence: u16) -> Result<(Term,Token,u16),Error> {
        let next = self.lexer.next()?;
        match next {
            Token::OpenCt => Ok((self.compound(s)?,self.lexer.next()?,0)),
            Token::Int(t,r) if s == "-" => Ok((integer(format!("{}{}",s,t),r)?,self.lexer.next()?,0)),
            Token::CharCode(c) if s == "-" => Ok((Term::Integer(-(c as i64)),self.lexer.next()?,0)),
            Token::Float(t) if s == "-" => Ok((float(format!("{}{}",s,t))?,self.lexer.next()?,0)),
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

    fn list(&mut self) -> Result<Term,Error> {
        let mut terms = Vec::new();
        let mut token = self.lexer.next()?;
        if ! matches!(token,Token::CloseL) {
            loop {
                let (term,next) = self.arg(token)?;
                terms.push(term);
                token = next;

                match token {
                    Token::Comma => token = self.lexer.next()?,
                    _ => break
                }
            }
        }

        let mut list = match token {
            Token::Bar => {
                let (term,next) = self.arg(token)?;
                token = next;
                term
            },
            _ => Term::Atom("[]".to_string())
        };

        if ! matches!(token,Token::CloseL) {
            return Err(Error::MissingClose(']'));
        }

        loop {
            match terms.pop() {
                None => break,
                Some(t) => {
                    let mut c = Compound::new(".",t);
                    c.params.push(list);
                    list = Term::Compound(c);
                }
            }
        }
        Ok(list)
    }

    fn quoted(&mut self, flags: &QuoteFlags, s: String, max_precedence: u16) -> Result<(Term,Token,u16),Error> {
        match flags {
            QuoteFlags::Atom => self.name(&s,max_precedence),
            QuoteFlags::Chars => {
                let mut list = Term::Atom("[]".to_string());
                for c in s.chars().rev() {
                    let mut c = Compound::new(".",Term::Atom(c.to_string()));
                    c.params.push(list);
                    list = Term::Compound(c);
                }
                Ok((list,self.lexer.next()?,0))
            },
            QuoteFlags::Codes => {
                let mut list = Term::Atom("[]".to_string());
                for c in s.chars().rev() {
                    let mut c = Compound::new(".",Term::Integer(c as i64));
                    c.params.push(list);
                    list = Term::Compound(c);
                }
                Ok((list,self.lexer.next()?,0))
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
            Token::Eof => return Err(Error::UnexpectedEof),
            Token::Name(s) => self.name(&s,max_precedence)?,
            Token::Var(s) => (Term::Var(s),self.lexer.next()?,0),
            Token::Int(s,r) => (integer(s,r)?,self.lexer.next()?,0),
            Token::CharCode(c) => (Term::Integer(c as i64),self.lexer.next()?,0),
            Token::Float(s) => (float(s)?,self.lexer.next()?,0),
            Token::DoubleQuotedList(s) => self.quoted(&self.context.flags.double_quotes,s,max_precedence)?, /* ISO/IEC 13211-1:1995/Cor.1:2007 */
            Token::BackQuotedString(s) => self.quoted(&self.context.flags.back_quotes,s,max_precedence)?,
            Token::Open |
            Token::OpenCt => {
                let next = self.lexer.next()?;
                let (term,next) = self.next(next,1201)?;
                match next {
                    Token::Close => (term,next,0),
                    Token::Eof => return Err(Error::UnexpectedEof),
                    _ => return Err(Error::MissingClose(')'))
                }
            },
            Token::OpenC => {
                let next = self.lexer.next()?;
                let (term,next) = self.next(next,1201)?;
                match next {
                    Token::CloseC => (Term::Compound(Compound::new("{}",term)),next,0),
                    Token::Eof => return Err(Error::UnexpectedEof),
                    _ => return Err(Error::MissingClose('}'))
                }
            },
            Token::OpenL => (self.list()?,self.lexer.next()?,0),
            _ => return Err(Error::UnexpectedToken(token))
        };

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

