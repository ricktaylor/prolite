
use crate::context::Context;
use super::*;
use multistream::*;
use parser::*;

pub enum ErrorKind {
    ParserError(parser::Error),
    NotCallableTerm(Term),
    Instantiation(Term)
}

pub struct Error {
    pub kind: ErrorKind
}

impl From<parser::Error> for Error {
    fn from(e: parser::Error) -> Self {
        Error { kind: ErrorKind::ParserError(e) }
    }
}

impl From<lexer::Error> for Error {
    fn from(e: lexer::Error) -> Self {
        Error::from(parser::Error::from(e))
    }
}

pub type ErrorSinkFn = fn(e: &Error) -> bool;

pub struct Program {

}

impl Program {
    fn new() -> Self {
        Program {

        }
    }

    fn consult_term(&mut self, term: Term) -> Result<(),Error> {
        match term {
            Term::Compound(ref c) if c.functor == ":-" => {
                match c.args.len() {
                    1 => self.directive(&c.args[0]),
                    2 => self.assert_clause(&c.args[0],&c.args[1]),
                    _ => self.assert_fact(term)
                }
            }
            Term::Compound(_) |
            Term::Atom(_) => self.assert_fact(term),
            Term::Var(_) => Err(Error{ kind: ErrorKind::Instantiation(term) }),
            _ => Err(Error{ kind: ErrorKind::NotCallableTerm(term) }),
        }
    }

    fn directive(&mut self, term: &Term) -> Result<(),Error> {
        todo!()
    }

    fn assert_clause(&mut self, head: &Term, tail: &Term) -> Result<(),Error> {
        todo!()
    }

    fn assert_fact(&mut self, term: Term) -> Result<(),Error> {
        todo!()
    }

}

fn sink_error<E>(sink: &ErrorSinkFn, r: Result<(),E>) -> Result<(),Error>
where consult::Error: From<E> {
    if let Err(e) = r {
        let e2 = Error::from(e);
        if sink(&e2) {
            Ok(())
        } else {
            Err(e2)
        }
    } else {
        Ok(())
    }
}

fn null_sink(_: &Error) -> bool { false }

pub fn consult(context: &Context, source: &mut dyn Stream, error_sink: Option<ErrorSinkFn>) -> Result<Program,Error> {

    let mut stream = MultiStream::new(source);
    let mut parser = Parser::new(context,&mut stream);
    let mut program = Program::new();
    let sink = match error_sink {
        None => null_sink,
        Some(s) => s
    };

    loop {
        match parser.next() {
            Ok(None) => return Ok(program),
            Ok(Some(t)) => sink_error(&sink,program.consult_term(t))?,
            Err(parser::Error::TokenError(mut e)) => {
                // Collect up to next whitespace
                let r = match e {
                    lexer::Error::BadEscape(ref mut s) |
                    lexer::Error::BadInteger(ref mut s) |
                    lexer::Error::BadFloat(ref mut s) |
                    lexer::Error::Unexpected(ref mut s) => parser.collect_to_whitespace(s),
                    _ => Ok(())
                };
                sink_error(&sink,Err(e))?;
                sink_error(&sink,r)?;
            },
            Err(e) => sink_error(&sink,Err(e))?,
        }
    }
}
