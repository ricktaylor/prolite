use crate::context::Context;
use super::*;
use multistream::*;
use parser::*;


#[derive(Debug)]
pub enum ErrorKind {
    ParserError(parser::Error),
    NotCallableTerm(Term),
    Instantiation(Term),
    UnknownDirective(Term),
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind
}

impl Error {
    fn new(kind: ErrorKind) -> Self {
        Self {
            kind
        }
    }
}

impl From<parser::Error> for Error {
    fn from(e: parser::Error) -> Self {
        Error::new(ErrorKind::ParserError(e))
    }
}

impl From<lexer::Error> for Error {
    fn from(e: lexer::Error) -> Self {
        Error::from(parser::Error::from(e))
    }
}

impl From<StreamError> for Error {
    fn from(e: StreamError) -> Self {
        Error::from(lexer::Error::from(e))
    }
}

pub struct Program {

}

impl Program {
    fn new() -> Self {
        Program {

        }
    }

    fn assert_clause(&mut self, head: Term, tail: Term) -> Result<(),Error> {
        todo!()
    }

    fn assert_fact(&mut self, term: Term) -> Result<(),Error> {
        todo!()
    }

}

fn collect_to_whitespace(s: &mut String, stream: &mut dyn Stream) -> Result<(),StreamError> {
    loop {
        match stream.peek()? {
            None |
            Some(' ') |
            Some('\t') |
            Some('\n') => break Ok(()),
            Some(c) => { s.push(c); stream.get()?; }
        }
    }
}

struct ConsultContext<'a> {
    context: Context,
    stream: MultiStream<'a>,
    parser: Parser<'a>,
    program: Program,
    sink: ErrorSinkFn
}

impl<'a> ConsultContext<'a> {
    fn null_sink(_: &Error) -> bool { false }

    fn new(context: &'a Context, source: &'a mut dyn Stream, error_sink: Option<ErrorSinkFn>) -> Self {
        Self {
            context: context.clone(),
            stream: MultiStream::new(source),
            parser: Parser::new(context),
            program: Program::new(),
            sink: match error_sink {
                None => Self::null_sink,
                Some(s) => s
            }
        }
    }

    fn error<E>(&mut self, r: Result<(),E>) -> Result<(),Error>
    where consult::Error: From<E> {
        if let Err(e) = r {
            let e2 = Error::from(e);
            if (self.sink)(&e2) {
                Ok(())
            } else {
                Err(e2)
            }
        } else {
            Ok(())
        }
    }

    pub fn consult(&mut self) -> Result<Program,Error> {
        loop {
            match self.parser.next(&mut self.stream) {
                Ok(None) => break,
                Ok(Some(t)) => { let r = self.consult_term(t); self.error(r)? },
                Err(parser::Error::TokenError(mut e)) => {
                    // Collect up to next whitespace
                    let r = match e {
                        lexer::Error::BadEscape(ref mut s) |
                        lexer::Error::BadInteger(ref mut s) |
                        lexer::Error::BadFloat(ref mut s) |
                        lexer::Error::Unexpected(ref mut s) => collect_to_whitespace(s,&mut self.stream),
                        _ => Ok(())
                    };
                    self.error(Err(e))?;
                    self.error(r)?;
                    self.parser.skip_to_end(&mut self.stream)?;
                },
                Err(e) => {
                    self.error(Err(e))?;
                    self.parser.skip_to_end(&mut self.stream)?;
                },
            }
        }

        todo!()
    }

    fn consult_term(&mut self, term: Term) -> Result<(),Error> {
        match term {
            Term::Compound(mut c) if c.functor == ":-" => {
                match c.args.len() {
                    1 => { let head = c.args.pop().unwrap(); self.directive(head) },
                    2 => { let tail = c.args.pop().unwrap(); let head = c.args.pop().unwrap();  self.program.assert_clause(head,tail) },
                    _ => self.program.assert_fact(Term::Compound(c))
                }
            }
            Term::Compound(_) |
            Term::Atom(_) => self.program.assert_fact(term),
            Term::Var(_) => Err(Error::new(ErrorKind::Instantiation(term))),
            _ => Err(Error::new(ErrorKind::NotCallableTerm(term))),
        }
    }

    fn directive(&mut self, term: Term) -> Result<(),Error> {
        match term {
            Term::Compound(mut c) if c.functor == "include" && c.args.len() == 1 => self.include(c.args.pop().unwrap()),
            Term::Compound(mut c) if c.functor == "op" && c.args.len() == 3 => {
                let operator = c.args.pop().unwrap();
                let specifier = c.args.pop().unwrap();
                let priority = c.args.pop().unwrap();
                self.op(priority,specifier,operator)
            },
            Term::Compound(mut c) if c.functor == "ensure_loaded" && c.args.len() == 1 => self.ensure_loaded(c.args.pop().unwrap()),
            Term::Compound(mut c) if c.functor == "initialization" && c.args.len() == 1 => self.initialization(c.args.pop().unwrap()),
            Term::Compound(mut c) if c.functor == "set_prolog_flag" && c.args.len() == 2 => {
                let value = c.args.pop().unwrap();
                let flag = c.args.pop().unwrap();
                self.prolog_flag(flag,value)
            },
            Term::Compound(mut c) if c.functor == "char_conversion" && c.args.len() == 2 => {
                let out_char = c.args.pop().unwrap();
                let in_char = c.args.pop().unwrap();
                self.char_conversion(in_char,out_char)
            },
            Term::Compound(mut c) if c.functor == "public" => { todo!() },
            Term::Compound(mut c) if c.functor == "dynamic" => { todo!() },
            Term::Compound(mut c) if c.functor == "multifile" => { todo!() },
            Term::Compound(mut c) if c.functor == "discontiguous" => { todo!() },
            Term::Var(_) => Err(Error::new(ErrorKind::Instantiation(term))),
            _ => Err(Error::new(ErrorKind::UnknownDirective(term)))
        }
    }

    fn include(&mut self, term: Term) -> Result<(),Error> {
        match term {
            Term::Atom(s) => { todo!() },
            _ => todo!()
        }
    }

    fn ensure_loaded(&mut self, term: Term) -> Result<(),Error> {
        match term {
            Term::Atom(s) => { todo!() },
            _ => todo!()
        }
    }

    fn op(&mut self, priority: Term, specifier: Term, operator: Term) -> Result<(),Error> {
        todo!()
    }

    fn prolog_flag(&mut self, flag: Term, value: Term) -> Result<(),Error> {
        todo!()
    }

    fn char_conversion(&mut self, in_char: Term, out_char: Term) -> Result<(),Error> {
        todo!()
    }

    fn initialization(&mut self, term: Term) -> Result<(),Error> {
        todo!()
    }
}

pub type ErrorSinkFn = fn(e: &Error) -> bool;

pub fn consult(context: &Context, source: &mut dyn Stream, sink: Option<ErrorSinkFn>) -> Result<Program,Error> {
    ConsultContext::new(context,source,sink).consult()
}
