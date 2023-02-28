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
    BadStreamName(Term),
    IncludeLoop(String),
    StreamResolverError(multistream::StreamResolverError)
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
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

impl From<multistream::StreamResolverError> for Error {
    fn from(e: multistream::StreamResolverError) -> Self {
        Error::new(ErrorKind::StreamResolverError(e))
    }
}

pub struct Program {

}

impl Program {
    fn new() -> Self {
        Program {

        }
    }

    fn assert_clause(&mut self, head: &Term, tail: &Term) -> Result<(),Error> {
        todo!()
    }

    fn assert_fact(&mut self, term: &Term) -> Result<(),Error> {
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
    stream: MultiStream,
    parser: Parser<'a>,
    program: Program,
    sink: ErrorSinkFn,
    resolver: &'a mut dyn StreamResolver,
    loaded_set: Vec<String>
}

impl<'a> ConsultContext<'a> {
    fn null_sink(_: &Error) -> bool { false }

    fn new(context: &'a Context, resolver: &'a mut dyn StreamResolver, name: String, source: Box<dyn Stream>, error_sink: Option<ErrorSinkFn>) -> Self {
        Self {
            context: context.clone(),
            stream: MultiStream::new(&name,source),
            parser: Parser::new(context),
            program: Program::new(),
            sink: match error_sink {
                None => Self::null_sink,
                Some(s) => s
            },
            resolver,
            loaded_set: Vec::new()
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
            Term::Compound(c) if c.functor == ":-" => {
                match c.args.len() {
                    1 => self.directive(&c.args[0]),
                    2 => self.program.assert_clause(&c.args[0],&c.args[1]),
                    _ => self.program.assert_fact(&Term::Compound(c))
                }
            }
            Term::Compound(_) |
            Term::Atom(_) => self.program.assert_fact(&term),
            Term::Var(_) => Err(Error::new(ErrorKind::Instantiation(term))),
            _ => Err(Error::new(ErrorKind::NotCallableTerm(term))),
        }
    }

    fn directive_expand(&mut self, c: &Compound, d: fn(&mut Self, &Term) -> Result<(),Error>) -> Result<(),Error> {
        if c.args.len() == 1 {
            if let Some(list) = c.args[0].list_iter() {
                for arg in list {
                    (d)(self,arg)?;
                }
            } else {
                (d)(self,&c.args[0])?;
            }
        } else {
            for arg in c.args.iter() {
                (d)(self,arg)?;
            }
        }
        Ok(())
    }

    fn directive(&mut self, term: &Term) -> Result<(),Error> {
        match term {
            Term::Compound(c) if c.functor == "op" && c.args.len() == 3 => self.op(&c.args[0],&c.args[1],&c.args[2]),
            Term::Compound(c) if c.functor == "initialization" && c.args.len() == 1 => self.initialization(&c.args[0]),
            Term::Compound(c) if c.functor == "set_prolog_flag" && c.args.len() == 2 => self.prolog_flag(&c.args[0],&c.args[1]),
            Term::Compound(c) if c.functor == "char_conversion" && c.args.len() == 2 => self.char_conversion(&c.args[0],&c.args[1]),
            Term::Compound(c) if c.functor == "include" => self.directive_expand(c,Self::include),
            Term::Compound(c) if c.functor == "ensure_loaded" => self.directive_expand(c,Self::ensure_loaded),
            Term::Compound(c) if c.functor == "public" => self.directive_expand(c,Self::public),
            Term::Compound(c) if c.functor == "dynamic" => self.directive_expand(c,Self::dynamic),
            Term::Compound(c) if c.functor == "multifile" => self.directive_expand(c,Self::multifile),
            Term::Compound(c) if c.functor == "discontiguous" => self.directive_expand(c,Self::discontiguous),
            Term::Var(_) => Err(Error::new(ErrorKind::Instantiation(term.clone()))),
            _ => Err(Error::new(ErrorKind::UnknownDirective(term.clone())))
        }
    }

    fn include(&mut self, term: &Term) -> Result<(),Error> {
        match term {
            Term::Atom(s) => {
                let (name,stream) = self.resolver.open(s)?;
                self.stream.include(&name,stream)
            },
            Term::Var(_) => Err(Error::new(ErrorKind::Instantiation(term.clone()))),
            _ => Err(Error::new(ErrorKind::BadStreamName(term.clone())))
        }
    }

    fn ensure_loaded(&mut self, term: &Term) -> Result<(),Error> {
        match term {
            Term::Atom(s) => {
                let full_path = self.resolver.full_path(s)?;
                for s in self.loaded_set.iter() {
                    if *s == full_path {
                        return Ok(());
                    }
                }
                self.loaded_set.push(full_path.clone());

                let (_,stream) = self.resolver.open(&full_path)?;
                let old_stream = std::mem::replace(&mut self.stream,MultiStream::new(&full_path,stream));
                self.consult()?;
                self.stream = old_stream;
                Ok(())
            },
            Term::Var(_) => Err(Error::new(ErrorKind::Instantiation(term.clone()))),
            _ => Err(Error::new(ErrorKind::BadStreamName(term.clone())))
        }
    }

    fn op(&mut self, priority: &Term, specifier: &Term, operator: &Term) -> Result<(),Error> {
        todo!()
    }

    fn prolog_flag(&mut self, flag: &Term, value: &Term) -> Result<(),Error> {
        todo!()
    }

    fn char_conversion(&mut self, in_char: &Term, out_char: &Term) -> Result<(),Error> {
        todo!()
    }

    fn initialization(&mut self, term: &Term) -> Result<(),Error> {
        todo!()
    }

    fn public(&mut self, term: &Term) -> Result<(),Error> {
        todo!()
    }

    fn dynamic(&mut self, term: &Term) -> Result<(),Error> {
        todo!()
    }

    fn multifile(&mut self, term: &Term) -> Result<(),Error> {
        todo!()
    }

    fn discontiguous(&mut self, term: &Term) -> Result<(),Error> {
        todo!()
    }
}

pub type ErrorSinkFn = fn(e: &Error) -> bool;

pub fn consult(context: &Context, resolver: &mut dyn StreamResolver, source: &str, sink: Option<ErrorSinkFn>) -> Result<Program,Error> {
    let (name,s) = resolver.open(source)?;
    ConsultContext::new(context,resolver,name,s,sink).consult()
}
