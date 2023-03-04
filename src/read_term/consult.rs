use super::*;
use multistream::*;
use term::*;
use parser::*;
use error::*;

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
    sink: ErrorSinkFn,
    resolver: &'a mut dyn StreamResolver,
    loaded_set: &'a mut Vec<String>
}

impl<'a> ConsultContext<'a> {
    fn null_sink(_: &Error) -> bool { false }

    fn new(resolver: &'a mut dyn StreamResolver, name: String, source: Box<dyn Stream>, error_sink: Option<ErrorSinkFn>, loaded_set: &'a mut Vec<String>) -> Self {
        let context: Context = Default::default();
        Self {
            context,
            stream: MultiStream::new(&name,source),
            sink: match error_sink {
                None => Self::null_sink,
                Some(s) => s
            },
            resolver,
            loaded_set
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
            match parser::next(&self.context,&mut self.stream) {
                Ok(None) => break,
                Ok(Some(t)) => { let r = self.consult_term(t); self.error(r)? },
                Err(e) => {
                    match e.kind {
                        // Collect up to next whitespace
                        ErrorKind::BadEscape(ref mut s) |
                        ErrorKind::BadInteger(ref mut s) |
                        ErrorKind::BadFloat(ref mut s) |
                        ErrorKind::Unexpected(ref mut s) => {
                            let r = collect_to_whitespace(s,&mut self.stream);
                            self.error(Err(e))?;
                            self.error(r)?;
                            parser::skip_to_end(&self.context,&mut self.stream)?;
                        },
                        ErrorKind::ExpectedToken(_) |
                        ErrorKind::UnexpectedToken(_) |
                        ErrorKind::ParseIntError(_) |
                        ErrorKind::ParseFloatError(_) => {
                            self.error(Err(e))?;
                            parser::skip_to_end(&self.context,&mut self.stream)?;
                        },
                        _ => {
                            self.error(Err(e))?;
                        }
                    }
                }
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
            Term::Var(_) => Error::new(ErrorKind::Instantiation(term)),
            _ => Error::new(ErrorKind::NotCallableTerm(term)),
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
            Term::Var(_) => Error::new(ErrorKind::Instantiation(term.clone())),
            _ => Error::new(ErrorKind::UnknownDirective(term.clone()))
        }
    }

    fn include(&mut self, term: &Term) -> Result<(),Error> {
        match term {
            Term::Atom(s) => {
                let (name,stream) = self.resolver.open(s)?;
                self.stream.include(&name,stream)
            },
            Term::Var(_) => Error::new(ErrorKind::Instantiation(term.clone())),
            _ => Error::new(ErrorKind::BadStreamName(term.clone()))
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

                Self::new(self.resolver,full_path,stream,Some(self.sink),self.loaded_set).consult()?;
                Ok(())
            },
            Term::Var(_) => Error::new(ErrorKind::Instantiation(term.clone())),
            _ => Error::new(ErrorKind::BadStreamName(term.clone()))
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
    let loaded_set = Vec::new();
    let (name,s) = resolver.open(source)?;
    ConsultContext::new(resolver,name,s,sink,&mut loaded_set).consult()
}
