use crate::flags::{QuoteFlag, UnknownFlag};
use crate::operators::Operator;

use super::*;
use error::*;
use multistream::*;

use read_term::parser;
use read_term::term::{Compound, Term};
use read_term::Context;

pub(super) struct Program {}

impl Program {
    fn new() -> Self {
        Program {}
    }

    fn assert_clause(&mut self, head: &Term, tail: &Term) -> Result<(), Error> {
        Ok(())
    }

    fn assert_fact(&mut self, term: &Term) -> Result<(), Error> {
        Ok(())
    }
}

struct ConsultContext<'a> {
    context: Context,
    stream: MultiStream,
    sink: ErrorSinkFn,
    program: &'a mut Program,
    resolver: &'a mut dyn StreamResolver,
    loaded_set: &'a mut Vec<String>,
}

impl<'a> ConsultContext<'a> {
    fn null_sink(_: &Error) -> bool {
        false
    }

    fn new(
        resolver: &'a mut dyn StreamResolver,
        source: &str,
        program: &'a mut Program,
        error_sink: Option<ErrorSinkFn>,
        loaded_set: &'a mut Vec<String>,
    ) -> Result<Self, Error> {
        let (full_name, stream) = resolver.open(source)?;
        Ok(Self {
            context: Default::default(),
            stream: MultiStream::new(&full_name, stream),
            sink: match error_sink {
                None => Self::null_sink,
                Some(s) => s,
            },
            program,
            resolver,
            loaded_set,
        })
    }

    fn error<E>(&mut self, r: Result<(), E>) -> Result<(), Error>
    where
        text::Error: From<E>,
    {
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
}

fn load_text(ctx: &mut ConsultContext) -> Result<(), Error> {
    loop {
        match parser::next(&ctx.context, &mut ctx.stream, true) {
            Ok(None) => break,
            Ok(Some(t)) => {
                let r = match t {
                    Term::Compound(c) if c.functor == ":-" => match c.args.len() {
                        1 => directive(ctx, &c.args[0]),
                        2 => ctx.program.assert_clause(&c.args[0], &c.args[1]),
                        _ => ctx.program.assert_fact(&Term::Compound(c)),
                    },
                    Term::Compound(_) | Term::Atom(_) => ctx.program.assert_fact(&t),
                    _ => Error::new(ErrorKind::NotCallableTerm(t)),
                };
                ctx.error(r)?;
            }
            Err(e) => {
                ctx.error(Err(e))?;
                parser::skip_to_end(&ctx.context, &mut ctx.stream)?;
            }
        }
    }
    Ok(())
}

fn directive_expand(
    ctx: &mut ConsultContext,
    c: &Compound,
    d: fn(&mut ConsultContext, &Term) -> Result<(), Error>,
) -> Result<(), Error> {
    if c.args.len() == 1 {
        if let Some(list) = c.args[0].list_iter() {
            for arg in list {
                (d)(ctx, arg)?;
            }
        } else {
            (d)(ctx, &c.args[0])?;
        }
    } else {
        for arg in c.args.iter() {
            (d)(ctx, arg)?;
        }
    }
    Ok(())
}

fn directive(ctx: &mut ConsultContext, term: &Term) -> Result<(), Error> {
    match term {
        Term::Compound(c) if c.functor == "op" && c.args.len() == 3 => {
            op(ctx, &c.args[0], &c.args[1], &c.args[2])
        }
        Term::Compound(c) if c.functor == "initialization" && c.args.len() == 1 => {
            initialization(ctx, &c.args[0])
        }
        Term::Compound(c) if c.functor == "set_prolog_flag" && c.args.len() == 2 => {
            prolog_flag(ctx, &c.args[0], &c.args[1])
        }
        Term::Compound(c) if c.functor == "char_conversion" && c.args.len() == 2 => {
            char_conversion(ctx, &c.args[0], &c.args[1])
        }
        Term::Compound(c) if c.functor == "include" => directive_expand(ctx, c, include),
        Term::Compound(c) if c.functor == "ensure_loaded" => {
            directive_expand(ctx, c, ensure_loaded)
        }
        Term::Compound(c) if c.functor == "public" => directive_expand(ctx, c, public),
        Term::Compound(c) if c.functor == "dynamic" => directive_expand(ctx, c, dynamic),
        Term::Compound(c) if c.functor == "multifile" => directive_expand(ctx, c, multifile),
        Term::Compound(c) if c.functor == "discontiguous" => {
            directive_expand(ctx, c, discontiguous)
        }
        _ => Error::new(ErrorKind::UnknownDirective(term.clone())),
    }
}

fn include(ctx: &mut ConsultContext, term: &Term) -> Result<(), Error> {
    match term {
        Term::Atom(s) => {
            let (name, stream) = ctx.resolver.open(s)?;
            ctx.stream.include(&name, stream)
        }
        _ => Error::new(ErrorKind::BadStreamName(term.clone())),
    }
}

fn ensure_loaded(ctx: &mut ConsultContext, term: &Term) -> Result<(), Error> {
    match term {
        Term::Atom(s) => {
            let (full_path, stream) = ctx.resolver.open(s)?;
            for s in ctx.loaded_set.iter() {
                if *s == full_path {
                    return Ok(());
                }
            }
            ctx.loaded_set.push(full_path.clone());

            // New context and stream
            let prev_ctx = std::mem::take(&mut ctx.context);
            let prev_stream =
                std::mem::replace(&mut ctx.stream, MultiStream::new(&full_path, stream));

            load_text(ctx)?;

            // Restore the previous context
            ctx.context = prev_ctx;
            ctx.stream = prev_stream;
            Ok(())
        }
        _ => Error::new(ErrorKind::BadStreamName(term.clone())),
    }
}

fn update_op(
    ctx: &mut ConsultContext,
    specifier: &Operator,
    operator: &Term,
    remove: bool,
) -> Result<(), Error> {
    match operator {
        Term::Atom(s) if s == "," => Error::new(ErrorKind::InvalidOperator(operator.clone())),
        Term::Atom(s) if remove => {
            ctx.context.operators.remove(s);
            Ok(())
        }
        Term::Atom(s) => {
            if let Some(ops) = ctx.context.operators.get_mut(s) {
                let mut found = false;
                for o in ops.iter_mut() {
                    match o {
                        Operator::fx(p1) => {
                            if let Operator::fx(p2) = specifier {
                                *p1 = *p2;
                                found = true;
                                break;
                            }
                        }
                        Operator::fy(p1) => {
                            if let Operator::fy(p2) = specifier {
                                *p1 = *p2;
                                found = true;
                                break;
                            }
                        }
                        Operator::xfx(p1) => match specifier {
                            Operator::xfx(p2) => {
                                *p1 = *p2;
                                found = true;
                                break;
                            }
                            Operator::xf(_) | Operator::yf(_) => {
                                return Error::new(ErrorKind::InvalidOpCombo(
                                    operator.clone(),
                                    o.clone(),
                                    specifier.clone(),
                                ))
                            }
                            _ => (),
                        },
                        Operator::xfy(p1) => match specifier {
                            Operator::xfy(p2) => {
                                *p1 = *p2;
                                found = true;
                                break;
                            }
                            Operator::xf(_) | Operator::yf(_) => {
                                return Error::new(ErrorKind::InvalidOpCombo(
                                    operator.clone(),
                                    o.clone(),
                                    specifier.clone(),
                                ))
                            }
                            _ => (),
                        },
                        Operator::yfx(p1) => match specifier {
                            Operator::yfx(p2) => {
                                *p1 = *p2;
                                found = true;
                                break;
                            }
                            Operator::xf(_) | Operator::yf(_) => {
                                return Error::new(ErrorKind::InvalidOpCombo(
                                    operator.clone(),
                                    o.clone(),
                                    specifier.clone(),
                                ))
                            }
                            _ => (),
                        },
                        Operator::xf(p1) => match specifier {
                            Operator::xf(p2) => {
                                *p1 = *p2;
                                found = true;
                                break;
                            }
                            Operator::xfx(_) | Operator::xfy(_) | Operator::yfx(_) => {
                                return Error::new(ErrorKind::InvalidOpCombo(
                                    operator.clone(),
                                    o.clone(),
                                    specifier.clone(),
                                ))
                            }
                            _ => (),
                        },
                        Operator::yf(p1) => match specifier {
                            Operator::yf(p2) => {
                                *p1 = *p2;
                                found = true;
                                break;
                            }
                            Operator::xfx(_) | Operator::xfy(_) | Operator::yfx(_) => {
                                return Error::new(ErrorKind::InvalidOpCombo(
                                    operator.clone(),
                                    o.clone(),
                                    specifier.clone(),
                                ))
                            }
                            _ => (),
                        },
                    }
                }
                if !found {
                    ops.push(specifier.clone());
                }
                ops.sort_unstable_by_key(|k| k.priority());
            } else {
                ctx.context
                    .operators
                    .insert(s.clone(), vec![specifier.clone()]);
            }
            Ok(())
        }
        _ => Error::new(ErrorKind::InvalidOperator(operator.clone())),
    }
}

fn op(
    ctx: &mut ConsultContext,
    priority: &Term,
    specifier: &Term,
    operator: &Term,
) -> Result<(), Error> {
    // Unpack specifier
    let (op_spec, remove) = match specifier {
        Term::Atom(s) => {
            // Unpack priority
            let p = match priority {
                Term::Integer(n) => match n {
                    0..=1200 => *n as u16,
                    _ => return Error::new(ErrorKind::InvalidOpPriority(priority.clone())),
                },
                _ => return Error::new(ErrorKind::InvalidOpPriority(priority.clone())),
            };

            match s.as_str() {
                "fx" => (Operator::fx(p), p == 0),
                "fy" => (Operator::fy(p), p == 0),
                "xfx" => (Operator::xfx(p), p == 0),
                "xfy" => (Operator::xfy(p), p == 0),
                "yfx" => (Operator::yfx(p), p == 0),
                "xf" => (Operator::xf(p), p == 0),
                "yf" => (Operator::yf(p), p == 0),
                _ => return Error::new(ErrorKind::InvalidOpSpecifier(specifier.clone())),
            }
        }
        _ => return Error::new(ErrorKind::InvalidOpSpecifier(specifier.clone())),
    };

    // Iterate atom_or_atom_list
    if let Some(l) = operator.list_iter() {
        for o in l {
            update_op(ctx, &op_spec, o, remove)?;
        }
    } else {
        update_op(ctx, &op_spec, operator, remove)?;
    }
    Ok(())
}

fn prolog_flag(ctx: &mut ConsultContext, flag: &Term, value: &Term) -> Result<(), Error> {
    match flag {
        Term::Atom(s) => match s.as_str() {
            "char_conversion" => match value {
                Term::Atom(s) => match s.as_str() {
                    "on" => {
                        ctx.context.flags.char_conversion = true;
                        Ok(())
                    }
                    "off" => {
                        ctx.context.flags.char_conversion = false;
                        Ok(())
                    }
                    _ => Error::new(ErrorKind::InvalidFlagValue(flag.clone(), value.clone())),
                },
                _ => Error::new(ErrorKind::InvalidFlagValue(flag.clone(), value.clone())),
            },
            "debug" => match value {
                Term::Atom(s) => match s.as_str() {
                    "on" => {
                        ctx.context.flags.debug = true;
                        Ok(())
                    }
                    "off" => {
                        ctx.context.flags.debug = false;
                        Ok(())
                    }
                    _ => Error::new(ErrorKind::InvalidFlagValue(flag.clone(), value.clone())),
                },
                _ => Error::new(ErrorKind::InvalidFlagValue(flag.clone(), value.clone())),
            },
            "unknown" => match value {
                Term::Atom(s) => match s.as_str() {
                    "error" => {
                        ctx.context.flags.unknown = UnknownFlag::Error;
                        Ok(())
                    }
                    "fail" => {
                        ctx.context.flags.unknown = UnknownFlag::Fail;
                        Ok(())
                    }
                    "warning" => {
                        ctx.context.flags.unknown = UnknownFlag::Warning;
                        Ok(())
                    }
                    _ => Error::new(ErrorKind::InvalidFlagValue(flag.clone(), value.clone())),
                },
                _ => Error::new(ErrorKind::InvalidFlagValue(flag.clone(), value.clone())),
            },
            "double_quotes" => match value {
                Term::Atom(s) => match s.as_str() {
                    "chars" => {
                        ctx.context.flags.double_quotes = QuoteFlag::Chars;
                        Ok(())
                    }
                    "codes" => {
                        ctx.context.flags.double_quotes = QuoteFlag::Codes;
                        Ok(())
                    }
                    "atom" => {
                        ctx.context.flags.double_quotes = QuoteFlag::Atom;
                        Ok(())
                    }
                    _ => Error::new(ErrorKind::InvalidFlagValue(flag.clone(), value.clone())),
                },
                _ => Error::new(ErrorKind::InvalidFlagValue(flag.clone(), value.clone())),
            },
            "back_quotes" => match value {
                Term::Atom(s) => match s.as_str() {
                    "chars" => {
                        ctx.context.flags.back_quotes = QuoteFlag::Chars;
                        Ok(())
                    }
                    "codes" => {
                        ctx.context.flags.back_quotes = QuoteFlag::Codes;
                        Ok(())
                    }
                    "atom" => {
                        ctx.context.flags.back_quotes = QuoteFlag::Atom;
                        Ok(())
                    }
                    _ => Error::new(ErrorKind::InvalidFlagValue(flag.clone(), value.clone())),
                },
                _ => Error::new(ErrorKind::InvalidFlagValue(flag.clone(), value.clone())),
            },
            _ => Error::new(ErrorKind::InvalidFlag(flag.clone())),
        },
        _ => Error::new(ErrorKind::InvalidFlag(flag.clone())),
    }
}

fn char_conversion(ctx: &mut ConsultContext, in_char: &Term, out_char: &Term) -> Result<(), Error> {
    todo!()
}

fn initialization(ctx: &mut ConsultContext, term: &Term) -> Result<(), Error> {
    todo!()
}

fn public(ctx: &mut ConsultContext, term: &Term) -> Result<(), Error> {
    todo!()
}

fn dynamic(ctx: &mut ConsultContext, term: &Term) -> Result<(), Error> {
    todo!()
}

fn multifile(ctx: &mut ConsultContext, term: &Term) -> Result<(), Error> {
    todo!()
}

fn discontiguous(ctx: &mut ConsultContext, term: &Term) -> Result<(), Error> {
    todo!()
}

pub(super) fn consult(
    resolver: &mut dyn StreamResolver,
    source: &str,
    sink: Option<ErrorSinkFn>,
) -> Result<Program, Error> {
    let mut program = Program::new();
    let mut loaded_set = Vec::new();
    let mut ctx = ConsultContext::new(resolver, source, &mut program, sink, &mut loaded_set)?;
    load_text(&mut ctx)?;

    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        env,
        fs::{File, OpenOptions},
        io::{self},
        path::{Path, PathBuf},
    };
    use stream::Stream;

    struct FSStream {
        reader: utf8_read::Reader<File>,
        next: Option<utf8_read::Char>,
    }

    impl FSStream {
        fn new(fp: &Path) -> io::Result<FSStream> {
            Ok(Self {
                reader: utf8_read::Reader::new(OpenOptions::new().read(true).open(fp)?),
                next: None,
            })
        }
    }

    impl Stream for FSStream {
        fn get(&mut self) -> Result<Option<char>, stream::Error> {
            let r = self.peek()?;
            self.next = None;
            Ok(r)
        }

        fn peek(&mut self) -> Result<Option<char>, stream::Error> {
            if let None = self.next {
                self.next = Some(self.reader.next_char().unwrap());
            }
            match self.next {
                Some(utf8_read::Char::Char(c)) => Ok(Some(c)),
                _ => Ok(None),
            }
        }
    }

    struct FSResolver {
        root: PathBuf,
    }

    impl FSResolver {
        fn new(root: &str) -> io::Result<FSResolver> {
            Ok(Self {
                root: env::current_dir()?.join(root).canonicalize()?,
            })
        }
    }

    impl StreamResolver for FSResolver {
        fn open(&mut self, name: &str) -> Result<(String, Box<dyn Stream>), StreamResolverError> {
            let mut fp = self.root.join(name);
            let mut s = FSStream::new(&fp);
            if let Err(e) = &s {
                match &e.kind() {
                    std::io::ErrorKind::NotFound if matches!(fp.extension(), None) => {
                        fp.set_extension("pl");
                        s = FSStream::new(&fp);
                    }
                    _ => {}
                }
            }

            match s {
                Ok(s) => {
                    let n = fp
                        .canonicalize()
                        .unwrap()
                        .into_os_string()
                        .into_string()
                        .unwrap();
                    println!("Opened {}", n);
                    Ok((n, Box::new(s)))
                }
                Err(e) => Err(StreamResolverError {
                    error: e,
                    path: fp.into_os_string().into_string().unwrap(),
                }),
            }
        }
    }

    fn error(e: &Error) -> bool {
        println!("{:?}", e);
        true
    }

    #[test]
    fn test_consult() {
        let mut res = FSResolver::new("./test/vanilla/").unwrap();
        consult(&mut res, "vanilla.pl", Some(error)).unwrap();
    }
}
