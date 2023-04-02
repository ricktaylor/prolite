use super::*;
use error::*;
use multireader::*;

use flags::{QuoteFlag, UnknownFlag};
use operators::Operator;

use read_term::parser;
use read_term::term::{Compound, Term};
use read_term::Context;

pub(super) struct Program {}

impl Program {
    fn new() -> Self {
        Program {}
    }

    fn assert_clause(&mut self, head: &Term, tail: &Term) -> Result<(), Box<Error>> {
        Ok(())
    }

    fn assert_fact(&mut self, term: &Term) -> Result<(), Box<Error>> {
        Ok(())
    }
}

struct ConsultContext<'a> {
    context: Context,
    stream: MultiReader,
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
    ) -> Result<Self, Box<Error>> {
        let (full_name, stream) = resolver.open(source)?;
        Ok(Self {
            context: Default::default(),
            stream: MultiReader::new(stream),
            sink: match error_sink {
                None => Self::null_sink,
                Some(s) => s,
            },
            program,
            resolver,
            loaded_set,
        })
    }

    fn error<E>(&mut self, r: Result<(), Box<E>>) -> Result<(), Box<Error>>
    where
        text::Error: From<E>,
    {
        if let Err(e) = r {
            let e2 = Error::from(*e);
            if (self.sink)(&e2) {
                Ok(())
            } else {
                Err(Box::new(e2))
            }
        } else {
            Ok(())
        }
    }
}

fn load_text(ctx: &mut ConsultContext) -> Result<(), Box<Error>> {
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
                    Term::Compound(_) | Term::Atom(..) => ctx.program.assert_fact(&t),
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
    d: fn(&mut ConsultContext, &Term) -> Result<(), Box<Error>>,
) -> Result<(), Box<Error>> {
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

fn directive(ctx: &mut ConsultContext, term: &Term) -> Result<(), Box<Error>> {
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

fn include(ctx: &mut ConsultContext, term: &Term) -> Result<(), Box<Error>> {
    match term {
        Term::Atom(s, _) => {
            let (_, stream) = ctx.resolver.open(s)?;
            ctx.stream.include(stream)
        }
        _ => Error::new(ErrorKind::BadStreamName(term.clone())),
    }
}

fn ensure_loaded(ctx: &mut ConsultContext, term: &Term) -> Result<(), Box<Error>> {
    match term {
        Term::Atom(s, _) => {
            let (full_path, stream) = ctx.resolver.open(s)?;
            for s in ctx.loaded_set.iter() {
                if *s == full_path {
                    return Ok(());
                }
            }
            ctx.loaded_set.push(full_path);

            // New context and stream
            let prev_ctx = std::mem::take(&mut ctx.context);
            let prev_stream = std::mem::replace(&mut ctx.stream, MultiReader::new(stream));

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
) -> Result<(), Box<Error>> {
    match operator {
        Term::Atom(s, _) if s == "," => Error::new(ErrorKind::InvalidOperator(operator.clone())),
        Term::Atom(s, _) if remove => {
            ctx.context.operators.remove(s);
            Ok(())
        }
        Term::Atom(s, _) => {
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
) -> Result<(), Box<Error>> {
    // Unpack specifier
    let (op_spec, remove) = match specifier {
        Term::Atom(s, _) => {
            // Unpack priority
            let p = match priority {
                Term::Integer(n, _) => match n {
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

fn prolog_flag(ctx: &mut ConsultContext, flag: &Term, value: &Term) -> Result<(), Box<Error>> {
    match flag {
        Term::Atom(s, _) => match s.as_str() {
            "char_conversion" => match value {
                Term::Atom(s, _) => match s.as_str() {
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
                Term::Atom(s, _) => match s.as_str() {
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
                Term::Atom(s, _) => match s.as_str() {
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
                Term::Atom(s, _) => match s.as_str() {
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
                Term::Atom(s, _) => match s.as_str() {
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

fn char_conversion(
    ctx: &mut ConsultContext,
    in_char: &Term,
    out_char: &Term,
) -> Result<(), Box<Error>> {
    match in_char {
        Term::Atom(s, _) if s.len() == 1 => {
            let in_char = s.chars().next().unwrap();
            match out_char {
                Term::Atom(s, _) if s.len() == 1 => {
                    let out_char = s.chars().next().unwrap();

                    if in_char == out_char {
                        ctx.context.char_conversion.remove(&in_char);
                    } else {
                        ctx.context.char_conversion.insert(in_char, out_char);
                    }
                    Ok(())
                }
                _ => Error::new(ErrorKind::InvalidCharacter(out_char.clone())),
            }
        }
        _ => Error::new(ErrorKind::InvalidCharacter(in_char.clone())),
    }
}

fn initialization(ctx: &mut ConsultContext, term: &Term) -> Result<(), Box<Error>> {
    todo!()
}

fn public(ctx: &mut ConsultContext, term: &Term) -> Result<(), Box<Error>> {
    todo!()
}

fn dynamic(ctx: &mut ConsultContext, term: &Term) -> Result<(), Box<Error>> {
    todo!()
}

fn multifile(ctx: &mut ConsultContext, term: &Term) -> Result<(), Box<Error>> {
    todo!()
}

fn discontiguous(ctx: &mut ConsultContext, term: &Term) -> Result<(), Box<Error>> {
    todo!()
}

pub(super) fn consult(
    resolver: &mut dyn StreamResolver,
    source: &str,
    sink: Option<ErrorSinkFn>,
) -> Result<Program, Box<Error>> {
    let mut program = Program::new();
    let mut loaded_set = Vec::new();
    let mut ctx = ConsultContext::new(resolver, source, &mut program, sink, &mut loaded_set)?;
    load_text(&mut ctx)?;

    todo!()
}
