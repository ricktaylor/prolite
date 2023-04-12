use super::*;
use error::*;
use multireader::*;
use program::*;

use flags::{QuoteFlag, UnknownFlag};
use operators::Operator;

use read_term::parser;
use read_term::term::{Compound, Term, TermKind};
use read_term::Context;

struct ConsultContext<'a> {
    context: Context,
    stream: MultiReader,
    sink: ErrorSinkFn,
    program: &'a mut Program,
    resolver: &'a mut dyn StreamResolver,
    loaded_set: &'a mut Vec<String>,
    current_procedure: Option<String>,
    current_text: String,
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
            current_procedure: None,
            current_text: full_name,
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

fn predicate_indicator(term: &Term) -> Result<String, Box<Error>> {
    match &term.kind {
        TermKind::Atom(s) => Ok(format!("{}/0", s)),
        TermKind::Compound(c) => Ok(format!("{}/{}", c.functor, c.args.len())),
        _ => Error::new(
            term.location.clone(),
            ErrorKind::NotCallableTerm(term.clone()),
        ),
    }
}

fn assert_clause(ctx: &mut ConsultContext, head: Term, tail: Term) -> Result<(), Box<Error>> {
    let pi = predicate_indicator(&head)?;
    if let Some(p) = ctx.program.procedures.get(&pi) {
        // Check discontiguous/multifile flags
        if !p.predicates.is_empty() {
            if !p.flags.multifile && ctx.current_text != p.source_text {
                return Error::new(head.location, ErrorKind::NotMultifile(pi));
            }

            if !p.flags.discontiguous {
                match &ctx.current_procedure {
                    Some(s) => {
                        if *s != pi {
                            return Error::new(head.location, ErrorKind::NotDiscontiguous(pi));
                        }
                    }
                    None => {}
                }
            }
        }
    } else {
        let mut p = Procedure {
            source_text: ctx.current_text.clone(),
            ..Default::default()
        };

        ctx.program.procedures.insert(pi.clone(), p);
    }
    ctx.current_procedure = Some(pi);
    Ok(())
}

fn assert_fact(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    let l = term.location.clone();
    assert_clause(ctx, term, Term::new_atom("true".to_string(), l))
}

fn load_text(ctx: &mut ConsultContext) -> Result<(), Box<Error>> {
    loop {
        match parser::next(&ctx.context, &mut ctx.stream, true) {
            Ok(None) => break,
            Ok(Some(t)) => {
                let r = match t.kind {
                    TermKind::Compound(mut c) if c.functor == ":-" => match c.args.len() {
                        1 => directive(ctx, c.args.pop().unwrap()),
                        2 => {
                            let mut i = c.args.into_iter();
                            assert_clause(ctx, i.next().unwrap(), i.next().unwrap())
                        }
                        _ => assert_fact(
                            ctx,
                            Term {
                                kind: TermKind::Compound(c),
                                location: t.location,
                            },
                        ),
                    },
                    _ => assert_fact(ctx, t),
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
    mut c: Compound,
    d: fn(&mut ConsultContext, Term) -> Result<(), Box<Error>>,
) -> Result<(), Box<Error>> {
    if c.args.len() == 1 {
        // PI or PI_list
        let mut pi = c.args.pop().unwrap();
        match pi.kind {
            TermKind::Compound(c) if c.functor == "." && c.args.len() == 2 => {
                let mut i = c.args.into_iter();
                loop {
                    (d)(ctx, i.next().unwrap())?;
                    pi = i.next().unwrap();

                    match pi.kind {
                        TermKind::Compound(c) if c.functor == "." && c.args.len() == 2 => {
                            i = c.args.into_iter();
                        }
                        TermKind::Atom(s) if s == "[]" => break Ok(()),
                        _ => break (d)(ctx, pi),
                    }
                }
            }
            _ => (d)(ctx, pi),
        }
    } else {
        for arg in c.args {
            (d)(ctx, arg)?;
        }
        Ok(())
    }
}

fn directive(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    // Clear current procedure
    ctx.current_procedure = None;

    match term.kind {
        TermKind::Compound(c) if c.functor == "op" && c.args.len() == 3 => {
            let mut i = c.args.into_iter();
            op(ctx, i.next().unwrap(), i.next().unwrap(), i.next().unwrap())
        }
        TermKind::Compound(mut c) if c.functor == "initialization" && c.args.len() == 1 => {
            initialization(ctx, c.args.pop().unwrap())
        }
        TermKind::Compound(c) if c.functor == "set_prolog_flag" && c.args.len() == 2 => {
            let mut i = c.args.into_iter();
            prolog_flag(ctx, i.next().unwrap(), i.next().unwrap())
        }
        TermKind::Compound(c) if c.functor == "char_conversion" && c.args.len() == 2 => {
            let mut i = c.args.into_iter();
            char_conversion(ctx, i.next().unwrap(), i.next().unwrap())
        }
        TermKind::Compound(c) if c.functor == "include" => directive_expand(ctx, c, include),
        TermKind::Compound(c) if c.functor == "ensure_loaded" => {
            directive_expand(ctx, c, ensure_loaded)
        }
        TermKind::Compound(c) if c.functor == "public" => directive_expand(ctx, c, public),
        TermKind::Compound(c) if c.functor == "dynamic" => directive_expand(ctx, c, dynamic),
        TermKind::Compound(c) if c.functor == "multifile" => directive_expand(ctx, c, multifile),
        TermKind::Compound(c) if c.functor == "discontiguous" => {
            directive_expand(ctx, c, discontiguous)
        }
        _ => Error::new(term.location.clone(), ErrorKind::UnknownDirective(term)),
    }
}

fn include(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    match &term.kind {
        TermKind::Atom(s) => {
            let (_, stream) = ctx.resolver.open(s)?;
            ctx.stream.include(stream)
        }
        _ => Error::new(term.location.clone(), ErrorKind::BadStreamName(term)),
    }
}

fn ensure_loaded(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    match &term.kind {
        TermKind::Atom(s) => {
            let (full_path, stream) = ctx.resolver.open(s)?;
            for s in ctx.loaded_set.iter() {
                if *s == full_path {
                    return Ok(());
                }
            }
            ctx.loaded_set.push(full_path.clone());

            // New context and stream
            let prev_ctx = std::mem::take(&mut ctx.context);
            let prev_stream = std::mem::replace(&mut ctx.stream, MultiReader::new(stream));
            let prev_text = std::mem::replace(&mut ctx.current_text, full_path);

            load_text(ctx)?;

            // Restore the previous context
            ctx.context = prev_ctx;
            ctx.stream = prev_stream;
            ctx.current_text = prev_text;
            Ok(())
        }
        _ => Error::new(term.location.clone(), ErrorKind::BadStreamName(term)),
    }
}

fn update_op(
    ctx: &mut ConsultContext,
    specifier: &Operator,
    operator: Term,
    remove: bool,
) -> Result<(), Box<Error>> {
    match operator.kind {
        TermKind::Atom(ref s) if s == "," => Error::new(
            operator.location.clone(),
            ErrorKind::InvalidOperator(operator),
        ),
        TermKind::Atom(s) if remove => {
            ctx.context.operators.remove(&s);
            Ok(())
        }
        TermKind::Atom(ref s) => {
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
                                return Error::new(
                                    operator.location.clone(),
                                    ErrorKind::InvalidOpCombo(
                                        operator,
                                        o.clone(),
                                        specifier.clone(),
                                    ),
                                )
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
                                return Error::new(
                                    operator.location.clone(),
                                    ErrorKind::InvalidOpCombo(
                                        operator,
                                        o.clone(),
                                        specifier.clone(),
                                    ),
                                )
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
                                return Error::new(
                                    operator.location.clone(),
                                    ErrorKind::InvalidOpCombo(
                                        operator,
                                        o.clone(),
                                        specifier.clone(),
                                    ),
                                )
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
                                return Error::new(
                                    operator.location.clone(),
                                    ErrorKind::InvalidOpCombo(
                                        operator,
                                        o.clone(),
                                        specifier.clone(),
                                    ),
                                )
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
                                return Error::new(
                                    operator.location.clone(),
                                    ErrorKind::InvalidOpCombo(
                                        operator,
                                        o.clone(),
                                        specifier.clone(),
                                    ),
                                )
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
        _ => Error::new(
            operator.location.clone(),
            ErrorKind::InvalidOperator(operator),
        ),
    }
}

fn op(
    ctx: &mut ConsultContext,
    priority: Term,
    specifier: Term,
    mut operator: Term,
) -> Result<(), Box<Error>> {
    // Unpack specifier
    let (op_spec, remove) = match specifier.kind {
        TermKind::Atom(ref s) => {
            // Unpack priority
            let p = match priority.kind {
                TermKind::Integer(n) => match n {
                    0..=1200 => n as u16,
                    _ => {
                        return Error::new(
                            priority.location.clone(),
                            ErrorKind::InvalidOpPriority(priority),
                        )
                    }
                },
                _ => {
                    return Error::new(
                        priority.location.clone(),
                        ErrorKind::InvalidOpPriority(priority),
                    )
                }
            };

            match s.as_str() {
                "fx" => (Operator::fx(p), p == 0),
                "fy" => (Operator::fy(p), p == 0),
                "xfx" => (Operator::xfx(p), p == 0),
                "xfy" => (Operator::xfy(p), p == 0),
                "yfx" => (Operator::yfx(p), p == 0),
                "xf" => (Operator::xf(p), p == 0),
                "yf" => (Operator::yf(p), p == 0),
                _ => {
                    return Error::new(
                        specifier.location.clone(),
                        ErrorKind::InvalidOpSpecifier(specifier),
                    )
                }
            }
        }
        _ => {
            return Error::new(
                specifier.location.clone(),
                ErrorKind::InvalidOpSpecifier(specifier),
            )
        }
    };

    // Iterate atom_or_atom_list
    match operator.kind {
        TermKind::Compound(c) if c.functor == "." && c.args.len() == 2 => {
            let mut i = c.args.into_iter();
            loop {
                update_op(ctx, &op_spec, i.next().unwrap(), remove)?;

                operator = i.next().unwrap();
                match operator.kind {
                    TermKind::Compound(c) if c.functor == "." && c.args.len() == 2 => {
                        i = c.args.into_iter();
                    }
                    TermKind::Atom(s) if s == "[]" => break Ok(()),
                    _ => break update_op(ctx, &op_spec, operator, remove),
                }
            }
        }
        _ => update_op(ctx, &op_spec, operator, remove),
    }
}

fn bool_flag(flag: Term, value: Term) -> Result<bool, Box<Error>> {
    match value.kind {
        TermKind::Atom(ref s) => match s.as_str() {
            "on" => Ok(true),
            "off" => Ok(false),
            _ => Error::new(
                value.location.clone(),
                ErrorKind::InvalidFlagValue(flag, value),
            ),
        },
        _ => Error::new(
            value.location.clone(),
            ErrorKind::InvalidFlagValue(flag, value),
        ),
    }
}

fn quote_flag(flag: Term, value: Term) -> Result<QuoteFlag, Box<Error>> {
    match value.kind {
        TermKind::Atom(ref s) => match s.as_str() {
            "chars" => Ok(QuoteFlag::Chars),
            "codes" => Ok(QuoteFlag::Codes),
            "atom" => Ok(QuoteFlag::Atom),
            _ => Error::new(
                value.location.clone(),
                ErrorKind::InvalidFlagValue(flag, value),
            ),
        },
        _ => Error::new(
            value.location.clone(),
            ErrorKind::InvalidFlagValue(flag, value),
        ),
    }
}

fn prolog_flag(ctx: &mut ConsultContext, flag: Term, value: Term) -> Result<(), Box<Error>> {
    match flag.kind {
        TermKind::Atom(ref s) => match s.as_str() {
            "char_conversion" => ctx.context.flags.char_conversion = bool_flag(flag, value)?,
            "debug" => ctx.context.flags.debug = bool_flag(flag, value)?,
            "unknown" => match &value.kind {
                TermKind::Atom(s) => match s.as_str() {
                    "error" => ctx.context.flags.unknown = UnknownFlag::Error,
                    "fail" => ctx.context.flags.unknown = UnknownFlag::Fail,
                    "warning" => ctx.context.flags.unknown = UnknownFlag::Warning,
                    _ => {
                        return Error::new(
                            value.location.clone(),
                            ErrorKind::InvalidFlagValue(flag, value),
                        )
                    }
                },
                _ => {
                    return Error::new(
                        value.location.clone(),
                        ErrorKind::InvalidFlagValue(flag, value),
                    )
                }
            },
            "double_quotes" => ctx.context.flags.double_quotes = quote_flag(flag, value)?,
            "back_quotes" => ctx.context.flags.back_quotes = quote_flag(flag, value)?,
            _ => return Error::new(flag.location.clone(), ErrorKind::InvalidFlag(flag)),
        },
        _ => return Error::new(flag.location.clone(), ErrorKind::InvalidFlag(flag)),
    }
    Ok(())
}

fn char_conversion(
    ctx: &mut ConsultContext,
    in_char: Term,
    out_char: Term,
) -> Result<(), Box<Error>> {
    match &in_char.kind {
        TermKind::Atom(s) if s.len() == 1 => {
            let in_char = s.chars().next().unwrap();
            match &out_char.kind {
                TermKind::Atom(s) if s.len() == 1 => {
                    let out_char = s.chars().next().unwrap();
                    if in_char == out_char {
                        ctx.context.char_conversion.remove(&in_char);
                    } else {
                        ctx.context.char_conversion.insert(in_char, out_char);
                    }
                    Ok(())
                }
                _ => Error::new(
                    out_char.location.clone(),
                    ErrorKind::InvalidCharacter(out_char),
                ),
            }
        }
        _ => Error::new(
            in_char.location.clone(),
            ErrorKind::InvalidCharacter(in_char),
        ),
    }
}

fn initialization(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    Ok(())
}

fn lookup_procedure<'a>(
    ctx: &'a mut ConsultContext,
    term: Term,
) -> Result<&'a mut Procedure, Box<Error>> {
    match &term.kind {
        TermKind::Compound(c) if c.functor == "/" && c.args.len() == 2 => {
            if let TermKind::Atom(ref s) = c.args[0].kind {
                match c.args[1].kind {
                    TermKind::Integer(n) if n >= 0 => {
                        let pi = format!("{}/{}", s, n);
                        return Ok(ctx
                            .program
                            .procedures
                            .entry(pi)
                            .or_insert_with(|| Procedure {
                                source_text: ctx.current_text.clone(),
                                ..Default::default()
                            }));
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }
    Error::new(
        term.location.clone(),
        ErrorKind::InvalidPredicateIndicator(term),
    )
}

fn public(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    lookup_procedure(ctx, term)?.flags.public = true;
    Ok(())
}

fn dynamic(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    lookup_procedure(ctx, term)?.flags.dynamic = true;
    Ok(())
}

fn multifile(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    lookup_procedure(ctx, term)?.flags.multifile = true;
    Ok(())
}

fn discontiguous(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    lookup_procedure(ctx, term)?.flags.discontiguous = true;
    Ok(())
}

pub(super) fn consult(
    resolver: &mut dyn StreamResolver,
    source: &str,
    sink: Option<ErrorSinkFn>,
) -> Result<Program, Box<Error>> {
    let mut program = Program::new();
    load_text(&mut ConsultContext::new(
        resolver,
        source,
        &mut program,
        sink,
        &mut Vec::new(),
    )?)?;

    Ok(program)
}
