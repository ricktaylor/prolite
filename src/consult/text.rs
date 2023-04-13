use std::collections::HashMap;

use super::*;
use error::*;
use multireader::*;

use flags::{QuoteFlag, UnknownFlag};
use operators::Operator;

use read_term::parser;
use read_term::term::{Compound, Term, TermKind};
use read_term::Context;

#[derive(Default, Debug, Clone)]
pub(super) struct Flags {
    pub public: bool,
    pub dynamic: bool,
    pub multifile: bool,
    pub discontiguous: bool,
}

#[derive(Default, Debug)]
pub(super) struct Procedure {
    pub flags: Flags,
    pub predicates: Vec<Compound>,
    pub source_text: String,
}

pub(super) struct Text {
    pub procedures: HashMap<String, Procedure>,
}

impl Text {
    pub(super) fn new() -> Self {
        Text {
            procedures: HashMap::new(),
        }
    }
}

struct ConsultContext<'a> {
    context: Context,
    stream: MultiReader,
    sink: ErrorSinkFn,
    text: &'a mut Text,
    resolver: &'a mut dyn StreamResolver,
    loaded_set: Vec<String>,
    current_procedure: Option<String>,
    current_text: String,
    failed: bool,
}

impl<'a> ConsultContext<'a> {
    fn null_sink(_: &Error) -> bool {
        false
    }

    fn new(
        resolver: &'a mut dyn StreamResolver,
        source: &str,
        text: &'a mut Text,
        error_sink: Option<ErrorSinkFn>,
    ) -> Result<Self, Box<Error>> {
        match resolver.open(source) {
            Err(e) => Error::new(Error::StreamResolver(
                Term::new_atom(source.to_string(), Default::default()),
                e,
            )),
            Ok((full_name, stream)) => Ok(Self {
                context: Default::default(),
                stream: MultiReader::new(stream),
                sink: match error_sink {
                    None => Self::null_sink,
                    Some(s) => s,
                },
                text,
                resolver,
                loaded_set: Vec::new(),
                current_procedure: None,
                current_text: full_name,
                failed: false,
            }),
        }
    }

    fn error<E>(&mut self, r: Result<(), Box<E>>) -> Result<(), Box<Error>>
    where
        text::Error: From<E>,
    {
        if let Err(e) = r {
            self.failed = true;
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

    fn load_text(&mut self) -> Result<(), Box<Error>> {
        loop {
            match parser::next(&self.context, &mut self.stream, true) {
                Ok(None) => break,
                Ok(Some(t)) => {
                    let r = match t.kind {
                        TermKind::Compound(mut c) if c.functor == ":-" && c.args.len() == 1 => {
                            directive(self, c.args.pop().unwrap())
                        }
                        TermKind::Compound(c) if c.functor == ":-" && c.args.len() == 2 => {
                            assert(self, c)
                        }
                        _ => {
                            let l = t.location.clone();
                            assert(
                                self,
                                Compound {
                                    functor: ":-".to_string(),
                                    args: vec![t, Term::new_atom("true".to_string(), l)],
                                },
                            )
                        }
                    };
                    self.error(r)?;
                }
                Err(e) => {
                    self.error(Err(e))?;
                    parser::skip_to_end(&self.context, &mut self.stream)?;
                }
            }
        }
        Ok(())
    }
}

fn is_builtin(pi: &str) -> bool {
    // TODO - check against builtins!

    false
}

fn assert_clause(proc: &mut Procedure, clause: Compound) -> Result<(), Box<Error>> {
    //todo!()

    Ok(())
}

fn assert(ctx: &mut ConsultContext, clause: Compound) -> Result<(), Box<Error>> {
    let pi = match &clause.args[0].kind {
        TermKind::Atom(s) => format!("{}/0", s),
        TermKind::Compound(c) => format!("{}/{}", c.functor, c.args.len()),
        _ => {
            return Error::new(Error::NotCallableTerm(
                clause.args.into_iter().next().unwrap(),
            ))
        }
    };
    if is_builtin(&pi) {
        todo!();
    }

    if let Some(p) = ctx.text.procedures.get_mut(&pi) {
        // Check discontiguous/multifile flags
        if !p.predicates.is_empty() {
            if !p.flags.multifile && ctx.current_text != p.source_text {
                return Error::new(Error::NotMultifile(
                    clause.args.into_iter().next().unwrap(),
                    p.predicates.first().unwrap().args[0].location.clone(),
                ));
            }

            if !p.flags.discontiguous {
                match &ctx.current_procedure {
                    Some(s) => {
                        if *s != pi {
                            return Error::new(Error::NotDiscontiguous(
                                clause.args.into_iter().next().unwrap(),
                                p.predicates.first().unwrap().args[0].location.clone(),
                            ));
                        }
                    }
                    None => {}
                }
            }
        }
        assert_clause(p, clause)?;
    } else {
        let mut p = Procedure {
            source_text: ctx.current_text.clone(),
            ..Default::default()
        };

        assert_clause(&mut p, clause)?;

        ctx.text.procedures.insert(pi.clone(), p);
    }
    ctx.current_procedure = Some(pi);
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
        _ => Error::new(Error::UnknownDirective(term)),
    }
}

fn include(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    match term.kind {
        TermKind::Atom(ref s) => match ctx.resolver.open(s) {
            Err(e) => Error::new(Error::StreamResolver(term, e)),
            Ok((_, stream)) => ctx.stream.include(stream),
        },
        _ => Error::new(Error::BadStreamName(term)),
    }
}

fn ensure_loaded(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    match term.kind {
        TermKind::Atom(ref s) => {
            match ctx.resolver.open(s) {
                Err(e) => Error::new(Error::StreamResolver(term, e)),
                Ok((full_path, stream)) => {
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

                    ctx.load_text()?;

                    // Restore the previous context
                    ctx.context = prev_ctx;
                    ctx.stream = prev_stream;
                    ctx.current_text = prev_text;
                    Ok(())
                }
            }
        }
        _ => Error::new(Error::BadStreamName(term)),
    }
}

fn update_op(
    ctx: &mut ConsultContext,
    specifier: &Operator,
    operator: Term,
    remove: bool,
) -> Result<(), Box<Error>> {
    match operator.kind {
        TermKind::Atom(ref s) if s == "," => Error::new(Error::InvalidOperator(operator)),
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
                                return Error::new(Error::InvalidOpCombo(
                                    operator,
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
                                return Error::new(Error::InvalidOpCombo(
                                    operator,
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
                                return Error::new(Error::InvalidOpCombo(
                                    operator,
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
                                return Error::new(Error::InvalidOpCombo(
                                    operator,
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
                                return Error::new(Error::InvalidOpCombo(
                                    operator,
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
        _ => Error::new(Error::InvalidOperator(operator)),
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
                    _ => return Error::new(Error::InvalidOpPriority(priority)),
                },
                _ => return Error::new(Error::InvalidOpPriority(priority)),
            };

            match s.as_str() {
                "fx" => (Operator::fx(p), p == 0),
                "fy" => (Operator::fy(p), p == 0),
                "xfx" => (Operator::xfx(p), p == 0),
                "xfy" => (Operator::xfy(p), p == 0),
                "yfx" => (Operator::yfx(p), p == 0),
                "xf" => (Operator::xf(p), p == 0),
                "yf" => (Operator::yf(p), p == 0),
                _ => return Error::new(Error::InvalidOpSpecifier(specifier)),
            }
        }
        _ => return Error::new(Error::InvalidOpSpecifier(specifier)),
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
            _ => Error::new(Error::InvalidFlagValue(flag, value)),
        },
        _ => Error::new(Error::InvalidFlagValue(flag, value)),
    }
}

fn quote_flag(flag: Term, value: Term) -> Result<QuoteFlag, Box<Error>> {
    match value.kind {
        TermKind::Atom(ref s) => match s.as_str() {
            "chars" => Ok(QuoteFlag::Chars),
            "codes" => Ok(QuoteFlag::Codes),
            "atom" => Ok(QuoteFlag::Atom),
            _ => Error::new(Error::InvalidFlagValue(flag, value)),
        },
        _ => Error::new(Error::InvalidFlagValue(flag, value)),
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
                    _ => return Error::new(Error::InvalidFlagValue(flag, value)),
                },
                _ => return Error::new(Error::InvalidFlagValue(flag, value)),
            },
            "double_quotes" => ctx.context.flags.double_quotes = quote_flag(flag, value)?,
            "back_quotes" => ctx.context.flags.back_quotes = quote_flag(flag, value)?,
            _ => return Error::new(Error::InvalidFlag(flag)),
        },
        _ => return Error::new(Error::InvalidFlag(flag)),
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
                _ => Error::new(Error::InvalidCharacter(out_char)),
            }
        }
        _ => Error::new(Error::InvalidCharacter(in_char)),
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
                        if is_builtin(&pi) {
                            todo!()
                        }

                        return Ok(ctx.text.procedures.entry(pi).or_insert_with(|| Procedure {
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
    Error::new(Error::InvalidPredicateIndicator(term))
}

fn public(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    lookup_procedure(ctx, term)?.flags.public = true;
    Ok(())
}

fn dynamic(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    let p = lookup_procedure(ctx, term)?;
    if !p.flags.dynamic && !p.predicates.is_empty() {
        // Error!!
        todo!()
    }
    p.flags.dynamic = true;
    Ok(())
}

fn multifile(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    let p = lookup_procedure(ctx, term)?;
    if !p.flags.multifile && !p.predicates.is_empty() {
        // Error!!
        todo!()
    }
    p.flags.multifile = true;
    Ok(())
}

fn discontiguous(ctx: &mut ConsultContext, term: Term) -> Result<(), Box<Error>> {
    let p = lookup_procedure(ctx, term)?;
    if !p.flags.discontiguous && !p.predicates.is_empty() {
        // Error!!
        todo!()
    }
    p.flags.discontiguous = true;
    Ok(())
}

pub(super) fn consult(
    resolver: &mut dyn StreamResolver,
    source: &str,
    sink: Option<ErrorSinkFn>,
) -> Result<Option<Text>, Box<Error>> {
    let mut text = Text::new();
    let mut ctx = ConsultContext::new(resolver, source, &mut text, sink)?;

    ctx.load_text()?;

    Ok(match ctx.failed {
        true => None,
        false => Some(text),
    })
}
