use std::collections::HashMap;
use std::rc::Rc;

use super::*;
use error::*;
use multireader::*;

use flags::{QuoteFlag, UnknownFlag};
use operators::Operator;

use read_term::parser;
use read_term::term::{Compound, Term, TermKind, VarInfo};

#[derive(Default, Debug)]
pub(crate) struct Flags {
    pub dynamic: bool,
    multifile: bool,
    discontiguous: bool,
}

#[derive(Debug)]
pub(crate) struct Clause {
    pub head: Rc<Term>,
    pub body: Option<Rc<Term>>,
}

#[derive(Default, Debug)]
pub(crate) struct Procedure {
    pub flags: Flags,
    pub predicates: Vec<Rc<Clause>>,
    pub source_text: Option<String>,
}

#[derive(Default)]
pub(crate) struct Text {
    pub procedures: HashMap<String, Procedure>,
    pub initialization: Vec<(Rc<Term>, Vec<VarInfo>)>,
    pub flags: flags::Flags,
    pub operators: operators::OperatorTable,
    pub char_conversion: HashMap<char, char>,
}

struct Context<'a> {
    stream: MultiReader,
    sink: ErrorSinkFn,
    directives: &'a mut HashMap<String, Procedure>,
    resolver: &'a mut dyn StreamResolver,
    loaded_set: &'a mut Vec<String>,
    current_procedure: Option<String>,
    current_text: String,
    failed: bool,
}

type Directive = fn(&mut Text, &mut Context, &Rc<Term>) -> Result<(), Box<Error>>;

fn bool_flag(flag: &Rc<Term>, value: &Rc<Term>) -> Result<bool, Box<Error>> {
    match &value.kind {
        TermKind::Atom(s) => match s.as_str() {
            "on" => Ok(true),
            "off" => Ok(false),
            _ => Error::new(Error::InvalidFlagValue(flag.clone(), value.clone())),
        },
        _ => Error::new(Error::InvalidFlagValue(flag.clone(), value.clone())),
    }
}

fn quote_flag(flag: &Rc<Term>, value: &Rc<Term>) -> Result<QuoteFlag, Box<Error>> {
    match &value.kind {
        TermKind::Atom(s) => match s.as_str() {
            "chars" => Ok(QuoteFlag::Chars),
            "codes" => Ok(QuoteFlag::Codes),
            "atom" => Ok(QuoteFlag::Atom),
            _ => Error::new(Error::InvalidFlagValue(flag.clone(), value.clone())),
        },
        _ => Error::new(Error::InvalidFlagValue(flag.clone(), value.clone())),
    }
}

fn pi_from_term(term: &Rc<Term>) -> Result<String, Box<Error>> {
    match &term.kind {
        TermKind::Compound(c) if c.functor == "/" && c.args.len() == 2 => {
            if let TermKind::Atom(s) = &c.args[0].kind {
                match c.args[1].kind {
                    TermKind::Integer(n) if n >= 0 => return Ok(format!("{}/{}", s, n)),
                    _ => {}
                }
            }
        }
        _ => {}
    }
    Error::new(Error::InvalidPredicateIndicator(term.clone()))
}

impl Text {
    pub fn new() -> Self {
        Self {
            procedures: HashMap::new(),
            initialization: Vec::new(),
            flags: flags::Flags::default(),
            operators: operators::Operator::default_table(),
            char_conversion: HashMap::new(),
        }
    }

    pub fn load(
        &mut self,
        resolver: &mut dyn StreamResolver,
        source: &str,
        sink: Option<ErrorSinkFn>,
    ) -> Result<bool, Box<Error>> {
        resolver
            .open(source)
            .map_err(|e| {
                Box::new(Error::StreamResolver(
                    Term::new_atom(source.to_string(), None),
                    e,
                ))
            })
            .map(|(current_text, stream)| {
                let loaded_set = &mut Vec::new();
                let directives = &mut HashMap::new();

                self.load_text(Context {
                    stream: MultiReader::new(stream),
                    sink: sink.unwrap_or(|_| false),
                    directives,
                    resolver,
                    loaded_set,
                    current_procedure: None,
                    current_text,
                    failed: false,
                })
            })?
    }

    fn load_text(&mut self, mut ctx: Context) -> Result<bool, Box<Error>> {
        loop {
            let mut var_info = Vec::new();
            match parser::next(
                read_term::Context {
                    flags: &self.flags,
                    operators: &self.operators,
                    char_conversion: &self.char_conversion,
                    greedy: true,
                },
                &mut var_info,
                &mut ctx.stream,
            )
            .map_err(|e| Error::ReadTerm(*e))
            {
                Ok(None) => break Ok(!ctx.failed),
                Ok(Some(t)) => {
                    let r = match &t.kind {
                        TermKind::Compound(c) if c.functor == ":-" && c.args.len() == 1 => {
                            self.directive(&mut ctx, var_info, &c.args[0])
                        }
                        TermKind::Compound(c) if c.functor == ":-" && c.args.len() == 2 => {
                            self.assert(&mut ctx, &c.args[0], Some(&c.args[1]))
                        }
                        _ => self.assert(&mut ctx, &t, None),
                    };

                    if let Err(e) = r {
                        ctx.failed = true;
                        if !(ctx.sink)(&e) {
                            break Err(e);
                        }
                    }
                }
                Err(e) => {
                    ctx.failed = true;
                    if (ctx.sink)(&e) {
                        if let Err(e) = parser::skip_to_end(
                            read_term::Context {
                                flags: &self.flags,
                                operators: &self.operators,
                                char_conversion: &self.char_conversion,
                                greedy: true,
                            },
                            &mut ctx.stream,
                        )
                        .map_err(|e| Error::ReadTerm(*e))
                        {
                            if !(ctx.sink)(&e) {
                                break Err(Box::new(e));
                            }
                        }
                    } else {
                        break Err(Box::new(e));
                    }
                }
            }
        }
    }

    fn assert(
        &mut self,
        ctx: &mut Context,
        head: &Rc<Term>,
        body: Option<&Rc<Term>>,
    ) -> Result<(), Box<Error>> {
        let pi = match &head.kind {
            TermKind::Atom(s) => format!("{}/0", s),
            TermKind::Compound(c) => format!("{}/{}", c.functor, c.args.len()),
            _ => return Error::new(Error::InvalidHead(head.clone())),
        };
        if builtins::is_builtin(&pi) {
            return Error::new(Error::AlterBuiltin(head.clone()));
        }

        let p = if !self.flags.strict_iso {
            ctx.directives
                .get_mut(&pi)
                .or_else(|| self.procedures.get_mut(&pi))
        } else {
            self.procedures.get_mut(&pi)
        };

        if let Some(p) = p {
            if let Some(first) = p.predicates.first() {
                if !p.flags.multifile {
                    if let Some(s) = &p.source_text {
                        if ctx.current_text != *s {
                            return Error::new(Error::NotMultifile(
                                head.clone(),
                                first.head.location.clone(),
                                s.clone(),
                            ));
                        }
                    }
                }

                if !p.flags.discontiguous
                    && ctx
                        .current_procedure
                        .as_ref()
                        .filter(|s| **s != pi)
                        .is_some()
                {
                    return Error::new(Error::NotDiscontiguous(
                        head.clone(),
                        first.head.location.clone(),
                    ));
                }
            }
            p.predicates.push(Rc::new(Clause {
                head: head.clone(),
                body: body.cloned(),
            }));
        } else {
            self.procedures.insert(
                pi.clone(),
                Procedure {
                    source_text: Some(ctx.current_text.clone()),
                    predicates: vec![Rc::new(Clause {
                        head: head.clone(),
                        body: body.cloned(),
                    })],
                    ..Procedure::default()
                },
            );
        }
        ctx.current_procedure = Some(pi);
        Ok(())
    }

    fn directive_list_expand(
        &mut self,
        ctx: &mut Context,
        c: &Compound,
        d: Directive,
    ) -> Result<(), Box<Error>> {
        (d)(self, ctx, &c.args[0])?;

        match &c.args[1].kind {
            TermKind::Compound(c) if c.functor == "." && c.args.len() == 2 => {
                self.directive_list_expand(ctx, c, d)
            }
            TermKind::Atom(s) if s == "[]" => Ok(()),
            _ => (d)(self, ctx, &c.args[1]),
        }
    }

    fn directive_expand(
        &mut self,
        ctx: &mut Context,
        c: &Compound,
        d: Directive,
    ) -> Result<(), Box<Error>> {
        if c.args.len() == 1 {
            // PI or PI_list
            match &c.args[0].kind {
                TermKind::Compound(c) if c.functor == "." && c.args.len() == 2 => {
                    self.directive_list_expand(ctx, c, d)
                }
                _ => (d)(self, ctx, &c.args[0]),
            }
        } else {
            for a in c.args.iter() {
                (d)(self, ctx, a)?;
            }
            Ok(())
        }
    }

    fn directive(
        &mut self,
        ctx: &mut Context,
        var_info: Vec<VarInfo>,
        term: &Rc<Term>,
    ) -> Result<(), Box<Error>> {
        // Clear current procedure
        ctx.current_procedure = None;

        match &term.kind {
            TermKind::Compound(c) if c.functor == "op" && c.args.len() == 3 => {
                self.op(ctx, &c.args[0], &c.args[1], &c.args[2])
            }
            TermKind::Compound(c) if c.functor == "initialization" && c.args.len() == 1 => {
                self.initialization(var_info, &c.args[0])
            }
            TermKind::Compound(c) if c.functor == "set_prolog_flag" && c.args.len() == 2 => {
                self.prolog_flag(ctx, &c.args[0], &c.args[1])
            }
            TermKind::Compound(c) if c.functor == "char_conversion" && c.args.len() == 2 => {
                self.char_conversion(ctx, &c.args[0], &c.args[1])
            }
            TermKind::Compound(c) if c.functor == "include" => {
                self.directive_expand(ctx, c, Self::include)
            }
            TermKind::Compound(c) if c.functor == "ensure_loaded" => {
                self.directive_expand(ctx, c, Self::ensure_loaded)
            }
            TermKind::Compound(c) if c.functor == "dynamic" => {
                self.directive_expand(ctx, c, Self::dynamic)
            }
            TermKind::Compound(c) if c.functor == "multifile" => {
                self.directive_expand(ctx, c, Self::multifile)
            }
            TermKind::Compound(c) if c.functor == "discontiguous" => {
                self.directive_expand(ctx, c, Self::discontiguous)
            }
            TermKind::Compound(c) if !self.flags.strict_iso => {
                if c.functor == "directive" {
                    self.directive_expand(ctx, c, Self::declare_directive)
                } else if ctx
                    .directives
                    .get(&format!("{}/{}", c.functor, c.args.len()))
                    .is_some()
                {
                    self.directive_eval(ctx, &c.args[0])
                } else {
                    Error::new(Error::UnknownDirective(c.args[0].clone()))
                }
            }
            _ => Error::new(Error::UnknownDirective(term.clone())),
        }
    }

    fn directive_eval(&mut self, ctx: &mut Context, term: &Rc<Term>) -> Result<(), Box<Error>> {
        todo!()
    }

    fn include(&mut self, ctx: &mut Context, term: &Rc<Term>) -> Result<(), Box<Error>> {
        match &term.kind {
            TermKind::Atom(s) => ctx
                .resolver
                .open(s)
                .map_err(|e| Box::new(Error::StreamResolver(term.clone(), e)))
                .and_then(|(_, stream)| ctx.stream.include(stream)),
            _ => Error::new(Error::BadStreamName(term.clone())),
        }
    }

    fn ensure_loaded(&mut self, ctx: &mut Context, term: &Rc<Term>) -> Result<(), Box<Error>> {
        match &term.kind {
            TermKind::Atom(s) => ctx
                .resolver
                .open(s)
                .map_err(|e| Box::new(Error::StreamResolver(term.clone(), e)))
                .and_then(|(full_path, stream)| {
                    if !ctx.loaded_set.iter().any(|s| *s == full_path) {
                        ctx.loaded_set.push(full_path.clone());

                        // New context and stream
                        ctx.failed = !self.load_text(Context {
                            stream: MultiReader::new(stream),
                            sink: ctx.sink,
                            directives: ctx.directives,
                            resolver: ctx.resolver,
                            loaded_set: ctx.loaded_set,
                            current_procedure: ctx.current_procedure.clone(),
                            current_text: full_path,
                            failed: ctx.failed,
                        })?;
                    }
                    Ok(())
                }),
            _ => Error::new(Error::BadStreamName(term.clone())),
        }
    }

    fn update_op(
        &mut self,
        ctx: &mut Context,
        specifier: &Operator,
        operator: &Rc<Term>,
        remove: bool,
    ) -> Result<(), Box<Error>> {
        match &operator.kind {
            TermKind::Atom(s) if s == "," => Error::new(Error::InvalidOperator(operator.clone())),
            TermKind::Atom(s) if remove => {
                self.operators.remove(s);
                Ok(())
            }
            TermKind::Atom(s) => {
                if let Some(ops) = self.operators.get_mut(s) {
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
                                    return Error::new(Error::InvalidOpCombo(
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
                                    return Error::new(Error::InvalidOpCombo(
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
                                    return Error::new(Error::InvalidOpCombo(
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
                                    return Error::new(Error::InvalidOpCombo(
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
                    self.operators.insert(s.clone(), vec![specifier.clone()]);
                }
                Ok(())
            }
            _ => Error::new(Error::InvalidOperator(operator.clone())),
        }
    }

    fn op_list_expand(
        &mut self,
        ctx: &mut Context,
        specifier: &Operator,
        c: &Compound,
        remove: bool,
    ) -> Result<(), Box<Error>> {
        self.update_op(ctx, specifier, &c.args[0], remove)?;

        match &c.args[1].kind {
            TermKind::Compound(c) if c.functor == "." && c.args.len() == 2 => {
                self.op_list_expand(ctx, specifier, c, remove)
            }
            TermKind::Atom(s) if s == "[]" => Ok(()),
            _ => self.update_op(ctx, specifier, &c.args[1], remove),
        }
    }

    fn op(
        &mut self,
        ctx: &mut Context,
        priority: &Rc<Term>,
        specifier: &Rc<Term>,
        operator: &Rc<Term>,
    ) -> Result<(), Box<Error>> {
        // Unpack specifier
        let (op_spec, remove) = match &specifier.kind {
            TermKind::Atom(s) => {
                // Unpack priority
                let p = match priority.kind {
                    TermKind::Integer(n) => match n {
                        0..=1200 => n as u16,
                        _ => return Error::new(Error::InvalidOpPriority(priority.clone())),
                    },
                    _ => return Error::new(Error::InvalidOpPriority(priority.clone())),
                };

                match s.as_str() {
                    "fx" => (Operator::fx(p), p == 0),
                    "fy" => (Operator::fy(p), p == 0),
                    "xfx" => (Operator::xfx(p), p == 0),
                    "xfy" => (Operator::xfy(p), p == 0),
                    "yfx" => (Operator::yfx(p), p == 0),
                    "xf" => (Operator::xf(p), p == 0),
                    "yf" => (Operator::yf(p), p == 0),
                    _ => return Error::new(Error::InvalidOpSpecifier(specifier.clone())),
                }
            }
            _ => return Error::new(Error::InvalidOpSpecifier(specifier.clone())),
        };

        // Iterate atom_or_atom_list
        match &operator.kind {
            TermKind::Compound(c) if c.functor == "." && c.args.len() == 2 => {
                self.op_list_expand(ctx, &op_spec, c, remove)
            }
            _ => self.update_op(ctx, &op_spec, operator, remove),
        }
    }

    fn prolog_flag(
        &mut self,
        ctx: &mut Context,
        flag: &Rc<Term>,
        value: &Rc<Term>,
    ) -> Result<(), Box<Error>> {
        match &flag.kind {
            TermKind::Atom(s) => match s.as_str() {
                "char_conversion" => self.flags.char_conversion = bool_flag(flag, value)?,
                "debug" => self.flags.debug = bool_flag(flag, value)?,
                "strict_iso" if !self.flags.strict_iso => {
                    self.flags.strict_iso = bool_flag(flag, value)?
                }
                "unknown" => match &value.kind {
                    TermKind::Atom(s) => match s.as_str() {
                        "error" => self.flags.unknown = UnknownFlag::Error,
                        "fail" => self.flags.unknown = UnknownFlag::Fail,
                        "warning" => self.flags.unknown = UnknownFlag::Warning,
                        _ => {
                            return Error::new(Error::InvalidFlagValue(flag.clone(), value.clone()))
                        }
                    },
                    _ => return Error::new(Error::InvalidFlagValue(flag.clone(), value.clone())),
                },
                "double_quotes" => self.flags.double_quotes = quote_flag(flag, value)?,
                "back_quotes" if !self.flags.strict_iso => {
                    self.flags.back_quotes = quote_flag(flag, value)?
                }
                _ => return Error::new(Error::InvalidFlag(flag.clone())),
            },
            _ => return Error::new(Error::InvalidFlag(flag.clone())),
        }
        Ok(())
    }

    fn char_conversion(
        &mut self,
        ctx: &mut Context,
        in_char: &Rc<Term>,
        out_char: &Rc<Term>,
    ) -> Result<(), Box<Error>> {
        match &in_char.kind {
            TermKind::Atom(s) if s.len() == 1 => {
                let in_char = s.chars().next().unwrap();
                match &out_char.kind {
                    TermKind::Atom(s) if s.len() == 1 => {
                        let out_char = s.chars().next().unwrap();
                        if in_char == out_char {
                            self.char_conversion.remove(&in_char);
                        } else {
                            self.char_conversion.insert(in_char, out_char);
                        }
                        Ok(())
                    }
                    _ => Error::new(Error::InvalidCharacter(out_char.clone())),
                }
            }
            _ => Error::new(Error::InvalidCharacter(in_char.clone())),
        }
    }

    fn initialization(
        &mut self,
        var_info: Vec<VarInfo>,
        term: &Rc<Term>,
    ) -> Result<(), Box<Error>> {
        self.initialization.push((term.clone(), var_info));
        Ok(())
    }

    fn lookup_procedure<'a>(
        &'a mut self,
        ctx: &'a mut Context,
        term: &Rc<Term>,
    ) -> Result<(&'a mut Procedure, bool), Box<Error>> {
        let pi = pi_from_term(term)?;
        if builtins::is_builtin(&pi) {
            return Error::new(Error::AlterBuiltin(term.clone()));
        }

        if !self.flags.strict_iso {
            if let Some(p) = ctx.directives.get_mut(&pi) {
                return Ok((p, true));
            }
        }

        Ok((
            self.procedures.entry(pi).or_insert_with(|| Procedure {
                source_text: Some(ctx.current_text.clone()),
                ..Default::default()
            }),
            false,
        ))
    }

    fn dynamic(&mut self, ctx: &mut Context, term: &Rc<Term>) -> Result<(), Box<Error>> {
        let (p, _) = self.lookup_procedure(ctx, term)?;
        if !p.flags.dynamic && !p.predicates.is_empty() {
            Error::new(Error::AlreadyNotDynamic(
                term.clone(),
                p.predicates.first().unwrap().head.location.clone(),
            ))
        } else {
            p.flags.dynamic = true;
            Ok(())
        }
    }

    fn multifile(&mut self, ctx: &mut Context, term: &Rc<Term>) -> Result<(), Box<Error>> {
        let (p, directive) = self.lookup_procedure(ctx, term)?;
        if (!p.flags.multifile && !p.predicates.is_empty()) || directive {
            Error::new(Error::AlreadyNotMultifile(
                term.clone(),
                p.predicates.first().unwrap().head.location.clone(),
            ))
        } else {
            p.flags.multifile = true;
            Ok(())
        }
    }

    fn discontiguous(&mut self, ctx: &mut Context, term: &Rc<Term>) -> Result<(), Box<Error>> {
        let (p, directive) = self.lookup_procedure(ctx, term)?;
        if (!p.flags.discontiguous && !p.predicates.is_empty()) || directive {
            Error::new(Error::AlreadyNotDiscontiguous(
                term.clone(),
                p.predicates.first().unwrap().head.location.clone(),
            ))
        } else {
            p.flags.discontiguous = true;
            Ok(())
        }
    }

    fn declare_directive(&mut self, ctx: &mut Context, term: &Rc<Term>) -> Result<(), Box<Error>> {
        let pi = pi_from_term(term)?;
        if let Some(p) = self.procedures.get(&pi) {
            return Error::new(Error::AlreadyNotDirective(
                term.clone(),
                p.predicates.first().unwrap().head.location.clone(),
            ));
        }

        if builtins::is_builtin(&pi) {
            return Error::new(Error::AlterBuiltin(term.clone()));
        }

        if pi == "initialization/1"
            || pi.starts_with("include/")
            || pi.starts_with("ensure_loaded/")
            || pi.starts_with("dynamic/")
            || pi.starts_with("multifile/")
            || pi.starts_with("discontiguous/")
            || pi.starts_with("directive/")
        {
            return Error::new(Error::AlterBuiltin(term.clone()));
        }

        let p = ctx.directives.entry(pi).or_insert_with(|| Procedure {
            source_text: Some(ctx.current_text.clone()),
            ..Procedure::default()
        });
        p.flags.dynamic = true;
        Ok(())
    }
}

pub(super) fn consult(
    resolver: &mut dyn StreamResolver,
    source: &str,
    sink: Option<ErrorSinkFn>,
) -> Result<Option<Text>, Box<Error>> {
    let mut text = Text::new();
    if text.load(resolver, source, sink)? {
        Ok(Some(text))
    } else {
        Ok(None)
    }
}
