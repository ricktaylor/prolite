use std::rc::Rc;
use std::str::FromStr;

use super::*;
use error::*;
use lexer::*;
use stream::Span;
use term::*;

use operators::Operator;
use stream::ReadStream;

fn parse_integer(s: &str, radix: u32, location: Option<Span>) -> Result<Rc<Term>, Box<Error>> {
    match i64::from_str_radix(s, radix) {
        Ok(i) => Ok(Rc::new(Term {
            kind: TermKind::Integer(i),
            location,
        })),
        Err(e) => Error::new(ErrorKind::ParseIntError(e), location),
    }
}

fn parse_float(s: &str, location: Option<Span>) -> Result<Rc<Term>, Box<Error>> {
    match f64::from_str(s) {
        Ok(f) => Ok(Rc::new(Term {
            kind: TermKind::Float(f),
            location,
        })),
        Err(e) => Error::new(ErrorKind::ParseFloatError(e), location),
    }
}

fn parse_compound(
    ctx: &Context,
    var_info: &mut Vec<VarInfo>,
    stream: &mut dyn ReadStream,
    functor: String,
    location: Option<Span>,
) -> Result<(Rc<Term>, Token, u16), Box<Error>> {
    let mut args = Vec::new();
    let mut next = lexer::next(ctx, stream, false)?;
    loop {
        let term;
        (term, next, _) = parse_term(ctx, var_info, stream, next, 999)?;
        args.push(term);

        next = match next.kind {
            TokenKind::Comma => lexer::next(ctx, stream, false)?,
            TokenKind::Close => {
                return Ok((
                    Term::new_compound(functor, location, args),
                    lexer::next(ctx, stream, false)?,
                    0,
                ))
            }
            _ => return Error::new(ErrorKind::ExpectedChar(')'), next.location),
        }
    }
}

fn parse_list(
    ctx: &Context,
    var_info: &mut Vec<VarInfo>,
    stream: &mut dyn ReadStream,
    token: Token,
) -> Result<Rc<Term>, Box<Error>> {
    let mut terms = Vec::new();
    let mut next = token;
    loop {
        let term;
        (term, next, _) = parse_term(ctx, var_info, stream, next, 999)?;
        terms.push(term);

        next = match next.kind {
            TokenKind::Comma => lexer::next(ctx, stream, false)?,
            _ => break,
        }
    }

    let list;
    match next.kind {
        TokenKind::Bar => {
            next = lexer::next(ctx, stream, false)?;
            (list, next, _) = parse_term(ctx, var_info, stream, next, 999)?;
        }
        _ => list = Term::new_atom("[]".to_string(), next.location.clone()),
    };

    if !matches!(next.kind, TokenKind::CloseL) {
        return Error::new(ErrorKind::ExpectedChar(']'), next.location);
    }

    Ok(terms.iter().rev().fold(list, |list, t| {
        Term::new_compound(".".to_string(), t.location.clone(), vec![t.clone(), list])
    }))
}

fn parse_quoted(
    flags: &flags::QuoteFlag,
    s: String,
    location: Option<Span>,
) -> Result<Rc<Term>, Box<Error>> {
    match flags {
        flags::QuoteFlag::Atom => Ok(Term::new_atom(s, location)),
        flags::QuoteFlag::Chars => Ok(s.chars().rev().fold(
            Term::new_atom("[]".to_string(), location.clone()),
            |list, c| {
                Term::new_compound(
                    ".".to_string(),
                    location.clone(),
                    vec![Term::new_atom(c.to_string(), location.clone()), list],
                )
            },
        )),
        flags::QuoteFlag::Codes => Ok(s.chars().rev().fold(
            Term::new_atom("[]".to_string(), location.clone()),
            |list, c| {
                Term::new_compound(
                    ".".to_string(),
                    location.clone(),
                    vec![
                        Rc::new(Term {
                            kind: TermKind::Integer(c as i64),
                            location: location.clone(),
                        }),
                        list,
                    ],
                )
            },
        )),
    }
}

fn parse_variable(
    ctx: &Context,
    var_info: &mut Vec<VarInfo>,
    stream: &mut dyn ReadStream,
    name: String,
    location: Option<Span>,
) -> Result<(Rc<Term>, Token, u16), Box<Error>> {
    let p = if name != "_" {
        var_info
            .iter()
            .position(|v| !v.anon && v.name == name)
            .unwrap_or_else(|| {
                var_info.push(VarInfo {
                    name,
                    refcount: 0,
                    anon: false,
                });
                var_info.len() - 1
            })
    } else {
        let p = var_info.len();
        var_info.push(VarInfo {
            name: format!("$VAR({})", p),
            refcount: 0,
            anon: true,
        });
        p
    };

    var_info[p].refcount += 1;

    Ok((
        Rc::new(Term {
            kind: TermKind::Var(p),
            location,
        }),
        lexer::next(ctx, stream, false)?,
        0,
    ))
}

fn next_term(
    ctx: &Context,
    var_info: &mut Vec<VarInfo>,
    stream: &mut dyn ReadStream,
    token: Token,
    max_precedence: u16,
) -> Result<(Rc<Term>, Token, u16), Box<Error>> {
    match token.kind {
        /* 6.3.1.1 */
        TokenKind::Int(s, r) => {
            let next = lexer::next(ctx, stream, true)?;
            match next.kind {
                TokenKind::Exponent(t) => Ok((
                    parse_float(
                        &(s + &t),
                        lexer::location_join(token.location, next.location),
                    )?,
                    lexer::next(ctx, stream, false)?,
                    0,
                )),
                _ => Ok((parse_integer(&s, r, token.location)?, next, 0)),
            }
        }

        /* 6.3.2 */
        TokenKind::Var(s) => parse_variable(ctx, var_info, stream, s, token.location),

        /* ISO/IEC 13211-1:1995/Cor.2:2012 */
        TokenKind::Bar => {
            let next = lexer::next(ctx, stream, false)?;
            if let TokenKind::OpenCt = next.kind {
                parse_compound(ctx, var_info, stream, "|".to_string(), token.location)
            } else {
                Ok((Term::new_atom("|".to_string(), token.location), next, 0))
            }
        }

        TokenKind::Name(s) => {
            let next = lexer::next(ctx, stream, false)?;
            match next.kind {
                /* 6.3.1.2  */
                TokenKind::Int(t, r) if s == "-" => {
                    let next2 = lexer::next(ctx, stream, true)?;
                    match next2.kind {
                        TokenKind::Exponent(u) => Ok((
                            parse_float(
                                &(s + &t + &u),
                                lexer::location_join(token.location, next2.location),
                            )?,
                            lexer::next(ctx, stream, false)?,
                            0,
                        )),
                        _ => Ok((
                            parse_integer(
                                &(s + &t),
                                r,
                                lexer::location_join(token.location, next.location),
                            )?,
                            next2,
                            0,
                        )),
                    }
                }

                /* 6.3.3 */
                TokenKind::OpenCt => parse_compound(ctx, var_info, stream, s, token.location),

                /* 6.3.1.3 */
                TokenKind::End => Ok((Term::new_atom(s, token.location), next, 0)),

                /* 6.3.4.2 */
                _ => {
                    if let Some(i) = ctx.operators.get(&s) {
                        for o in i {
                            match o {
                                Operator::fx(p) => {
                                    if *p > max_precedence {
                                        break;
                                    }

                                    return match parse_term(ctx, var_info, stream, next, *p - 1) {
                                        Ok((term, next, _)) => Ok((
                                            Term::new_compound(s, token.location, vec![term]),
                                            next,
                                            *p,
                                        )),
                                        Err(e) => {
                                            if let ErrorKind::UnexpectedToken(k) = e.kind {
                                                Ok((
                                                    Term::new_atom(s, token.location),
                                                    Token {
                                                        kind: k,
                                                        location: e.location,
                                                    },
                                                    1201,
                                                ))
                                            } else {
                                                Err(e)
                                            }
                                        }
                                    };
                                }
                                Operator::fy(p) => {
                                    if *p > max_precedence {
                                        break;
                                    }

                                    return match parse_term(ctx, var_info, stream, next, *p - 1) {
                                        Ok((term, next, _)) => Ok((
                                            Term::new_compound(s, token.location, vec![term]),
                                            next,
                                            *p,
                                        )),
                                        Err(e) => {
                                            if let ErrorKind::UnexpectedToken(k) = e.kind {
                                                Ok((
                                                    Term::new_atom(s, token.location),
                                                    Token {
                                                        kind: k,
                                                        location: e.location,
                                                    },
                                                    1201,
                                                ))
                                            } else {
                                                Err(e)
                                            }
                                        }
                                    };
                                }
                                _ => {}
                            }
                        }
                        Ok((Term::new_atom(s, token.location), next, 1201))
                    } else {
                        Ok((Term::new_atom(s, token.location), next, 0))
                    }
                }
            }
        }

        /* 6.3.4.1 */
        TokenKind::Open | TokenKind::OpenCt => {
            let next = lexer::next(ctx, stream, false)?;
            let (term, next, _) = parse_term(ctx, var_info, stream, next, 1201)?;
            if let TokenKind::Close = next.kind {
                Ok((term, lexer::next(ctx, stream, false)?, 0))
            } else {
                Error::new(ErrorKind::ExpectedChar(')'), next.location)
            }
        }
        /* 6.3.5 */
        TokenKind::OpenL => {
            let next = lexer::next(ctx, stream, false)?;
            if let TokenKind::CloseL = next.kind {
                let next2 = lexer::next(ctx, stream, false)?;
                if let TokenKind::OpenCt = next2.kind {
                    parse_compound(
                        ctx,
                        var_info,
                        stream,
                        "[]".to_string(),
                        lexer::location_join(token.location, next2.location),
                    )
                } else {
                    Ok((
                        Term::new_atom(
                            "[]".to_string(),
                            lexer::location_join(token.location, next.location),
                        ),
                        next2,
                        0,
                    ))
                }
            } else {
                Ok((
                    parse_list(ctx, var_info, stream, next)?,
                    lexer::next(ctx, stream, false)?,
                    0,
                ))
            }
        }
        /* 6.3.6 */
        TokenKind::OpenC => {
            let next = lexer::next(ctx, stream, false)?;
            if let TokenKind::CloseC = next.kind {
                let next2 = lexer::next(ctx, stream, false)?;
                if let TokenKind::OpenCt = next2.kind {
                    parse_compound(
                        ctx,
                        var_info,
                        stream,
                        "{}".to_string(),
                        lexer::location_join(token.location, next2.location),
                    )
                } else {
                    Ok((
                        Term::new_atom(
                            "{}".to_string(),
                            lexer::location_join(token.location, next.location),
                        ),
                        next2,
                        0,
                    ))
                }
            } else {
                let (term, next, _) = parse_term(ctx, var_info, stream, next, 1201)?;
                if let TokenKind::CloseC = next.kind {
                    Ok((
                        Term::new_compound(
                            "{}".to_string(),
                            lexer::location_join(token.location, next.location),
                            vec![term],
                        ),
                        lexer::next(ctx, stream, false)?,
                        0,
                    ))
                } else {
                    Error::new(ErrorKind::ExpectedChar('}'), next.location)
                }
            }
        }
        /* 6.3.7 */
        TokenKind::DoubleQuotedList(s) => {
            /* ISO/IEC 13211-1:1995/Cor.1:2007 */
            Ok((
                parse_quoted(&ctx.flags.double_quotes, s, token.location)?,
                lexer::next(ctx, stream, false)?,
                0,
            ))
        }
        TokenKind::BackQuotedString(s) if !ctx.flags.strict_iso => Ok((
            parse_quoted(&ctx.flags.back_quotes, s, token.location)?,
            lexer::next(ctx, stream, false)?,
            0,
        )),
        _ => Error::new(ErrorKind::UnexpectedToken(token.kind), token.location),
    }
}

fn lookup_op(ctx: &Context, s: &str) -> (u16, u16, u16, bool) {
    if let Some(i) = ctx.operators.get(s) {
        for o in i {
            match o {
                Operator::xfx(p) => return (*p - 1, *p, *p - 1, true),
                Operator::xfy(p) => return (*p - 1, *p, *p, true),
                Operator::yfx(p) => return (*p, *p, *p - 1, true),
                Operator::xf(p) => return (*p - 1, *p, 0, false),
                Operator::yf(p) => return (*p, *p, 0, false),
                _ => {}
            }
        }
    }
    (0, 1201, 0, false)
}

fn parse_term(
    ctx: &Context,
    var_info: &mut Vec<VarInfo>,
    stream: &mut dyn ReadStream,
    token: Token,
    max_precedence: u16,
) -> Result<(Rc<Term>, Token, u16), Box<Error>> {
    // This is precedence climbing, if you're interested
    let (mut term, mut next, mut precedence) =
        next_term(ctx, var_info, stream, token, max_precedence)?;
    loop {
        let ((l, op_precedence, r, bin_op), f) = match &next.kind {
            TokenKind::Name(s) => (lookup_op(ctx, s), ""),
            TokenKind::Comma => ((999, 1000, 1000, true), ","),

            /* ISO/IEC 13211-1:1995/Cor.2:2012 */
            TokenKind::Bar => (lookup_op(ctx, "|"), "|"),

            _ => return Ok((term, next, precedence)),
        };

        if op_precedence > max_precedence || precedence > l {
            return Ok((term, next, precedence));
        }

        let functor = match next.kind {
            TokenKind::Name(s) => s,
            _ => f.to_string(),
        };
        let location = next.location;
        let mut args = vec![term];

        next = lexer::next(ctx, stream, false)?;

        if bin_op {
            (term, next, _) = parse_term(ctx, var_info, stream, next, r)?;
            args.push(term);
        }

        term = Term::new_compound(functor, location, args);
        precedence = op_precedence;
    }
}

pub(crate) fn next(
    ctx: Context,
    var_info: &mut Vec<VarInfo>,
    stream: &mut dyn ReadStream,
) -> Result<Option<Rc<Term>>, Box<Error>> {
    let t = lexer::next(&ctx, stream, false)?;
    if let TokenKind::Eof = t.kind {
        return Ok(None);
    }

    let (term, next, _) = parse_term(&ctx, var_info, stream, t, 1201)?;
    match next.kind {
        TokenKind::End => Ok(Some(term)),
        _ => Error::new(ErrorKind::ExpectedChar('.'), next.location),
    }
}

pub(crate) fn skip_to_end(ctx: Context, stream: &mut dyn ReadStream) -> Result<(), Box<Error>> {
    loop {
        match lexer::next(&ctx, stream, false)?.kind {
            TokenKind::Eof | TokenKind::End => break Ok(()),
            _ => {}
        }
    }
}
