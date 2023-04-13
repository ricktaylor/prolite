use std::str::FromStr;

use super::*;
use error::*;
use lexer::*;
use stream::Span;
use term::*;

use operators::Operator;
use stream::ReadStream;

fn parse_integer(s: &str, radix: u32, location: Span) -> Result<Term, Box<Error>> {
    match i64::from_str_radix(s, radix) {
        Ok(i) => Ok(Term {
            kind: TermKind::Integer(i),
            location,
        }),
        Err(e) => Error::new(ErrorKind::ParseIntError(e), location),
    }
}

fn parse_float(s: &str, location: Span) -> Result<Term, Box<Error>> {
    match f64::from_str(s) {
        Ok(f) => Ok(Term {
            kind: TermKind::Float(f),
            location,
        }),
        Err(e) => Error::new(ErrorKind::ParseFloatError(e), location),
    }
}

fn parse_compound(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    functor: String,
    location: Span,
    greedy: bool,
) -> Result<(Term, Token, u16), Box<Error>> {
    let mut args = Vec::new();
    let mut next = lexer::next(ctx, stream, false, greedy)?;
    loop {
        let term;
        (term, next, _) = parse_term(ctx, stream, next, 999, greedy)?;
        args.push(term);

        next = match next.kind {
            TokenKind::Comma => lexer::next(ctx, stream, false, greedy)?,
            TokenKind::Close => {
                return Ok((
                    Term::new_compound(functor, location, args),
                    lexer::next(ctx, stream, false, greedy)?,
                    0,
                ))
            }
            _ => return Error::new(ErrorKind::ExpectedChar(')'), next.location),
        }
    }
}

fn parse_list(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    token: Token,
    greedy: bool,
) -> Result<Term, Box<Error>> {
    let mut terms = Vec::new();
    let mut next = token;
    loop {
        let term: Term;
        (term, next, _) = parse_term(ctx, stream, next, 999, greedy)?;
        terms.push(term);

        next = match next.kind {
            TokenKind::Comma => lexer::next(ctx, stream, false, greedy)?,
            _ => break,
        }
    }

    let mut list: Term;
    match next.kind {
        TokenKind::Bar => {
            next = lexer::next(ctx, stream, false, greedy)?;
            (list, next, _) = parse_term(ctx, stream, next, 999, greedy)?;
        }
        _ => list = Term::new_atom("[]".to_string(), next.location.clone()),
    };

    if !matches!(next.kind, TokenKind::CloseL) {
        return Error::new(ErrorKind::ExpectedChar(']'), next.location);
    }

    while let Some(t) = terms.pop() {
        list = Term::new_compound(".".to_string(), t.location.clone(), vec![t, list]);
    }
    Ok(list)
}

fn parse_quoted(flags: &flags::QuoteFlag, s: String, location: Span) -> Result<Term, Box<Error>> {
    match flags {
        flags::QuoteFlag::Atom => Ok(Term::new_atom(s, location)),
        flags::QuoteFlag::Chars => {
            let mut list = Term::new_atom("[]".to_string(), location.clone());
            for c in s.chars().rev() {
                list = Term::new_compound(
                    ".".to_string(),
                    location.clone(),
                    vec![Term::new_atom(c.to_string(), location.clone()), list],
                );
            }
            Ok(list)
        }
        flags::QuoteFlag::Codes => {
            let mut list = Term::new_atom("[]".to_string(), location.clone());
            for c in s.chars().rev() {
                list = Term::new_compound(
                    ".".to_string(),
                    location.clone(),
                    vec![
                        Term {
                            kind: TermKind::Integer(c as i64),
                            location: location.clone(),
                        },
                        list,
                    ],
                );
            }
            Ok(list)
        }
    }
}

fn next_term(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    token: Token,
    max_precedence: u16,
    greedy: bool,
) -> Result<(Term, Token, u16), Box<Error>> {
    match token.kind {
        /* 6.3.1.1 */
        TokenKind::Int(s, r) => {
            let next = lexer::next(ctx, stream, true, greedy)?;
            match next.kind {
                TokenKind::Exponent(t) => Ok((
                    parse_float(&(s + &t), Span::concat(&token.location, &next.location))?,
                    lexer::next(ctx, stream, false, greedy)?,
                    0,
                )),
                _ => Ok((parse_integer(&s, r, token.location)?, next, 0)),
            }
        }
        TokenKind::CharCode(c) => Ok((
            Term {
                kind: TermKind::Integer(c as i64),
                location: token.location,
            },
            lexer::next(ctx, stream, false, greedy)?,
            0,
        )),

        /* 6.3.2 */
        TokenKind::Var(s) => Ok((
            Term {
                kind: TermKind::Var(s),
                location: token.location,
            },
            lexer::next(ctx, stream, false, greedy)?,
            0,
        )),

        /* ISO/IEC 13211-1:1995/Cor.2:2012 */
        TokenKind::Bar => {
            let next = lexer::next(ctx, stream, false, greedy)?;
            if let TokenKind::OpenCt = next.kind {
                parse_compound(ctx, stream, "|".to_string(), token.location, greedy)
            } else {
                Ok((Term::new_atom("|".to_string(), token.location), next, 0))
            }
        }

        TokenKind::Name(s) => {
            let next = lexer::next(ctx, stream, false, greedy)?;
            match next.kind {
                /* 6.3.1.2  */
                TokenKind::Int(t, r) if s == "-" => {
                    let next2 = lexer::next(ctx, stream, true, greedy)?;
                    match next2.kind {
                        TokenKind::Exponent(u) => Ok((
                            parse_float(
                                &(s + &t + &u),
                                Span::concat(&token.location, &next2.location),
                            )?,
                            lexer::next(ctx, stream, false, greedy)?,
                            0,
                        )),
                        _ => Ok((
                            parse_integer(
                                &(s + &t),
                                r,
                                Span::concat(&token.location, &next.location),
                            )?,
                            next2,
                            0,
                        )),
                    }
                }
                TokenKind::CharCode(c) if s == "-" => Ok((
                    Term {
                        kind: TermKind::Integer(-(c as i64)),
                        location: token.location,
                    },
                    lexer::next(ctx, stream, false, greedy)?,
                    0,
                )),

                /* 6.3.3 */
                TokenKind::OpenCt => parse_compound(ctx, stream, s, token.location, greedy),

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

                                    return match parse_term(ctx, stream, next, *p - 1, greedy) {
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

                                    return match parse_term(ctx, stream, next, *p - 1, greedy) {
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
            let next = lexer::next(ctx, stream, false, greedy)?;
            let (term, next, _) = parse_term(ctx, stream, next, 1201, greedy)?;
            if let TokenKind::Close = next.kind {
                Ok((term, lexer::next(ctx, stream, false, greedy)?, 0))
            } else {
                Error::new(ErrorKind::ExpectedChar(')'), next.location)
            }
        }
        /* 6.3.5 */
        TokenKind::OpenL => {
            let next = lexer::next(ctx, stream, false, greedy)?;
            if let TokenKind::CloseL = next.kind {
                let next2 = lexer::next(ctx, stream, false, greedy)?;
                if let TokenKind::OpenCt = next2.kind {
                    parse_compound(
                        ctx,
                        stream,
                        "[]".to_string(),
                        Span::concat(&token.location, &next2.location),
                        greedy,
                    )
                } else {
                    Ok((
                        Term::new_atom(
                            "[]".to_string(),
                            Span::concat(&token.location, &next.location),
                        ),
                        next2,
                        0,
                    ))
                }
            } else {
                Ok((
                    parse_list(ctx, stream, next, greedy)?,
                    lexer::next(ctx, stream, false, greedy)?,
                    0,
                ))
            }
        }
        /* 6.3.6 */
        TokenKind::OpenC => {
            let next = lexer::next(ctx, stream, false, greedy)?;
            if let TokenKind::CloseC = next.kind {
                let next2 = lexer::next(ctx, stream, false, greedy)?;
                if let TokenKind::OpenCt = next2.kind {
                    parse_compound(
                        ctx,
                        stream,
                        "{}".to_string(),
                        Span::concat(&token.location, &next2.location),
                        greedy,
                    )
                } else {
                    Ok((
                        Term::new_atom(
                            "{}".to_string(),
                            Span::concat(&token.location, &next.location),
                        ),
                        next2,
                        0,
                    ))
                }
            } else {
                let (term, next, _) = parse_term(ctx, stream, next, 1201, greedy)?;
                if let TokenKind::CloseC = next.kind {
                    Ok((
                        Term::new_compound(
                            "{}".to_string(),
                            Span::concat(&token.location, &next.location),
                            vec![term],
                        ),
                        lexer::next(ctx, stream, false, greedy)?,
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
                lexer::next(ctx, stream, false, greedy)?,
                0,
            ))
        }
        TokenKind::BackQuotedString(s) => Ok((
            parse_quoted(&ctx.flags.back_quotes, s, token.location)?,
            lexer::next(ctx, stream, false, greedy)?,
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
    stream: &mut dyn ReadStream,
    token: Token,
    max_precedence: u16,
    greedy: bool,
) -> Result<(Term, Token, u16), Box<Error>> {
    // This is precedence climbing, if you're interested
    let (mut term, mut next, mut precedence) =
        next_term(ctx, stream, token, max_precedence, greedy)?;
    loop {
        let (l, op_precedence, r, bin_op) = match &next.kind {
            TokenKind::Name(s) => lookup_op(ctx, s),
            TokenKind::Comma => (999, 1000, 1000, true),

            /* ISO/IEC 13211-1:1995/Cor.2:2012 */
            TokenKind::Bar => lookup_op(ctx, "|"),

            _ => return Ok((term, next, precedence)),
        };

        if op_precedence > max_precedence || precedence > l {
            return Ok((term, next, precedence));
        }

        let functor = match next.kind {
            TokenKind::Name(s) => s,
            TokenKind::Bar => "|".to_string(),
            TokenKind::Comma => ",".to_string(),
            _ => panic!(),
        };
        let location = next.location;
        let mut args = vec![term];

        next = lexer::next(ctx, stream, false, greedy)?;

        if bin_op {
            (term, next, _) = parse_term(ctx, stream, next, r, greedy)?;
            args.push(term);
        }

        term = Term::new_compound(functor, location, args);
        precedence = op_precedence;
    }
}

pub(crate) fn next(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    greedy: bool,
) -> Result<Option<Term>, Box<Error>> {
    let t = lexer::next(ctx, stream, false, greedy)?;
    if let TokenKind::Eof = t.kind {
        return Ok(None);
    }

    let (term, next, _) = parse_term(ctx, stream, t, 1201, greedy)?;
    match next.kind {
        TokenKind::End => Ok(Some(term)),
        _ => Error::new(ErrorKind::ExpectedChar('.'), next.location),
    }
}

pub(crate) fn skip_to_end(ctx: &Context, stream: &mut dyn ReadStream) -> Result<(), Box<Error>> {
    loop {
        match lexer::next(ctx, stream, false, true)?.kind {
            TokenKind::Eof | TokenKind::End => break Ok(()),
            _ => {}
        }
    }
}
