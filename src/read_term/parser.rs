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

        next = match next {
            Token::Comma(_) => lexer::next(ctx, stream, false, greedy)?,
            Token::Close(p) => {
                return Ok((
                    Term::new_compound(functor, location, args),
                    lexer::next(ctx, stream, false, greedy)?,
                    0,
                ))
            }
            _ => return Error::new(ErrorKind::ExpectedChar(')'), next.span()),
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

        next = match next {
            Token::Comma(_) => lexer::next(ctx, stream, false, greedy)?,
            _ => break,
        }
    }

    let mut list: Term;
    match next {
        Token::Bar(_) => {
            next = lexer::next(ctx, stream, false, greedy)?;
            (list, next, _) = parse_term(ctx, stream, next, 999, greedy)?;
        }
        _ => list = Term::new_atom("[]".to_string(), next.span()),
    };

    if !matches!(next, Token::CloseL(_)) {
        return Error::new(ErrorKind::ExpectedChar(']'), next.span());
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
    match token {
        /* 6.3.1.1 */
        Token::Int(s, r, location) => {
            let next = lexer::next(ctx, stream, true, greedy)?;
            match next {
                Token::Exponent(t, location2) => Ok((
                    parse_float(&(s + &t), Span::concat(&location, &location2))?,
                    lexer::next(ctx, stream, false, greedy)?,
                    0,
                )),
                _ => Ok((parse_integer(&s, r, location)?, next, 0)),
            }
        }
        Token::CharCode(c, p) => Ok((
            Term {
                kind: TermKind::Integer(c as i64),
                location: p.into(),
            },
            lexer::next(ctx, stream, false, greedy)?,
            0,
        )),

        /* 6.3.2 */
        Token::Var(s, location) => Ok((
            Term {
                kind: TermKind::Var(s),
                location,
            },
            lexer::next(ctx, stream, false, greedy)?,
            0,
        )),

        /* ISO/IEC 13211-1:1995/Cor.2:2012 */
        Token::Bar(p) => {
            let next = lexer::next(ctx, stream, false, greedy)?;
            if let Token::OpenCt(_) = next {
                parse_compound(ctx, stream, "|".to_string(), p.into(), greedy)
            } else {
                Ok((Term::new_atom("|".to_string(), p.into()), next, 0))
            }
        }

        Token::Name(s, location) => {
            let next = lexer::next(ctx, stream, false, greedy)?;
            match next {
                /* 6.3.1.2  */
                Token::Int(t, r, location2) if s == "-" => {
                    let next2 = lexer::next(ctx, stream, true, greedy)?;
                    match next2 {
                        Token::Exponent(u, location2) => Ok((
                            parse_float(&(s + &t + &u), Span::concat(&location, &location2))?,
                            lexer::next(ctx, stream, false, greedy)?,
                            0,
                        )),
                        _ => Ok((
                            parse_integer(&(s + &t), r, Span::concat(&location, &location2))?,
                            next2,
                            0,
                        )),
                    }
                }
                Token::CharCode(c, p) if s == "-" => Ok((
                    Term {
                        kind: TermKind::Integer(-(c as i64)),
                        location: p.into(),
                    },
                    lexer::next(ctx, stream, false, greedy)?,
                    0,
                )),

                /* 6.3.3 */
                Token::OpenCt(_) => parse_compound(ctx, stream, s, location, greedy),

                /* 6.3.1.3 */
                Token::End(_) => Ok((Term::new_atom(s, location), next, 0)),

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
                                            Term::new_compound(s, location, vec![term]),
                                            next,
                                            *p,
                                        )),
                                        Err(e) => {
                                            if let ErrorKind::UnexpectedToken(next) = e.kind {
                                                Ok((Term::new_atom(s, location), next, 1201))
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
                                            Term::new_compound(s, location, vec![term]),
                                            next,
                                            *p,
                                        )),
                                        Err(e) => {
                                            if let ErrorKind::UnexpectedToken(next) = e.kind {
                                                Ok((Term::new_atom(s, location), next, 1201))
                                            } else {
                                                Err(e)
                                            }
                                        }
                                    };
                                }
                                _ => {}
                            }
                        }
                        Ok((Term::new_atom(s, location), next, 1201))
                    } else {
                        Ok((Term::new_atom(s, location), next, 0))
                    }
                }
            }
        }

        /* 6.3.4.1 */
        Token::Open(_) | Token::OpenCt(_) => {
            let next = lexer::next(ctx, stream, false, greedy)?;
            let (term, next, _) = parse_term(ctx, stream, next, 1201, greedy)?;
            if let Token::Close(_) = next {
                Ok((term, lexer::next(ctx, stream, false, greedy)?, 0))
            } else {
                Error::new(ErrorKind::ExpectedChar(')'), next.span())
            }
        }
        /* 6.3.5 */
        Token::OpenL(p) => {
            let mut next = lexer::next(ctx, stream, false, greedy)?;
            if let Token::CloseL(q) = next {
                next = lexer::next(ctx, stream, false, greedy)?;
                if let Token::OpenCt(_) = next {
                    parse_compound(ctx, stream, "[]".to_string(), Span::new(p, q), greedy)
                } else {
                    Ok((Term::new_atom("[]".to_string(), Span::new(p, q)), next, 0))
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
        Token::OpenC(p) => {
            let mut next = lexer::next(ctx, stream, false, greedy)?;
            if let Token::CloseC(q) = next {
                next = lexer::next(ctx, stream, false, greedy)?;
                if let Token::OpenCt(_) = next {
                    parse_compound(ctx, stream, "{}".to_string(), Span::new(p, q), greedy)
                } else {
                    Ok((Term::new_atom("{}".to_string(), Span::new(p, q)), next, 0))
                }
            } else {
                let (term, next, _) = parse_term(ctx, stream, next, 1201, greedy)?;
                if let Token::CloseC(q) = next {
                    Ok((
                        Term::new_compound("{}".to_string(), Span::new(p, q), vec![term]),
                        lexer::next(ctx, stream, false, greedy)?,
                        0,
                    ))
                } else {
                    Error::new(ErrorKind::ExpectedChar('}'), next.span())
                }
            }
        }
        /* 6.3.7 */
        Token::DoubleQuotedList(s, location) => {
            /* ISO/IEC 13211-1:1995/Cor.1:2007 */
            Ok((
                parse_quoted(&ctx.flags.double_quotes, s, location)?,
                lexer::next(ctx, stream, false, greedy)?,
                0,
            ))
        }
        Token::BackQuotedString(s, location) => Ok((
            parse_quoted(&ctx.flags.back_quotes, s, location)?,
            lexer::next(ctx, stream, false, greedy)?,
            0,
        )),
        _ => {
            let location = token.span();
            Error::new(ErrorKind::UnexpectedToken(token), location)
        }
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
        let (l, op_precedence, r, bin_op) = match &next {
            Token::Name(s, _) => lookup_op(ctx, s),
            Token::Comma(_) => (999, 1000, 1000, true),

            /* ISO/IEC 13211-1:1995/Cor.2:2012 */
            Token::Bar(_) => lookup_op(ctx, "|"),

            _ => return Ok((term, next, precedence)),
        };

        if op_precedence > max_precedence || precedence > l {
            return Ok((term, next, precedence));
        }

        let (functor, location) = match &next {
            Token::Name(s, l) => (s.clone(), l.clone()),
            Token::Bar(p) => ("|".to_string(), Span::from(p)),
            Token::Comma(p) => (",".to_string(), Span::from(p)),
            _ => panic!(),
        };
        let mut args = vec![term];

        if bin_op {
            next = lexer::next(ctx, stream, false, greedy)?;
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
    if let Token::Eof(_) = t {
        return Ok(None);
    }

    let (term, next, _) = parse_term(ctx, stream, t, 1201, greedy)?;
    match next {
        Token::End(_) => Ok(Some(term)),
        _ => Error::new(ErrorKind::ExpectedChar('.'), next.span()),
    }
}

pub(crate) fn skip_to_end(ctx: &Context, stream: &mut dyn ReadStream) -> Result<(), Box<Error>> {
    loop {
        match lexer::next(ctx, stream, false, true)? {
            Token::Eof(_) | Token::End(_) => break Ok(()),
            _ => {}
        }
    }
}
