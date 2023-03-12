use std::str::FromStr;

use super::*;
use error::*;
use lexer::*;
use term::*;

use operators::Operator;
use stream::Stream;

fn parse_integer(s: &str, radix: u32) -> Result<Term, Error> {
    Ok(Term::Integer(i64::from_str_radix(s, radix)?))
}

fn parse_float(s: &str) -> Result<Term, Error> {
    Ok(Term::Float(f64::from_str(s)?))
}

fn parse_compound(
    ctx: &Context,
    stream: &mut dyn Stream,
    s: &str,
    greedy: bool,
) -> Result<(Term, Token, u16), Error> {
    let mut c = Compound {
        functor: s.to_string(),
        args: Vec::new(),
    };

    let mut next = lexer::next(ctx, stream, greedy)?;
    loop {
        let term;
        (term, next, _) = parse_term(ctx, stream, next, 999, greedy)?;
        c.args.push(term);

        match next {
            Token::Comma => {
                next = lexer::next(ctx, stream, greedy)?;
            }
            Token::Close => return Ok((Term::Compound(c), lexer::next(ctx, stream, greedy)?, 0)),
            _ => return Error::new(ErrorKind::ExpectedToken(Token::Close, next)),
        }
    }
}

fn parse_list(
    ctx: &Context,
    stream: &mut dyn Stream,
    token: Token,
    greedy: bool,
) -> Result<Term, Error> {
    let mut terms = Vec::new();
    let mut next = token;
    loop {
        let term: Term;
        (term, next, _) = parse_term(ctx, stream, next, 999, greedy)?;
        terms.push(term);

        match next {
            Token::Comma => next = lexer::next(ctx, stream, greedy)?,
            _ => break,
        }
    }

    let mut list: Term;
    match next {
        Token::Bar => {
            next = lexer::next(ctx, stream, greedy)?;
            (list, next, _) = parse_term(ctx, stream, next, 999, greedy)?;
        }
        _ => list = Term::Atom("[]".to_string()),
    };

    if !matches!(next, Token::CloseL) {
        return Error::new(ErrorKind::ExpectedToken(Token::CloseL, next));
    }

    while let Some(t) = terms.pop() {
        list = Term::new_compound(".", vec![t, list]);
    }
    Ok(list)
}

fn parse_quoted(flags: &flags::QuoteFlag, s: String) -> Result<Term, Error> {
    match flags {
        flags::QuoteFlag::Atom => Ok(Term::Atom(s)),
        flags::QuoteFlag::Chars => {
            let mut list = Term::Atom("[]".to_string());
            for c in s.chars().rev() {
                list = Term::new_compound(".", vec![Term::Atom(c.to_string()), list]);
            }
            Ok(list)
        }
        flags::QuoteFlag::Codes => {
            let mut list = Term::Atom("[]".to_string());
            for c in s.chars().rev() {
                list = Term::new_compound(".", vec![Term::Integer(c as i64), list]);
            }
            Ok(list)
        }
    }
}

fn next_term(
    ctx: &Context,
    stream: &mut dyn Stream,
    token: Token,
    max_precedence: u16,
    greedy: bool,
) -> Result<(Term, Token, u16), Error> {
    match token {
        /* 6.3.1.1 */
        Token::Int(s, r) => Ok((parse_integer(&s, r)?, lexer::next(ctx, stream, greedy)?, 0)),
        Token::Float(s) => Ok((parse_float(&s)?, lexer::next(ctx, stream, greedy)?, 0)),
        Token::CharCode(c) => Ok((
            Term::Integer(c as i64),
            lexer::next(ctx, stream, greedy)?,
            0,
        )),

        /* 6.3.2 */
        Token::Var(s) => Ok((Term::Var(s), lexer::next(ctx, stream, greedy)?, 0)),

        /* ISO/IEC 13211-1:1995/Cor.2:2012 */
        Token::Bar => {
            let next = lexer::next(ctx, stream, greedy)?;
            if let Token::OpenCt = next {
                parse_compound(ctx, stream, "|", greedy)
            } else {
                Ok((Term::Atom("|".to_string()), next, 0))
            }
        }

        Token::Name(s) => {
            let next = lexer::next(ctx, stream, greedy)?;
            match next {
                /* 6.3.1.2  */
                Token::Int(t, r) if s == "-" => Ok((
                    parse_integer(&format!("{}{}", s, t), r)?,
                    lexer::next(ctx, stream, greedy)?,
                    0,
                )),
                Token::Float(t) if s == "-" => Ok((
                    parse_float(&format!("{}{}", s, t))?,
                    lexer::next(ctx, stream, greedy)?,
                    0,
                )),
                Token::CharCode(c) if s == "-" => Ok((
                    Term::Integer(-(c as i64)),
                    lexer::next(ctx, stream, greedy)?,
                    0,
                )),

                /* 6.3.3 */
                Token::OpenCt => parse_compound(ctx, stream, &s, greedy),

                /* 6.3.1.3 */
                Token::End => Ok((Term::Atom(s.to_string()), next, 0)),

                /* 6.3.4.2 */
                _ => {
                    if let Some(i) = ctx.operators.get(&s) {
                        for o in i {
                            match o {
                                Operator::fx(p) => {
                                    if *p <= max_precedence {
                                        let (term, next, _) =
                                            parse_term(ctx, stream, next, *p - 1, greedy)?;
                                        return Ok((Term::new_compound(&s, vec![term]), next, *p));
                                    }
                                    break;
                                }
                                Operator::fy(p) => {
                                    if *p <= max_precedence {
                                        let (term, next, _) =
                                            parse_term(ctx, stream, next, *p, greedy)?;
                                        return Ok((Term::new_compound(&s, vec![term]), next, *p));
                                    }
                                    break;
                                }
                                _ => {}
                            }
                        }
                        Ok((Term::Atom(s.to_string()), next, 1201))
                    } else {
                        Ok((Term::Atom(s.to_string()), next, 0))
                    }
                }
            }
        }

        /* 6.3.4.1 */
        Token::Open | Token::OpenCt => {
            let next = lexer::next(ctx, stream, greedy)?;
            let (term, next, _) = parse_term(ctx, stream, next, 1201, greedy)?;
            if let Token::Close = next {
                Ok((term, lexer::next(ctx, stream, greedy)?, 0))
            } else {
                Error::new(ErrorKind::ExpectedToken(Token::Close, next))
            }
        }
        /* 6.3.5 */
        Token::OpenL => {
            let mut next = lexer::next(ctx, stream, greedy)?;
            if let Token::CloseL = next {
                next = lexer::next(ctx, stream, greedy)?;
                if let Token::OpenCt = next {
                    parse_compound(ctx, stream, "[]", greedy)
                } else {
                    Ok((Term::Atom("[]".to_string()), next, 0))
                }
            } else {
                Ok((
                    parse_list(ctx, stream, next, greedy)?,
                    lexer::next(ctx, stream, greedy)?,
                    0,
                ))
            }
        }
        /* 6.3.6 */
        Token::OpenC => {
            let mut next = lexer::next(ctx, stream, greedy)?;
            if let Token::CloseC = next {
                next = lexer::next(ctx, stream, greedy)?;
                if let Token::OpenCt = next {
                    parse_compound(ctx, stream, "{}", greedy)
                } else {
                    Ok((Term::Atom("{}".to_string()), next, 0))
                }
            } else {
                let (term, next, _) = parse_term(ctx, stream, next, 1201, greedy)?;
                if let Token::CloseC = next {
                    Ok((
                        Term::new_compound("{}", vec![term]),
                        lexer::next(ctx, stream, greedy)?,
                        0,
                    ))
                } else {
                    Error::new(ErrorKind::ExpectedToken(Token::CloseC, next))
                }
            }
        }
        /* 6.3.7 */
        Token::DoubleQuotedList(s) => {
            /* ISO/IEC 13211-1:1995/Cor.1:2007 */
            Ok((
                parse_quoted(&ctx.flags.double_quotes, s)?,
                lexer::next(ctx, stream, greedy)?,
                0,
            ))
        }
        Token::BackQuotedString(s) => Ok((
            parse_quoted(&ctx.flags.back_quotes, s)?,
            lexer::next(ctx, stream, greedy)?,
            0,
        )),
        _ => Error::new(ErrorKind::UnexpectedToken(token)),
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
    stream: &mut dyn Stream,
    token: Token,
    max_precedence: u16,
    greedy: bool,
) -> Result<(Term, Token, u16), Error> {
    // This is precedence climbing, if you're interested
    let (mut term, mut next, mut precedence) =
        next_term(ctx, stream, token, max_precedence, greedy)?;
    loop {
        let (l, op_precedence, r, bin_op) = match next {
            Token::Name(ref s) => lookup_op(ctx, s),
            Token::Comma => (999, 1000, 999, true),

            /* ISO/IEC 13211-1:1995/Cor.2:2012 */
            Token::Bar => lookup_op(ctx, "|"),

            _ => return Ok((term, next, precedence)),
        };

        if op_precedence > max_precedence || precedence > l {
            return Ok((term, next, precedence));
        }

        let mut c = Compound {
            functor: match next {
                Token::Name(ref s) => s.clone(),
                Token::Bar => "|".to_string(),
                Token::Comma => ",".to_string(),
                _ => panic!(),
            },
            args: vec![term],
        };

        if bin_op {
            next = lexer::next(ctx, stream, greedy)?;
            (term, next, _) = parse_term(ctx, stream, next, r, greedy)?;
            c.args.push(term);
        }

        term = Term::Compound(c);
        precedence = op_precedence;
    }
}

pub(crate) fn next(
    ctx: &Context,
    stream: &mut dyn Stream,
    greedy: bool,
) -> Result<Option<Term>, Error> {
    let t = lexer::next(ctx, stream, greedy)?;
    if let Token::Eof = t {
        return Ok(None);
    }

    let (term, next, _) = parse_term(ctx, stream, t, 1201, greedy)?;
    match next {
        Token::End => Ok(Some(term)),
        _ => Error::new(ErrorKind::ExpectedToken(Token::End, next)),
    }
}

pub(crate) fn skip_to_end(ctx: &Context, stream: &mut dyn Stream) -> Result<(), Error> {
    loop {
        match lexer::next(ctx, stream, true)? {
            Token::Eof | Token::End => break Ok(()),
            _ => {}
        }
    }
}
