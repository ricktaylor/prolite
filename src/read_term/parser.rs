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

fn arg(
    ctx: &Context,
    stream: &mut dyn Stream,
    token: Token,
    greedy: bool,
) -> Result<(Term, Token), Error> {
    if let Token::Name(s) = &token {
        match ctx.lookup_op(s) {
            Some(&Operator::fx(p))
            | Some(&Operator::fy(p))
            | Some(&Operator::xfx(p))
            | Some(&Operator::xfy(p))
            | Some(&Operator::yfx(p))
            | Some(&Operator::xf(p))
            | Some(&Operator::yf(p))
                if p > 999 =>
            {
                let next = lexer::next(ctx, stream, greedy)?;
                match next {
                    Token::OpenCt => Ok((
                        compound(ctx, stream, s, greedy)?,
                        lexer::next(ctx, stream, greedy)?,
                    )),
                    Token::Int(t, r) if s == "-" => Ok((
                        parse_integer(&format!("{}{}", s, t), r)?,
                        lexer::next(ctx, stream, greedy)?,
                    )),
                    Token::CharCode(c) if s == "-" => Ok((
                        Term::Integer(-(c as i64)),
                        lexer::next(ctx, stream, greedy)?,
                    )),
                    Token::Float(t) if s == "-" => Ok((
                        parse_float(&format!("{}{}", s, t))?,
                        lexer::next(ctx, stream, greedy)?,
                    )),
                    _ => Ok((Term::Atom(s.clone()), next)),
                }
            }
            _ => next_term(ctx, stream, token, 999, greedy),
        }
    } else {
        next_term(ctx, stream, token, 999, greedy)
    }
}

fn compound(ctx: &Context, stream: &mut dyn Stream, s: &str, greedy: bool) -> Result<Term, Error> {
    let mut c = Compound {
        functor: s.to_string(),
        args: Vec::new(),
    };

    let mut next = lexer::next(ctx, stream, greedy)?;
    loop {
        let term;
        (term, next) = arg(ctx, stream, next, greedy)?;
        c.args.push(term);

        match next {
            Token::Comma => {
                next = lexer::next(ctx, stream, greedy)?;
            }
            Token::Close => return Ok(Term::Compound(c)),
            _ => return Error::new(ErrorKind::ExpectedToken(Token::Close, next)),
        }
    }
}

fn name(
    ctx: &Context,
    stream: &mut dyn Stream,
    s: &str,
    max_precedence: u16,
    greedy: bool,
) -> Result<(Term, Token, u16), Error> {
    let next = lexer::next(ctx, stream, greedy)?;
    match next {
        Token::OpenCt => Ok((
            compound(ctx, stream, s, greedy)?,
            lexer::next(ctx, stream, greedy)?,
            0,
        )),
        Token::Int(t, r) if s == "-" => Ok((
            parse_integer(&format!("{}{}", s, t), r)?,
            lexer::next(ctx, stream, greedy)?,
            0,
        )),
        Token::CharCode(c) if s == "-" => Ok((
            Term::Integer(-(c as i64)),
            lexer::next(ctx, stream, greedy)?,
            0,
        )),
        Token::Float(t) if s == "-" => Ok((
            parse_float(&format!("{}{}", s, t))?,
            lexer::next(ctx, stream, greedy)?,
            0,
        )),
        Token::Comma | Token::Close => Ok((Term::Atom(s.to_string()), next, 0)),
        _ => match ctx.lookup_prefix_op(s) {
            Some(&Operator::fx(p)) if p <= max_precedence => {
                let (term, next) = next_term(ctx, stream, next, p - 1, greedy)?;
                Ok((Term::new_compound(s, vec![term]), next, p))
            }
            Some(&Operator::fy(p)) if p <= max_precedence => {
                let (term, next) = next_term(ctx, stream, next, p, greedy)?;
                Ok((Term::new_compound(s, vec![term]), next, p))
            }
            _ => Ok((Term::Atom(s.to_string()), next, 0)),
        },
    }
}

fn list(ctx: &Context, stream: &mut dyn Stream, greedy: bool) -> Result<Term, Error> {
    let mut terms = Vec::new();
    let mut next = lexer::next(ctx, stream, greedy)?;
    if !matches!(next, Token::CloseL) {
        loop {
            let term: Term;
            (term, next) = arg(ctx, stream, next, greedy)?;
            terms.push(term);

            match next {
                Token::Comma => next = lexer::next(ctx, stream, greedy)?,
                _ => break,
            }
        }
    }

    let mut list: Term;
    match next {
        Token::Bar => {
            (list, next) = arg(ctx, stream, next, greedy)?;
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

fn quoted(
    ctx: &Context,
    stream: &mut dyn Stream,
    flags: &flags::QuoteFlag,
    s: String,
    max_precedence: u16,
    greedy: bool,
) -> Result<(Term, Token, u16), Error> {
    match flags {
        flags::QuoteFlag::Atom => name(ctx, stream, &s, max_precedence, greedy),
        flags::QuoteFlag::Chars => {
            let mut list = Term::Atom("[]".to_string());
            for c in s.chars().rev() {
                list = Term::new_compound(".", vec![Term::Atom(c.to_string()), list]);
            }
            Ok((list, lexer::next(ctx, stream, greedy)?, 0))
        }
        flags::QuoteFlag::Codes => {
            let mut list = Term::Atom("[]".to_string());
            for c in s.chars().rev() {
                list = Term::new_compound(".", vec![Term::Integer(c as i64), list]);
            }
            Ok((list, lexer::next(ctx, stream, greedy)?, 0))
        }
    }
}

fn lookup_op(ctx: &Context, s: &str, max_precedence: u16) -> (u16, u16, usize, bool) {
    match ctx.lookup_op(s) {
        Some(&Operator::fx(p)) | Some(&Operator::fy(p)) => (0, 0, 1, p > max_precedence),
        Some(&Operator::xfx(p)) => (p - 1, p - 1, 2, p > max_precedence),
        Some(&Operator::xfy(p)) => (p - 1, p, 2, p > max_precedence),
        Some(&Operator::yfx(p)) => (p, p - 1, 2, p > max_precedence),
        Some(&Operator::xf(p)) => (p - 1, 0, 1, p > max_precedence),
        Some(&Operator::yf(p)) => (p - 1, 0, 1, p > max_precedence),
        _ => (0, 0, 0, true),
    }
}

fn next_term(
    ctx: &Context,
    stream: &mut dyn Stream,
    token: Token,
    max_precedence: u16,
    greedy: bool,
) -> Result<(Term, Token), Error> {
    let mut term;
    let mut next;
    let mut precedence = 0u16;

    match token {
        Token::Name(s) => (term, next, precedence) = name(ctx, stream, &s, max_precedence, greedy)?,
        Token::Var(s) => {
            term = Term::Var(s);
            next = lexer::next(ctx, stream, greedy)?
        }
        Token::Int(s, r) => {
            term = parse_integer(&s, r)?;
            next = lexer::next(ctx, stream, greedy)?
        }
        Token::CharCode(c) => {
            term = Term::Integer(c as i64);
            next = lexer::next(ctx, stream, greedy)?
        }
        Token::Float(s) => {
            term = parse_float(&s)?;
            next = lexer::next(ctx, stream, greedy)?
        }
        Token::DoubleQuotedList(s) => {
            /* ISO/IEC 13211-1:1995/Cor.1:2007 */
            (term, next, precedence) = quoted(
                ctx,
                stream,
                &ctx.flags.double_quotes,
                s,
                max_precedence,
                greedy,
            )?
        }
        Token::BackQuotedString(s) => {
            (term, next, precedence) = quoted(
                ctx,
                stream,
                &ctx.flags.back_quotes,
                s,
                max_precedence,
                greedy,
            )?
        }
        Token::Open | Token::OpenCt => {
            next = lexer::next(ctx, stream, greedy)?;
            (term, next) = next_term(ctx, stream, next, 1201, greedy)?;
            match next {
                Token::Close => next = lexer::next(ctx, stream, greedy)?,
                _ => return Error::new(ErrorKind::ExpectedToken(Token::Close, next)),
            }
        }
        Token::OpenC => {
            next = lexer::next(ctx, stream, greedy)?;
            (term, next) = next_term(ctx, stream, next, 1201, greedy)?;
            match next {
                Token::CloseC => {
                    term = Term::new_compound("{}", vec![term]);
                    next = lexer::next(ctx, stream, greedy)?;
                }
                _ => return Error::new(ErrorKind::ExpectedToken(Token::CloseC, next)),
            }
        }
        Token::OpenL => {
            term = list(ctx, stream, greedy)?;
            next = lexer::next(ctx, stream, greedy)?
        }
        _ => return Error::new(ErrorKind::UnexpectedToken(token)),
    }

    /* This is precedence climbing, if you're interested */
    loop {
        let functor: String;
        let (r, l, arity, do_break) = match next {
            Token::Name(ref s) => {
                functor = s.clone();
                lookup_op(ctx, s, max_precedence)
            }
            Token::Bar => {
                /* ISO/IEC 13211-1:1995/Cor.2:2012 */
                functor = '|'.to_string();
                lookup_op(ctx, "|", max_precedence)
            }
            Token::Comma => {
                functor = ",".to_string();
                (999, 1000, 2, 1000 > max_precedence)
            }
            _ => return Ok((term, next)),
        };

        if do_break || precedence > l {
            return Ok((term, next));
        }

        let mut c = Compound {
            functor,
            args: vec![term],
        };

        if arity == 2 {
            (term, next) = next_term(ctx, stream, next, r, greedy)?;
            c.args.push(term);
        }

        term = Term::Compound(c);
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

    let (term, next) = next_term(ctx, stream, t, 1201, greedy)?;
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
