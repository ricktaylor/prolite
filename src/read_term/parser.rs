use std::str::FromStr;

use crate::operators::*;
use crate::flags::QuoteFlag;
use super::*;
use token::*;
use term::*;
use error::*;

fn parse_integer(s: &str, radix: u32) -> Result<Term,Error> {
    Ok(Term::Integer(i64::from_str_radix(s,radix)?))
}

fn parse_float(s: &str) -> Result<Term,Error> {
    Ok(Term::Float(f64::from_str(s)?))
}

fn arg(ctx: &Context, stream: &mut dyn Stream, token: Token) -> Result<(Term,Token),Error> {
    if let Token::Name(s) = &token {
        match ctx.lookup_op(s) {
            Some(&Operator::fx(p)) |
            Some(&Operator::fy(p)) |
            Some(&Operator::xfx(p)) |
            Some(&Operator::xfy(p)) |
            Some(&Operator::yfx(p)) |
            Some(&Operator::xf(p)) |
            Some(&Operator::yf(p)) if p > 999 => {
                let next = token::next(ctx,stream)?;
                match next {
                    Token::OpenCt => Ok((compound(ctx,stream,s)?,token::next(ctx,stream)?)),
                    Token::Int(t,r) if s == "-" => Ok((parse_integer(&format!("{}{}",s,t),r)?,token::next(ctx,stream)?)),
                    Token::CharCode(c) if s == "-" => Ok((Term::Integer(-(c as i64)),token::next(ctx,stream)?)),
                    Token::Float(t) if s == "-" => Ok((parse_float(&format!("{}{}",s,t))?,token::next(ctx,stream)?)),
                    _ => Ok((Term::Atom(s.clone()),next))
                }
            },
            _ => next_term(ctx,stream,token,999)
        }
    } else {
        next_term(ctx,stream,token,999)
    }
}

fn compound(ctx: &Context, stream: &mut dyn Stream, s: &str) -> Result<Term,Error> {
    let mut c = Compound{
        functor: s.to_string(),
        args: Vec::new()
    };

    let mut next = token::next(ctx,stream)?;
    loop {
        next = {
            let (term,next) = arg(ctx,stream,next)?;
            c.args.push(term);
            next
        };

        match next {
            Token::Comma => {},
            Token::Close => return Ok(Term::Compound(c)),
            _ => return Error::new(ErrorKind::ExpectedToken(Token::Close))
        }
    }
}

fn name(ctx: &Context, stream: &mut dyn Stream, s: &str, max_precedence: u16) -> Result<(Term,Token,u16),Error> {
    let next = token::next(ctx,stream)?;
    match next {
        Token::OpenCt => Ok((compound(ctx,stream,s)?,token::next(ctx,stream)?,0)),
        Token::Int(t,r) if s == "-" => Ok((parse_integer(&format!("{}{}",s,t),r)?,token::next(ctx,stream)?,0)),
        Token::CharCode(c) if s == "-" => Ok((Term::Integer(-(c as i64)),token::next(ctx,stream)?,0)),
        Token::Float(t) if s == "-" => Ok((parse_float(&format!("{}{}",s,t))?,token::next(ctx,stream)?,0)),
        Token::Comma |
        Token::Close => Ok((Term::Atom(s.to_string()),next,0)),
        _ => {
            match ctx.lookup_prefix_op(s) {
                Some(&Operator::fx(p)) if p <= max_precedence => {
                    let (term,next) = next_term(ctx,stream,next,p-1)?;
                    Ok((Term::new_compound(s,vec![term]),next,p))
                },
                Some(&Operator::fy(p)) if p <= max_precedence => {
                    let (term,next) = next_term(ctx,stream,next,p)?;
                    Ok((Term::new_compound(s,vec![term]),next,p))
                },
                _ => Ok((Term::Atom(s.to_string()),next,0))
            }
        }
    }

}

fn list(ctx: &Context, stream: &mut dyn Stream) -> Result<Term,Error> {
    let mut terms = Vec::new();
    let mut token = token::next(ctx,stream)?;
    if ! matches!(token,Token::CloseL) {
        loop {
            let (term,next) = arg(ctx,stream,token)?;
            terms.push(term);
            token = next;

            match token {
                Token::Comma => token = token::next(ctx,stream)?,
                _ => break
            }
        }
    }

    let mut list = match token {
        Token::Bar => {
            let (term,next) = arg(ctx,stream,token)?;
            token = next;
            term
        },
        _ => Term::Atom("[]".to_string())
    };

    if ! matches!(token,Token::CloseL) {
        return Error::new(ErrorKind::ExpectedToken(Token::CloseL));
    }

    while let Some(t) = terms.pop() {
        list = Term::new_compound(".",vec![t,list]);
    }
    Ok(list)
}

fn quoted(ctx: &Context, stream: &mut dyn Stream, flags: &QuoteFlag, s: String, max_precedence: u16) -> Result<(Term,Token,u16),Error> {
    match flags {
        QuoteFlag::Atom => name(ctx,stream,&s,max_precedence),
        QuoteFlag::Chars => {
            let mut list = Term::Atom("[]".to_string());
            for c in s.chars().rev() {
                list = Term::new_compound(".",vec![Term::Atom(c.to_string()),list]);
            }
            Ok((list,token::next(ctx,stream)?,0))
        },
        QuoteFlag::Codes => {
            let mut list = Term::Atom("[]".to_string());
            for c in s.chars().rev() {
                list = Term::new_compound(".",vec![Term::Integer(c as i64),list]);
            }
            Ok((list,token::next(ctx,stream)?,0))
        }
    }
}

fn lookup_op(ctx: &Context, s: &str, max_precedence: u16) -> (u16,u16,usize,bool) {
    match ctx.lookup_op(s) {
        Some(&Operator::fx(p)) |
        Some(&Operator::fy(p)) => (0,0,1,p > max_precedence),
        Some(&Operator::xfx(p)) => (p-1,p-1,2,p > max_precedence),
        Some(&Operator::xfy(p)) => (p-1,p,2,p > max_precedence),
        Some(&Operator::yfx(p)) => (p,p-1,2,p > max_precedence),
        Some(&Operator::xf(p)) => (p-1,0,1,p > max_precedence),
        Some(&Operator::yf(p)) => (p-1,0,1,p > max_precedence),
        _ => (0,0,0,true)
    }
}

fn next_term(ctx: &Context, stream: &mut dyn Stream, token: Token, max_precedence: u16) -> Result<(Term,Token),Error> {
    let (mut term, mut next,precedence) = match token {
        Token::Name(s) => name(ctx,stream,&s,max_precedence)?,
        Token::Var(s) => (Term::Var(s),token::next(ctx,stream)?,0),
        Token::Int(s,r) => (parse_integer(&s,r)?,token::next(ctx,stream)?,0),
        Token::CharCode(c) => (Term::Integer(c as i64),token::next(ctx,stream)?,0),
        Token::Float(s) => (parse_float(&s)?,token::next(ctx,stream)?,0),
        Token::DoubleQuotedList(s) => quoted(ctx,stream,&ctx.flags.double_quotes,s,max_precedence)?, /* ISO/IEC 13211-1:1995/Cor.1:2007 */
        Token::BackQuotedString(s) => quoted(ctx,stream,&ctx.flags.back_quotes,s,max_precedence)?,
        Token::Open |
        Token::OpenCt => {
            let next = token::next(ctx,stream)?;
            let (term,next) = next_term(ctx,stream,next,1201)?;
            match next {
                Token::Close => (term,next,0),
                _ => return Error::new(ErrorKind::ExpectedToken(Token::Close))
            }
        },
        Token::OpenC => {
            let next = token::next(ctx,stream)?;
            let (term,next) = next_term(ctx,stream,next,1201)?;
            match next {
                Token::CloseC => (Term::new_compound("{}",vec![term]),next,0),
                _ => return Error::new(ErrorKind::ExpectedToken(Token::CloseC))
            }
        },
        Token::OpenL => (list(ctx,stream)?,token::next(ctx,stream)?,0),
        _ => return Error::new(ErrorKind::UnexpectedToken(token))
    };

    /* This is precedence climbing, if you're interested */
    loop {
        let functor: String;
        let (r,l,arity,do_break) = match next {
            Token::Name(ref s) => {
                functor = s.clone();
                lookup_op(ctx,s,max_precedence)
            },
            Token::Bar => {
                /* ISO/IEC 13211-1:1995/Cor.2:2012 */
                functor = '|'.to_string();
                lookup_op(ctx,"|",max_precedence)
            },
            Token::Comma => {
                functor =",".to_string();
                (999,1000,2,1000 > max_precedence)
            },
            _ => return Ok((term,next))
        };

        if do_break || precedence > l {
            return Ok((term,next));
        }

        let mut c = Compound {
            functor,
            args: vec![term]
        };

        next = if arity == 2 {
            let (term,next) = next_term(ctx,stream,next,r)?;
            c.args.push(term);
            next
        } else {
            next
        };

        term = Term::Compound(c);
    }
}

pub(super) fn next(ctx: &Context, stream: &mut dyn Stream) -> Result<Option<Term>,Error> {
    let t = token::next(ctx,stream)?;
    if let Token::Eof = t {
        return Ok(None);
    }

    let (term,next) = next_term(ctx,stream,t,1201)?;
    match next {
        Token::End => Ok(Some(term)),
        _ => Error::new(ErrorKind::ExpectedToken(Token::End))
    }
}

pub(super) fn skip_to_end(ctx: &Context, stream: &mut dyn Stream) -> Result<(),Error> {
    loop {
        match token::next(ctx,stream)? {
            Token::Eof |
            Token::End => break Ok(()),
            _ => {}
        }
    }
}
