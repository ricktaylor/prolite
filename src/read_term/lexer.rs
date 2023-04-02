use super::*;
use error::*;
use stream::{Position, ReadStream, Span};

#[derive(Debug)]
pub(crate) enum Token {
    Eof(Position),
    Name(String, Span),
    Var(String, Span),
    Int(String, u32, Span),
    CharCode(char, Position),
    Float(String, Span),
    DoubleQuotedList(String, Span),
    BackQuotedString(String, Span),
    Open(Position),
    OpenCt(Position),
    Close(Position),
    OpenL(Position),
    CloseL(Position),
    OpenC(Position),
    CloseC(Position),
    Bar(Position),
    Comma(Position),
    End(Position),
}

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Token::Eof(p)
            | Token::Open(p)
            | Token::OpenCt(p)
            | Token::Close(p)
            | Token::OpenL(p)
            | Token::CloseL(p)
            | Token::OpenC(p)
            | Token::CloseC(p)
            | Token::Bar(p)
            | Token::Comma(p)
            | Token::CharCode(_, p)
            | Token::End(p) => Span::from(p),
            Token::Name(_, s)
            | Token::Var(_, s)
            | Token::Int(_, _, s)
            | Token::Float(_, s)
            | Token::DoubleQuotedList(_, s)
            | Token::BackQuotedString(_, s) => s.clone(),
        }
    }
}

enum Char {
    Layout(char),
    Solo(char),
    Meta(char),
    Digit(char),
    Underscore,
    CapitalLetter(char),
    SmallLetter(char),
    Graphic(char),
    Invalid(char),
    Eof,
}

impl From<char> for Char {
    fn from(c: char) -> Self {
        match c {
            ' ' | '\t' | '\n' => Char::Layout(c),
            '!' | '(' | ')' | ',' | ';' | '[' | ']' | '{' | '}' | '|' | '%' => Char::Solo(c),
            '\\' | '\'' | '"' | '`' => Char::Meta(c),
            '0'..='9' => Char::Digit(c),
            '_' => Char::Underscore,
            'A'..='Z' => Char::CapitalLetter(c),
            'a'..='z' => Char::SmallLetter(c),
            '#' | '$' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>' | '?' | '@'
            | '^' | '~' => Char::Graphic(c),
            _ => Char::Invalid(c),
        }
    }
}

fn next_char_raw(stream: &mut dyn ReadStream) -> Result<(Option<char>, Position), Box<Error>> {
    let p = stream.position();
    let c = stream.get()?;
    Ok((c, p))
}

fn peek_char_raw(stream: &mut dyn ReadStream) -> Result<Option<char>, Box<Error>> {
    Ok(stream.peek()?)
}

fn eat_char(stream: &mut dyn ReadStream) -> Result<Position, Box<Error>> {
    let p = stream.position();
    next_char_raw(stream)?;
    Ok(p)
}

fn convert_char(ctx: &Context, c: Option<char>) -> Char {
    if let Some(c) = c {
        match ctx.char_conversion.get(&c) {
            Some(c) => Char::from(*c),
            None => Char::from(c),
        }
    } else {
        Char::Eof
    }
}

fn next_char(ctx: &Context, stream: &mut dyn ReadStream) -> Result<(Char, Position), Box<Error>> {
    let (c, p) = next_char_raw(stream)?;
    Ok((convert_char(ctx, c), p))
}

fn peek_char(ctx: &Context, stream: &mut dyn ReadStream) -> Result<Char, Box<Error>> {
    Ok(convert_char(ctx, peek_char_raw(stream)?))
}

fn multiline_comment(
    ctx: &Context,
    stream: &mut dyn ReadStream,
) -> Result<(Char, Position), Box<Error>> {
    loop {
        match next_char(ctx, stream)? {
            (Char::Eof, p) => return Ok((Char::Eof, p)),
            (Char::Graphic('*'), _) => match next_char(ctx, stream)? {
                (Char::Eof, p) => return Ok((Char::Eof, p)),
                (Char::Graphic('/'), _) => return next_char(ctx, stream),
                _ => {}
            },
            _ => {}
        }
    }
}

fn integral(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    max: char,
    radix: u32,
    start: &Position,
) -> Result<Token, Box<Error>> {
    let mut t = String::new();
    let mut span = Span::from(start);
    loop {
        match peek_char(ctx, stream)? {
            Char::Digit(c) if c <= max => t.push(c),
            Char::SmallLetter(c) if radix == 16 && ('a'..='f').contains(&c) => t.push(c),
            _ => break,
        }
        span.inc(eat_char(stream)?);
    }

    if t.is_empty() {
        match radix {
            2 => Error::new(ErrorKind::BadInteger("0b".to_string()), span),
            8 => Error::new(ErrorKind::BadInteger("0o".to_string()), span),
            _ => Error::new(ErrorKind::BadInteger("0x".to_string()), span),
        }
    } else {
        Ok(Token::Int(t, radix, span))
    }
}

fn numeric(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    c: char,
    start: &Position,
) -> Result<Token, Box<Error>> {
    let mut t = c.to_string();
    let mut span = Span::from(start);
    loop {
        match peek_char(ctx, stream)? {
            Char::Digit(c) => t.push(c),
            Char::Graphic('.') => {
                match peek_char(ctx, stream)? {
                    Char::Solo('%') | Char::Layout(_) | Char::Eof => {
                        return Ok(Token::Int(t, 10, span))
                    }
                    _ => {}
                }
                span.inc(eat_char(stream)?);
                t.push('.');

                match next_char(ctx, stream)? {
                    (Char::Digit(c), _) => t.push(c),
                    (Char::Underscore, p) => {
                        t.push('_');
                        return Error::new(ErrorKind::BadFloat(t), span.add(p));
                    }
                    (Char::Meta(c), p)
                    | (Char::CapitalLetter(c), p)
                    | (Char::SmallLetter(c), p)
                    | (Char::Graphic(c), p)
                    | (Char::Solo(c), p) => {
                        t.push(c);
                        return Error::new(ErrorKind::BadFloat(t), span.add(p));
                    }
                    (_, p) => return Error::new(ErrorKind::BadFloat(t), span.add(p)),
                }

                loop {
                    match peek_char(ctx, stream)? {
                        Char::Digit(c) => t.push(c),
                        Char::CapitalLetter('E') => {
                            eat_char(stream)?;
                            t.push('E');
                            break;
                        }
                        Char::SmallLetter('e') => {
                            eat_char(stream)?;
                            t.push('e');
                            break;
                        }
                        _ => return Ok(Token::Float(t, span)),
                    }
                    span.inc(eat_char(stream)?);
                }

                match next_char(ctx, stream)? {
                    (Char::Graphic('+'), _) => t.push('+'),
                    (Char::Graphic('-'), _) => t.push('-'),
                    (Char::Underscore, p) => {
                        t.push('_');
                        return Error::new(ErrorKind::BadFloat(t), span.add(p));
                    }
                    (Char::Digit(c), p)
                    | (Char::Meta(c), p)
                    | (Char::CapitalLetter(c), p)
                    | (Char::SmallLetter(c), p)
                    | (Char::Graphic(c), p)
                    | (Char::Solo(c), p)
                    | (Char::Layout(c), p) => {
                        t.push(c);
                        return Error::new(ErrorKind::BadFloat(t), span.add(p));
                    }
                    (_, p) => return Error::new(ErrorKind::BadFloat(t), span.add(p)),
                }

                loop {
                    match peek_char(ctx, stream)? {
                        Char::Digit(c) => t.push(c),
                        _ => return Ok(Token::Float(t, span)),
                    }
                    span.inc(eat_char(stream)?);
                }
            }
            _ => return Ok(Token::Int(t, 10, span)),
        }
        span.inc(eat_char(stream)?);
    }
}

fn alpha_numeric(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    c: char,
    start: &Position,
) -> Result<(String, Span), Box<Error>> {
    let mut t = c.to_string();
    let mut span = Span::from(start);
    loop {
        match peek_char(ctx, stream)? {
            Char::Underscore => t.push('_'),
            Char::SmallLetter(c) | Char::CapitalLetter(c) | Char::Digit(c) => t.push(c),
            _ => return Ok((t, span)),
        }
        span.inc(eat_char(stream)?);
    }
}

fn quoted(
    stream: &mut dyn ReadStream,
    quote: char,
    start: &Position,
    greedy: bool,
) -> Result<(String, Span), Box<Error>> {
    let mut t = String::new();
    let mut span = Span::from(start);
    loop {
        match next_char_raw(stream)? {
            (None, p) => return Error::new(ErrorKind::Missing(quote), span.add(p)),
            (Some('\\'), _) => match next_char_raw(stream)? {
                (Some('\n'), _) => {}
                (Some('\\'), _) => t.push('\\'),
                (Some('\''), _) => t.push('\''),
                (Some('"'), _) => t.push('"'),
                (Some('`'), _) => t.push('`'),
                (Some('a'), _) => t.push('\x07'),
                (Some('b'), _) => t.push('\x08'),
                (Some('r'), _) => t.push('\r'),
                (Some('f'), _) => t.push('\x0C'),
                (Some('n'), _) => t.push('\n'),
                (Some('v'), _) => t.push('\x0B'),
                (Some('x'), _) => todo!(),
                (Some(c), _) if ('0'..='7').contains(&c) => {
                    if greedy {}
                    todo!()
                }
                (Some(c), p) => {
                    return Error::new(
                        ErrorKind::BadEscape(String::from_iter(vec!['\\', c])),
                        span.add(p),
                    )
                }
                (None, p) => {
                    return Error::new(ErrorKind::BadEscape('\\'.to_string()), span.add(p))
                }
            },
            (Some(c), p) if c == quote => match peek_char_raw(stream)? {
                Some(c) if c == quote => {
                    t.push(c);
                    eat_char(stream)?;
                }
                _ => return Ok((t, span.add(p))),
            },
            (Some(c), _) => t.push(c),
        }
    }
}

fn graphic(ctx: &Context,
    stream: &mut dyn ReadStream,
    c: char,
    p: Position
) -> Result<Token, Box<Error>> {
    let mut t = c.to_string();
    let mut span = Span::from(p);
    loop {
        match peek_char(ctx, stream)? {
            Char::Graphic(c) => t.push(c),
            Char::Meta('\\') => t.push('\\'),
            _ => return Ok(Token::Name(t, span)),
        }
        span.inc(eat_char(stream)?);
    }
}

pub(super) fn next(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    greedy: bool,
) -> Result<Token, Box<Error>> {
    let mut c = next_char(ctx, stream)?;

    // open ct (* 6.4 *)
    if let (Char::Solo('('), p) = c {
        return Ok(Token::OpenCt(p));
    }

    loop {
        c = match c {
            (Char::Eof, p) => return Ok(Token::Eof(p)),

            // layout text sequence (* 6.4.1 *)
            (Char::Layout(_), _) => next_char(ctx, stream)?,

            // single line comment (* 6.4.1 *)
            (Char::Solo('%'), _) => loop {
                match next_char(ctx, stream)? {
                    (Char::Eof, p) => return Ok(Token::Eof(p)),
                    (Char::Layout('\n'), _) => break next_char(ctx, stream)?,
                    _ => {}
                }
            },

            // name (* 6.4 *)
            // letter digit token (* 6.4.2 *)
            (Char::SmallLetter(c), p) => {
                let (s, span) = alpha_numeric(ctx, stream, c, &p)?;
                return Ok(Token::Name(s, span));
            }

            // graphic token (* 6.4.2 *)
            (Char::Graphic('.'), p) => match peek_char(ctx, stream)? {
                Char::Solo('%') | Char::Layout(_) | Char::Eof => return Ok(Token::End(p)),
                _ => return graphic(ctx,stream,'.',p)
            }

            // bracketed comment (* 6.4.1 *)
            (Char::Graphic('/'), p) => match peek_char(ctx, stream)? {
                Char::Graphic('*') => {
                    eat_char(stream)?;
                    multiline_comment(ctx, stream)?
                }
                _ => return graphic(ctx,stream,'/',p)
            }
            (Char::Meta('\\'),p) => return graphic(ctx,stream,'\\',p),
            (Char::Graphic(c), p) => return graphic(ctx,stream,c,p),

            // quoted token (* 6.4.2 *)
            (Char::Meta('\''), p) => {
                let (s, span) = quoted(stream, '\'', &p, greedy)?;
                return Ok(Token::Name(s, span));
            }

            // semicolon token (* 6.4.2 *)
            (Char::Solo(';'), p) => return Ok(Token::Name(String::from(';'), Span::from(p))),

            // cut token (* 6.4.2 *)
            (Char::Solo('!'), p) => return Ok(Token::Name(String::from('!'), Span::from(p))),

            // variable (* 6.4 *)
            (Char::Underscore, p) => {
                let (s, span) = alpha_numeric(ctx, stream, '_', &p)?;
                return Ok(Token::Var(s, span));
            }
            (Char::CapitalLetter(c), p) => {
                let (s, span) = alpha_numeric(ctx, stream, c, &p)?;
                return Ok(Token::Var(s, span));
            }

            // integer (* 6.4 *)
            // float number (* 6.4 *)
            (Char::Digit('0'), p) => match peek_char(ctx, stream)? {
                Char::Meta('\'') => todo!(),
                Char::SmallLetter('b') => {
                    eat_char(stream)?;
                    return integral(ctx, stream, '1', 2, &p);
                }
                Char::SmallLetter('o') => {
                    eat_char(stream)?;
                    return integral(ctx, stream, '7', 8, &p);
                }
                Char::SmallLetter('x') => {
                    eat_char(stream)?;
                    return integral(ctx, stream, '9', 16, &p);
                }
                Char::Digit(c) => {
                    eat_char(stream)?;
                    return numeric(ctx, stream, c, &p);
                }
                _ => return Ok(Token::Int('0'.to_string(), 10, p.into())),
            },
            (Char::Digit(c), p) => return numeric(ctx, stream, c, &p),

            // double quoted list (* 6.4 *)
            (Char::Meta('"'), p) => {
                let (s, span) = quoted(stream, '"', &p, greedy)?;
                return Ok(Token::DoubleQuotedList(s, span));
            }

            // back quoted string (* 6.4.7 *)
            (Char::Meta('`'), p) => {
                let (s, span) = quoted(stream, '`', &p, greedy)?;
                return Ok(Token::BackQuotedString(s, span));
            }

            // open (* 6.4 *)
            (Char::Solo('('), p) => return Ok(Token::Open(p)),

            // close (* 6.4 *)
            (Char::Solo(')'), p) => return Ok(Token::Close(p)),

            // open list (* 6.4 *)
            (Char::Solo('['), p) => return Ok(Token::OpenL(p)),

            // close list (* 6.4 *)
            (Char::Solo(']'), p) => return Ok(Token::CloseL(p)),

            // open curly (* 6.4 *)
            (Char::Solo('{'), p) => return Ok(Token::OpenC(p)),

            // close curly (* 6.4 *)
            (Char::Solo('}'), p) => return Ok(Token::CloseC(p)),

            // ht sep (* 6.4 *)
            (Char::Solo('|'), p) => return Ok(Token::Bar(p)),

            // comma (* 6.4 *)
            (Char::Solo(','), p) => return Ok(Token::Comma(p)),

            (Char::Solo(c), p) | (Char::Meta(c), p) | (Char::Invalid(c), p) => {
                let mut s = c.to_string();
                if greedy {
                    while let Char::Invalid(c) = peek_char(ctx, stream)? {
                        s.push(c);
                        eat_char(stream)?;
                    }
                }
                return Error::new(ErrorKind::Unexpected(s), p.into());
            }
        }
    }
}
