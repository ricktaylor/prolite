use super::*;
use error::*;
use stream::{Position, ReadStream, Span};

#[derive(Debug)]
pub(crate) enum TokenKind {
    Eof,
    Name(String),
    Var(String),
    Int(String, u32),
    CharCode(char),
    Exponent(String),
    DoubleQuotedList(String),
    BackQuotedString(String),
    Open,
    OpenCt,
    Close,
    OpenL,
    CloseL,
    OpenC,
    CloseC,
    Bar,
    Comma,
    End,
}

#[derive(Debug)]
pub(crate) struct Token {
    pub kind: TokenKind,
    pub location: Span,
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
            (Char::Eof, p) => break Ok((Char::Eof, p)),
            (Char::Graphic('*'), _) => match next_char(ctx, stream)? {
                (Char::Eof, p) => break Ok((Char::Eof, p)),
                (Char::Graphic('/'), _) => break next_char(ctx, stream),
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
    start: Position,
) -> Result<Token, Box<Error>> {
    let mut t = String::new();
    let mut span = Span::from(start);
    loop {
        match peek_char(ctx, stream)? {
            Char::Digit(c) if c <= max => t.push(c),
            Char::SmallLetter(c @ 'a'..='f') if radix == 16 => t.push(c),
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
        Ok(Token {
            kind: TokenKind::Int(t, radix),
            location: span,
        })
    }
}

fn numeric(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    c: char,
    start: Position,
) -> Result<Token, Box<Error>> {
    let mut t = c.to_string();
    let mut span = Span::from(start);
    loop {
        match peek_char(ctx, stream)? {
            Char::Digit(c) => t.push(c),
            _ => {
                break Ok(Token {
                    kind: TokenKind::Int(t, 10),
                    location: span,
                })
            }
        }
        span.inc(eat_char(stream)?);
    }
}

fn alpha_numeric(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    c: char,
    start: Position,
) -> Result<(String, Span), Box<Error>> {
    let mut t = c.to_string();
    let mut span = Span::from(start);
    loop {
        match peek_char(ctx, stream)? {
            Char::Underscore => t.push('_'),
            Char::SmallLetter(c) | Char::CapitalLetter(c) | Char::Digit(c) => t.push(c),
            _ => break Ok((t, span)),
        }
        span.inc(eat_char(stream)?);
    }
}

fn char_code(
    stream: &mut dyn ReadStream,
    init_c: char,
    start: Position,
    greedy: bool,
) -> Result<(char, Position), Box<Error>> {
    let mut o = String::new();
    if init_c != 'x' {
        o.push(init_c);
    }

    let mut span = Span::from(start);
    loop {
        match next_char_raw(stream)? {
            (None, p) => return Error::new(ErrorKind::Missing('\\'), span.add(p)),
            (Some('\\'), p) => match u32::from_str_radix(&o, if init_c == 'x' { 16 } else { 8 }) {
                Ok(u) => match char::from_u32(u) {
                    None => {
                        o.push('\\');
                        break;
                    }
                    Some(c) => return Ok((c, p)),
                },
                Err(_) => {
                    o.push('\\');
                    break;
                }
            },
            (Some(c @ 'A'..='F'), _) | (Some(c @ 'a'..='f'), _) | (Some(c @ '8'..='9'), _)
                if init_c == 'x' =>
            {
                o.push(c)
            }
            (Some(c @ '0'..='7'), _) => o.push(c),
            (Some(c), p) => {
                o.push(c);
                span.inc(p);
                if greedy {
                    loop {
                        match next_char_raw(stream) {
                            Err(_) | Ok((None, _)) => break,
                            Ok((Some(c), _)) => {
                                o.push(c);
                                if c == '\\' {
                                    break;
                                }
                            }
                        }
                    }
                }
                break;
            }
        }
    }

    if init_c == 'x' {
        o = "\\x".to_string() + &o;
    } else {
        o = "\\".to_string() + &o;
    }
    Error::new(ErrorKind::BadEscape(o), span)
}

fn quoted_inner<const Q: char>(
    stream: &mut dyn ReadStream,
    start: Position,
    greedy: bool,
) -> Result<(String, Span), Box<Error>> {
    let mut t = String::new();
    let mut span = Span::from(start);
    loop {
        match next_char_raw(stream)? {
            (None, p) => return Error::new(ErrorKind::Missing(Q), span.add(p)),
            (Some('\\'), p) => match next_char_raw(stream)? {
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
                (Some(c @ '0'..='7'), _) | (Some(c @ 'x'), _) => {
                    let (c, p) = char_code(stream, c, p, greedy)?;
                    t.push(c);
                    span.inc(p);
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
            (Some(c), p) if c == Q => match peek_char_raw(stream)? {
                Some(c) if c == Q => {
                    t.push(c);
                    eat_char(stream)?;
                }
                _ => return Ok((t, span.add(p))),
            },
            (Some(c), _) => t.push(c),
        }
    }
}

fn quoted<const Q: char>(
    stream: &mut dyn ReadStream,
    start: Position,
    greedy: bool,
) -> Result<(String, Span), Box<Error>> {
    let r = quoted_inner::<Q>(stream, start, greedy);
    if r.is_err() && greedy {
        loop {
            match next_char_raw(stream) {
                Err(_) | Ok((None, _)) | Ok((Some('\\'), _)) => break,
                _ => {}
            }
        }
    }
    r
}

fn exponent(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    c: char,
    start: Position,
) -> Result<Token, Box<Error>> {
    let mut t = String::from_iter(vec!['.', c]);
    let mut span = Span::from(start);
    span.inc(eat_char(stream)?);

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
            _ => {
                return Ok(Token {
                    kind: TokenKind::Exponent(t),
                    location: span,
                })
            }
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
            _ => {
                break Ok(Token {
                    kind: TokenKind::Exponent(t),
                    location: span,
                })
            }
        }
        span.inc(eat_char(stream)?);
    }
}

fn graphic(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    c: char,
    p: Position,
) -> Result<Token, Box<Error>> {
    let mut t = c.to_string();
    let mut span = Span::from(p);
    loop {
        match peek_char(ctx, stream)? {
            Char::Graphic(c) => t.push(c),
            Char::Meta('\\') => t.push('\\'),
            _ => {
                break Ok(Token {
                    kind: TokenKind::Name(t),
                    location: span,
                })
            }
        }
        span.inc(eat_char(stream)?);
    }
}

pub(super) fn next(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    exp: bool,
    greedy: bool,
) -> Result<Token, Box<Error>> {
    let mut c = next_char(ctx, stream)?;
    match c {
        // open ct (* 6.4 *)
        (Char::Solo('('), p) => {
            return Ok(Token {
                kind: TokenKind::OpenCt,
                location: Span::from(p),
            })
        }

        // end or .exponent
        (Char::Graphic('.'), p) if exp => match peek_char(ctx, stream)? {
            Char::Digit(c) => return exponent(ctx, stream, c, p),
            _ => {
                return Ok(Token {
                    kind: TokenKind::End,
                    location: Span::from(p),
                })
            }
        },
        _ => {}
    }

    loop {
        c = match c {
            (Char::Eof, p) => {
                return Ok(Token {
                    kind: TokenKind::Eof,
                    location: Span::from(p),
                })
            }

            // layout text sequence (* 6.4.1 *)
            (Char::Layout(_), _) => next_char(ctx, stream)?,

            // single line comment (* 6.4.1 *)
            (Char::Solo('%'), _) => loop {
                match next_char(ctx, stream)? {
                    (Char::Eof, p) => {
                        return Ok(Token {
                            kind: TokenKind::Eof,
                            location: Span::from(p),
                        })
                    }
                    (Char::Layout('\n'), _) => break next_char(ctx, stream)?,
                    _ => {}
                }
            },

            // name (* 6.4 *)
            // letter digit token (* 6.4.2 *)
            (Char::SmallLetter(c), p) => {
                let (s, span) = alpha_numeric(ctx, stream, c, p)?;
                return Ok(Token {
                    kind: TokenKind::Name(s),
                    location: span,
                });
            }

            // graphic token (* 6.4.2 *)
            (Char::Graphic('.'), p) => match peek_char(ctx, stream)? {
                Char::Solo('%') | Char::Layout(_) | Char::Eof => {
                    return Ok(Token {
                        kind: TokenKind::End,
                        location: Span::from(p),
                    })
                }
                _ => return graphic(ctx, stream, '.', p),
            },

            // bracketed comment (* 6.4.1 *)
            (Char::Graphic('/'), p) => match peek_char(ctx, stream)? {
                Char::Graphic('*') => {
                    eat_char(stream)?;
                    multiline_comment(ctx, stream)?
                }
                _ => return graphic(ctx, stream, '/', p),
            },
            (Char::Meta('\\'), p) => return graphic(ctx, stream, '\\', p),
            (Char::Graphic(c), p) => return graphic(ctx, stream, c, p),

            // quoted token (* 6.4.2 *)
            (Char::Meta('\''), p) => {
                let (s, span) = quoted::<'\''>(stream, p, greedy)?;
                return Ok(Token {
                    kind: TokenKind::Name(s),
                    location: span,
                });
            }

            // semicolon token (* 6.4.2 *)
            (Char::Solo(';'), p) => {
                return Ok(Token {
                    kind: TokenKind::Name(String::from(';')),
                    location: Span::from(p),
                })
            }

            // cut token (* 6.4.2 *)
            (Char::Solo('!'), p) => {
                return Ok(Token {
                    kind: TokenKind::Name(String::from('!')),
                    location: Span::from(p),
                })
            }

            // variable (* 6.4 *)
            (Char::Underscore, p) => {
                let (s, span) = alpha_numeric(ctx, stream, '_', p)?;
                return Ok(Token {
                    kind: TokenKind::Var(s),
                    location: span,
                });
            }
            (Char::CapitalLetter(c), p) => {
                let (s, span) = alpha_numeric(ctx, stream, c, p)?;
                return Ok(Token {
                    kind: TokenKind::Var(s),
                    location: span,
                });
            }

            // integer (* 6.4 *)
            // float number (* 6.4 *)
            (Char::Digit('0'), p) => match peek_char(ctx, stream)? {
                Char::Meta('\'') => {
                    eat_char(stream)?;
                    return match next_char_raw(stream)? {
                        (None, q) => {
                            Error::new(ErrorKind::BadEscape("0\'".to_string()), Span::new(p, q))
                        }
                        (Some('\''), _) => match next_char_raw(stream)? {
                            (None, q) => Error::new(
                                ErrorKind::BadEscape("0\'\'".to_string()),
                                Span::new(p, q),
                            ),
                            (Some('\''), q) => Ok(Token {
                                kind: TokenKind::Int("39".to_string(), 10),
                                location: Span::new(p, q),
                            }),
                            (Some(c), q) => Error::new(
                                ErrorKind::BadEscape(String::from_iter(vec!['0', '\'', '\'', c])),
                                Span::new(p, q),
                            ),
                        },
                        (Some(c), q) => Ok(Token {
                            kind: TokenKind::Int((c as u32).to_string(), 10),
                            location: Span::new(p, q),
                        }),
                    };
                }
                Char::SmallLetter('b') => {
                    eat_char(stream)?;
                    return integral(ctx, stream, '1', 2, p);
                }
                Char::SmallLetter('o') => {
                    eat_char(stream)?;
                    return integral(ctx, stream, '7', 8, p);
                }
                Char::SmallLetter('x') => {
                    eat_char(stream)?;
                    return integral(ctx, stream, '9', 16, p);
                }
                Char::Digit(c) => {
                    eat_char(stream)?;
                    return numeric(ctx, stream, c, p);
                }
                _ => {
                    return Ok(Token {
                        kind: TokenKind::Int("0".to_string(), 10),
                        location: Span::from(p),
                    })
                }
            },
            (Char::Digit(c), p) => return numeric(ctx, stream, c, p),

            // double quoted list (* 6.4 *)
            (Char::Meta('"'), p) => {
                let (s, span) = quoted::<'"'>(stream, p, greedy)?;
                return Ok(Token {
                    kind: TokenKind::DoubleQuotedList(s),
                    location: span,
                });
            }

            // back quoted string (* 6.4.7 *)
            (Char::Meta('`'), p) => {
                let (s, span) = quoted::<'`'>(stream, p, greedy)?;
                return Ok(Token {
                    kind: TokenKind::BackQuotedString(s),
                    location: span,
                });
            }

            // open (* 6.4 *)
            (Char::Solo('('), p) => {
                return Ok(Token {
                    kind: TokenKind::Open,
                    location: Span::from(p),
                })
            }

            // close (* 6.4 *)
            (Char::Solo(')'), p) => {
                return Ok(Token {
                    kind: TokenKind::Close,
                    location: Span::from(p),
                })
            }

            // open list (* 6.4 *)
            (Char::Solo('['), p) => {
                return Ok(Token {
                    kind: TokenKind::OpenL,
                    location: Span::from(p),
                })
            }

            // close list (* 6.4 *)
            (Char::Solo(']'), p) => {
                return Ok(Token {
                    kind: TokenKind::CloseL,
                    location: Span::from(p),
                })
            }

            // open curly (* 6.4 *)
            (Char::Solo('{'), p) => {
                return Ok(Token {
                    kind: TokenKind::OpenC,
                    location: Span::from(p),
                })
            }

            // close curly (* 6.4 *)
            (Char::Solo('}'), p) => {
                return Ok(Token {
                    kind: TokenKind::CloseC,
                    location: Span::from(p),
                })
            }

            // ht sep (* 6.4 *)
            (Char::Solo('|'), p) => {
                return Ok(Token {
                    kind: TokenKind::Bar,
                    location: Span::from(p),
                })
            }

            // comma (* 6.4 *)
            (Char::Solo(','), p) => {
                return Ok(Token {
                    kind: TokenKind::Comma,
                    location: Span::from(p),
                })
            }

            (Char::Solo(c), p) | (Char::Meta(c), p) | (Char::Invalid(c), p) => {
                let mut s = c.to_string();
                if greedy {
                    while let Char::Invalid(c) = peek_char(ctx, stream)? {
                        s.push(c);
                        eat_char(stream)?;
                    }
                }
                return Error::new(ErrorKind::Unexpected(s), Span::from(p));
            }
        }
    }
}
