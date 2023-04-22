use super::*;
use error::*;
use stream::{Position, ReadStream, Span};

#[derive(Debug)]
pub(crate) enum TokenKind {
    Eof,
    Name(String),
    Var(String),
    Int(String, u32),
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

fn map_char(c: char) -> Char {
    match c {
        ' ' | '\t' | '\n' => Char::Layout(c),
        '!' | '(' | ')' | ',' | ';' | '[' | ']' | '{' | '}' | '|' | '%' => Char::Solo(c),
        '\\' | '\'' | '"' | '`' => Char::Meta(c),
        '0'..='9' => Char::Digit(c),
        '_' => Char::Underscore,
        'A'..='Z' => Char::CapitalLetter(c),
        'a'..='z' => Char::SmallLetter(c),
        '#' | '$' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^'
        | '~' => Char::Graphic(c),
        _ => Char::Invalid(c),
    }
}

fn next_char_raw(stream: &mut dyn ReadStream) -> Result<(Option<char>, Span), Box<Error>> {
    let p = stream.position();
    match stream.get().map_err(|e| {
        Box::new(Error {
            kind: ErrorKind::StreamError(e),
            location: Span::new(stream.position(), None),
        })
    })? {
        Some(c) => Ok((Some(c), Span::new(p, Some(stream.position())))),
        None => Ok((None, Span::new(p, None))),
    }
}

fn peek_char_raw(stream: &mut dyn ReadStream) -> Result<Option<char>, Box<Error>> {
    stream.peek().map_err(|e| {
        Box::new(Error {
            kind: ErrorKind::StreamError(e),
            location: Span::new(stream.position(), None),
        })
    })
}

fn eat_char(stream: &mut dyn ReadStream) -> Result<Position, Box<Error>> {
    next_char_raw(stream)?;
    Ok(stream.position())
}

fn convert_char(ctx: &Context, c: Option<char>) -> Char {
    c.map_or(Char::Eof, |c| {
        if ctx.flags.char_conversion {
            ctx.char_conversion
                .get(&c)
                .map_or_else(|| map_char(c), |c| map_char(*c))
        } else {
            map_char(c)
        }
    })
}

fn next_char(ctx: &Context, stream: &mut dyn ReadStream) -> Result<(Char, Span), Box<Error>> {
    let (c, l) = next_char_raw(stream)?;
    Ok((convert_char(ctx, c), l))
}

fn peek_char(ctx: &Context, stream: &mut dyn ReadStream) -> Result<Char, Box<Error>> {
    Ok(convert_char(ctx, peek_char_raw(stream)?))
}

fn multiline_comment(
    ctx: &Context,
    stream: &mut dyn ReadStream,
) -> Result<(Char, Span), Box<Error>> {
    loop {
        match next_char(ctx, stream)? {
            (Char::Eof, location) => break Ok((Char::Eof, location)),
            (Char::Graphic('*'), _) => match next_char(ctx, stream)? {
                (Char::Eof, location) => break Ok((Char::Eof, location)),
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
    mut location: Span,
) -> Result<Token, Box<Error>> {
    let mut t = String::new();
    loop {
        match peek_char(ctx, stream)? {
            Char::Digit(c) if c <= max => t.push(c),
            Char::SmallLetter(c @ 'a'..='f') if radix == 16 => t.push(c),
            _ => break,
        }
        location.inc(eat_char(stream)?);
    }

    if t.is_empty() {
        match radix {
            2 => Error::new(ErrorKind::BadInteger("0b".to_string()), location),
            8 => Error::new(ErrorKind::BadInteger("0o".to_string()), location),
            _ => Error::new(ErrorKind::BadInteger("0x".to_string()), location),
        }
    } else {
        Ok(Token {
            kind: TokenKind::Int(t, radix),
            location,
        })
    }
}

fn numeric(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    c: char,
    mut location: Span,
) -> Result<Token, Box<Error>> {
    let mut t = c.to_string();
    loop {
        match peek_char(ctx, stream)? {
            Char::Digit(c) => t.push(c),
            _ => {
                break Ok(Token {
                    kind: TokenKind::Int(t, 10),
                    location,
                })
            }
        }
        location.inc(eat_char(stream)?);
    }
}

fn alpha_numeric(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    c: char,
    mut location: Span,
) -> Result<(String, Span), Box<Error>> {
    let mut t = c.to_string();
    loop {
        match peek_char(ctx, stream)? {
            Char::Underscore => t.push('_'),
            Char::SmallLetter(c) | Char::CapitalLetter(c) | Char::Digit(c) => t.push(c),
            _ => break Ok((t, location)),
        }
        location.inc(eat_char(stream)?);
    }
}

fn char_code(
    stream: &mut dyn ReadStream,
    init_c: char,
    mut location: Span,
    greedy: bool,
) -> Result<(char, Span), Box<Error>> {
    let mut o = String::new();
    if init_c != 'x' {
        o.push(init_c);
    }

    loop {
        match next_char_raw(stream)? {
            (None, l) => return Error::new(ErrorKind::Missing('\\'), location.join(l)),
            (Some('\\'), l) => match u32::from_str_radix(&o, if init_c == 'x' { 16 } else { 8 }) {
                Ok(u) => match char::from_u32(u) {
                    None => {
                        o.push('\\');
                        break;
                    }
                    Some(c) => return Ok((c, location.join(l))),
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
            (Some(c), l) => {
                o.push(c);
                location.append(l);
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
    Error::new(ErrorKind::BadEscape(o), location)
}

fn quoted_inner<const Q: char>(
    stream: &mut dyn ReadStream,
    mut location: Span,
    greedy: bool,
) -> Result<(String, Span), Box<Error>> {
    let mut t = String::new();
    loop {
        match next_char_raw(stream)? {
            (None, l) => return Error::new(ErrorKind::Missing(Q), location.join(l)),
            (Some('\\'), l) => match next_char_raw(stream)? {
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
                    let (c, l) = char_code(stream, c, l, greedy)?;
                    t.push(c);
                    location.append(l);
                }
                (Some(c), l) => {
                    return Error::new(
                        ErrorKind::BadEscape(String::from_iter(vec!['\\', c])),
                        location.join(l),
                    )
                }
                (None, l) => {
                    return Error::new(ErrorKind::BadEscape('\\'.to_string()), location.join(l))
                }
            },
            (Some(c), l) if c == Q => match peek_char_raw(stream)? {
                Some(c) if c == Q => {
                    t.push(c);
                    eat_char(stream)?;
                }
                _ => return Ok((t, location.join(l))),
            },
            (Some(c), _) => t.push(c),
        }
    }
}

fn quoted<const Q: char>(
    stream: &mut dyn ReadStream,
    location: Span,
    greedy: bool,
) -> Result<(String, Span), Box<Error>> {
    let r = quoted_inner::<Q>(stream, location, greedy);
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
    mut location: Span,
) -> Result<Token, Box<Error>> {
    let mut t = String::from_iter(vec!['.', c]);
    location.inc(eat_char(stream)?);

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
                    location,
                })
            }
        }
        location.inc(eat_char(stream)?);
    }

    match next_char(ctx, stream)? {
        (Char::Graphic('+'), _) => t.push('+'),
        (Char::Graphic('-'), _) => t.push('-'),
        (Char::Underscore, l) => {
            t.push('_');
            return Error::new(ErrorKind::BadFloat(t), location.join(l));
        }
        (Char::Digit(c), l)
        | (Char::Meta(c), l)
        | (Char::CapitalLetter(c), l)
        | (Char::SmallLetter(c), l)
        | (Char::Graphic(c), l)
        | (Char::Solo(c), l)
        | (Char::Layout(c), l) => {
            t.push(c);
            return Error::new(ErrorKind::BadFloat(t), location.join(l));
        }
        (_, l) => return Error::new(ErrorKind::BadFloat(t), location.join(l)),
    }

    loop {
        match peek_char(ctx, stream)? {
            Char::Digit(c) => t.push(c),
            _ => {
                break Ok(Token {
                    kind: TokenKind::Exponent(t),
                    location,
                })
            }
        }
        location.inc(eat_char(stream)?);
    }
}

fn graphic(
    ctx: &Context,
    stream: &mut dyn ReadStream,
    c: char,
    mut location: Span,
) -> Result<Token, Box<Error>> {
    let mut t = c.to_string();
    loop {
        match peek_char(ctx, stream)? {
            Char::Graphic(c) => t.push(c),
            Char::Meta('\\') => t.push('\\'),
            _ => {
                break Ok(Token {
                    kind: TokenKind::Name(t),
                    location,
                })
            }
        }
        location.inc(eat_char(stream)?);
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
        (Char::Solo('('), location) => {
            return Ok(Token {
                kind: TokenKind::OpenCt,
                location,
            })
        }

        // end or .exponent
        (Char::Graphic('.'), location) if exp => match peek_char(ctx, stream)? {
            Char::Digit(c) => return exponent(ctx, stream, c, location),
            _ => {
                return Ok(Token {
                    kind: TokenKind::End,
                    location,
                })
            }
        },
        _ => {}
    }

    loop {
        c = match c {
            (Char::Eof, location) => {
                return Ok(Token {
                    kind: TokenKind::Eof,
                    location,
                })
            }

            // layout text sequence (* 6.4.1 *)
            (Char::Layout(_), _) => next_char(ctx, stream)?,

            // single line comment (* 6.4.1 *)
            (Char::Solo('%'), _) => loop {
                match next_char(ctx, stream)? {
                    (Char::Eof, location) => {
                        return Ok(Token {
                            kind: TokenKind::Eof,
                            location,
                        })
                    }
                    (Char::Layout('\n'), _) => break next_char(ctx, stream)?,
                    _ => {}
                }
            },

            // name (* 6.4 *)
            // letter digit token (* 6.4.2 *)
            (Char::SmallLetter(c), location) => {
                let (s, location) = alpha_numeric(ctx, stream, c, location)?;
                return Ok(Token {
                    kind: TokenKind::Name(s),
                    location,
                });
            }

            // graphic token (* 6.4.2 *)
            (Char::Graphic('.'), location) => match peek_char(ctx, stream)? {
                Char::Solo('%') | Char::Layout(_) | Char::Eof => {
                    return Ok(Token {
                        kind: TokenKind::End,
                        location,
                    })
                }
                _ => return graphic(ctx, stream, '.', location),
            },

            // bracketed comment (* 6.4.1 *)
            (Char::Graphic('/'), location) => match peek_char(ctx, stream)? {
                Char::Graphic('*') => {
                    eat_char(stream)?;
                    multiline_comment(ctx, stream)?
                }
                _ => return graphic(ctx, stream, '/', location),
            },
            (Char::Meta('\\'), location) => return graphic(ctx, stream, '\\', location),
            (Char::Graphic(c), location) => return graphic(ctx, stream, c, location),

            // quoted token (* 6.4.2 *)
            (Char::Meta('\''), location) => {
                let (s, location) = quoted::<'\''>(stream, location, greedy)?;
                return Ok(Token {
                    kind: TokenKind::Name(s),
                    location,
                });
            }

            // semicolon token (* 6.4.2 *)
            (Char::Solo(';'), location) => {
                return Ok(Token {
                    kind: TokenKind::Name(String::from(';')),
                    location,
                })
            }

            // cut token (* 6.4.2 *)
            (Char::Solo('!'), location) => {
                return Ok(Token {
                    kind: TokenKind::Name(String::from('!')),
                    location,
                })
            }

            // variable (* 6.4 *)
            (Char::Underscore, location) => {
                let (s, location) = alpha_numeric(ctx, stream, '_', location)?;
                return Ok(Token {
                    kind: TokenKind::Var(s),
                    location,
                });
            }
            (Char::CapitalLetter(c), location) => {
                let (s, location) = alpha_numeric(ctx, stream, c, location)?;
                return Ok(Token {
                    kind: TokenKind::Var(s),
                    location,
                });
            }

            // integer (* 6.4 *)
            // float number (* 6.4 *)
            (Char::Digit('0'), location) => match peek_char(ctx, stream)? {
                Char::Meta('\'') => {
                    eat_char(stream)?;
                    return match next_char_raw(stream)? {
                        (None, l) => {
                            Error::new(ErrorKind::BadEscape("0\'".to_string()), location.join(l))
                        }
                        (Some('\''), _) => match next_char_raw(stream)? {
                            (None, l) => Error::new(
                                ErrorKind::BadEscape("0\'\'".to_string()),
                                location.join(l),
                            ),
                            (Some('\''), l) => Ok(Token {
                                kind: TokenKind::Int("39".to_string(), 10),
                                location: location.join(l),
                            }),
                            (Some(c), l) => Error::new(
                                ErrorKind::BadEscape(String::from_iter(vec!['0', '\'', '\'', c])),
                                location.join(l),
                            ),
                        },
                        (Some(c), l) => Ok(Token {
                            kind: TokenKind::Int((c as u32).to_string(), 10),
                            location: location.join(l),
                        }),
                    };
                }
                Char::SmallLetter('b') => {
                    eat_char(stream)?;
                    return integral(ctx, stream, '1', 2, location);
                }
                Char::SmallLetter('o') => {
                    eat_char(stream)?;
                    return integral(ctx, stream, '7', 8, location);
                }
                Char::SmallLetter('x') => {
                    eat_char(stream)?;
                    return integral(ctx, stream, '9', 16, location);
                }
                Char::Digit(c) => {
                    eat_char(stream)?;
                    return numeric(ctx, stream, c, location);
                }
                _ => {
                    return Ok(Token {
                        kind: TokenKind::Int("0".to_string(), 10),
                        location,
                    })
                }
            },
            (Char::Digit(c), location) => return numeric(ctx, stream, c, location),

            // double quoted list (* 6.4 *)
            (Char::Meta('"'), location) => {
                let (s, location) = quoted::<'"'>(stream, location, greedy)?;
                return Ok(Token {
                    kind: TokenKind::DoubleQuotedList(s),
                    location,
                });
            }

            // back quoted string (* 6.4.7 *)
            (Char::Meta('`'), location) => {
                let (s, location) = quoted::<'`'>(stream, location, greedy)?;
                return Ok(Token {
                    kind: TokenKind::BackQuotedString(s),
                    location,
                });
            }

            // open (* 6.4 *)
            (Char::Solo('('), location) => {
                return Ok(Token {
                    kind: TokenKind::Open,
                    location,
                })
            }

            // close (* 6.4 *)
            (Char::Solo(')'), location) => {
                return Ok(Token {
                    kind: TokenKind::Close,
                    location,
                })
            }

            // open list (* 6.4 *)
            (Char::Solo('['), location) => {
                return Ok(Token {
                    kind: TokenKind::OpenL,
                    location,
                })
            }

            // close list (* 6.4 *)
            (Char::Solo(']'), location) => {
                return Ok(Token {
                    kind: TokenKind::CloseL,
                    location,
                })
            }

            // open curly (* 6.4 *)
            (Char::Solo('{'), location) => {
                return Ok(Token {
                    kind: TokenKind::OpenC,
                    location,
                })
            }

            // close curly (* 6.4 *)
            (Char::Solo('}'), location) => {
                return Ok(Token {
                    kind: TokenKind::CloseC,
                    location,
                })
            }

            // ht sep (* 6.4 *)
            (Char::Solo('|'), location) => {
                return Ok(Token {
                    kind: TokenKind::Bar,
                    location,
                })
            }

            // comma (* 6.4 *)
            (Char::Solo(','), location) => {
                return Ok(Token {
                    kind: TokenKind::Comma,
                    location,
                })
            }

            (Char::Solo(c), location)
            | (Char::Meta(c), location)
            | (Char::Invalid(c), location) => {
                let mut s = c.to_string();
                if greedy {
                    while let Char::Invalid(c) = peek_char(ctx, stream)? {
                        s.push(c);
                        eat_char(stream)?;
                    }
                }
                return Error::new(ErrorKind::Unexpected(s), location);
            }
        }
    }
}
