use super::*;

pub struct Utf8Reader<R: std::io::Read> {
    reader: R,
    next_char: Option<char>,
    next_byte: Option<u8>,
    position: Option<stream::Position>,
}

impl<R: std::io::Read> Utf8Reader<R> {
    const REPLACEMENT_CHAR: char = '\u{FFFD}';

    pub fn new(reader: R, source: &str) -> Self {
        Self {
            reader,
            next_char: None,
            next_byte: None,
            position: Some(stream::Position {
                source: source.to_string(),
                line: 1,
                column: 1,
            }),
        }
    }

    fn from_utf8(i: u32) -> char {
        char::from_u32(i).unwrap_or(Self::REPLACEMENT_CHAR)
    }

    fn peek_byte(&mut self) -> Result<Option<u8>, std::io::Error> {
        if self.next_byte.is_none() {
            let mut buf = [0u8];
            self.next_byte = match self.reader.read(&mut buf)? {
                1 => Some(buf[0]),
                _ => None,
            }
        }
        Ok(self.next_byte)
    }

    fn continuation(&mut self, count: usize, val: u32) -> Result<Option<char>, std::io::Error> {
        let mut val = val;
        for _ in 1..count {
            self.next_byte = None;
            match self.peek_byte()? {
                Some(b @ 0x80..=0xBF) => val = (val << 6) | (b & 0x3F) as u32,
                _ => return Ok(Some(Self::REPLACEMENT_CHAR)),
            }
        }
        Ok(Some(Self::from_utf8(val)))
    }
}

impl<R: std::io::Read> stream::ReadStream for Utf8Reader<R> {
    fn get(&mut self) -> Result<Option<char>, std::io::Error> {
        let r = self.peek()?;
        match (r, &mut self.position) {
            (Some('\n'), Some(p)) => {
                p.line += 1;
                p.column = 1;
            }
            (Some(_), Some(p)) => p.column += 1,
            _ => {}
        }
        self.next_byte = None;
        self.next_char = None;
        Ok(r)
    }

    fn peek(&mut self) -> Result<Option<char>, std::io::Error> {
        if self.next_char.is_none() {
            self.next_char = match self.peek_byte()? {
                None => None,
                Some(b @ 0..=0x7F) => Some(Self::from_utf8(b as u32)),
                Some(b @ 0xC2..=0xDF) => self.continuation(2, (b & 0x1F) as u32)?,
                Some(0xE0) => {
                    self.next_byte = None;
                    match self.peek_byte()? {
                        Some(b @ 0xA0..=0xBF) => self.continuation(2, (b & 0x3F) as u32)?,
                        _ => Some(Self::REPLACEMENT_CHAR),
                    }
                }
                Some(b @ 0xE1..=0xEC) | Some(b @ 0xEE..=0xEF) => {
                    self.continuation(3, (b & 0xF) as u32)?
                }
                Some(0xED) => {
                    self.next_byte = None;
                    match self.peek_byte()? {
                        Some(b @ 0x80..=0x9F) => {
                            self.continuation(2, 0x340u32 | (b & 0x3F) as u32)?
                        }
                        _ => Some(Self::REPLACEMENT_CHAR),
                    }
                }
                Some(0xF0) => {
                    self.next_byte = None;
                    match self.peek_byte()? {
                        Some(b @ 0x90..=0xBF) => self.continuation(3, (b & 0x7) as u32)?,
                        _ => Some(Self::REPLACEMENT_CHAR),
                    }
                }
                Some(b @ 0xF1..=0xF3) => self.continuation(4, (b & 0x7) as u32)?,
                Some(0xF4) => {
                    self.next_byte = None;
                    match self.peek_byte()? {
                        Some(b @ 0x80..=0x8F) => self.continuation(3, 0x100 | (b & 0x7) as u32)?,
                        _ => Some(Self::REPLACEMENT_CHAR),
                    }
                }
                Some(_) => Some(Self::REPLACEMENT_CHAR),
            }
        }
        Ok(self.next_char)
    }

    fn position(&self) -> Option<stream::Position> {
        self.position.clone()
    }
}
