
#[derive(Debug,Clone)]
pub enum QuoteFlags {
    Chars,
    Codes,
    Atom
}

#[derive(Debug,Clone)]
pub enum UnknownFlags {
    Error,
    Fail,
    Warning
}

#[derive(Debug,Clone)]
pub struct Flags {
    pub char_conversion: bool,
    pub double_quotes: QuoteFlags,
    pub back_quotes: QuoteFlags,
    pub unknown: UnknownFlags,
    pub debug: bool,
    pub colon_sets_calling_context: bool
}

impl Default for Flags {
    fn default() -> Self {
        Self {
            char_conversion: true,
            double_quotes: QuoteFlags::Chars,
            back_quotes: QuoteFlags::Codes,
            unknown: UnknownFlags::Error,
            debug: false,
            colon_sets_calling_context: false
        }
    }
}
