
pub enum QuoteFlags {
    Chars,
    Codes,
    Atom
}

pub enum UnknownFlags {
    Error,
    Fail,
    Warning
}

pub struct Flags {
    char_conversion: bool,
    double_quotes: QuoteFlags,
    back_quotes: QuoteFlags,
    unknown: UnknownFlags,
    debug: bool,
    colon_sets_calling_context: bool
}

impl Default for Flags {
    fn default() -> Self {
        Flags {
            char_conversion: true,
            double_quotes: QuoteFlags::Chars,
            back_quotes: QuoteFlags::Codes,
            unknown: UnknownFlags::Error,
            debug: false,
            colon_sets_calling_context: false
        }
    }
}
