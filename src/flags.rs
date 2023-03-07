#[derive(Debug, Clone)]
pub(crate) enum QuoteFlag {
    Chars,
    Codes,
    Atom,
}

#[derive(Debug, Clone)]
pub(crate) enum UnknownFlag {
    Error,
    Fail,
    Warning,
}

#[derive(Debug, Clone)]
pub(crate) struct Flags {
    pub char_conversion: bool,
    pub double_quotes: QuoteFlag,
    pub back_quotes: QuoteFlag,
    pub unknown: UnknownFlag,
    pub debug: bool,
    pub colon_sets_calling_context: bool,
}

impl Default for Flags {
    fn default() -> Self {
        Self {
            char_conversion: true,
            double_quotes: QuoteFlag::Chars,
            back_quotes: QuoteFlag::Codes,
            unknown: UnknownFlag::Error,
            debug: false,
            colon_sets_calling_context: false,
        }
    }
}
