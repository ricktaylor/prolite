#[derive(Debug)]
pub enum ErrorKind {}

#[derive(Debug)]
pub struct ErrorInfo {}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub info: Option<ErrorInfo>,
}

impl Error {
    pub fn new<T>(kind: ErrorKind, info: Option<ErrorInfo>) -> Result<T, Error> {
        Err(Self { kind, info })
    }
}
