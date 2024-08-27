#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
pub enum LexerError<'a> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnknownToken(UnknownTokenError<'a>),
}

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
#[error("[line {line_number}] Error: Unexpected character: {token}")]
pub struct UnknownTokenError<'a> {
    #[source_code]
    line: &'a str,
    line_number: usize,
    token: char,
    #[label("here")]
    span: miette::SourceSpan,
}

impl<'a> UnknownTokenError<'a> {
    pub fn new(line: &'a str, line_number: usize, token: char, span: miette::SourceSpan) -> Self {
        Self {
            line,
            line_number,
            token,
            span,
        }
    }
}
