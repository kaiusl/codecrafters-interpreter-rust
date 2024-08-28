use std::borrow::Cow;

use crate::lexer::{LexerError, Token};

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
pub enum ParserError<'a> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexer(crate::lexer::LexerError<'a>),

    #[error(transparent)]
    #[diagnostic(transparent)]
    MissingItem(MissingItemError<'a>),
}

impl<'a> From<LexerError<'a>> for ParserError<'a> {
    fn from(error: LexerError<'a>) -> Self {
        Self::Lexer(error)
    }
}

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
#[error("[line {line_number}] Error at '{}': {msg}", token.lexeme())]
pub struct MissingItemError<'a> {
    #[source_code]
    line: Cow<'a, str>,
    line_number: usize,
    token: Token<'a>,
    #[label("here")]
    span: miette::SourceSpan,
    #[help]
    msg: &'static str,
}

impl<'a> MissingItemError<'a> {
    pub fn new(
        line: &'a str,
        line_number: usize,
        token: Token<'a>,
        span: miette::SourceSpan,
        msg: &'static str,
    ) -> Self {
        Self {
            line: line.into(),
            line_number,
            token,
            span,
            msg,
        }
    }
}
