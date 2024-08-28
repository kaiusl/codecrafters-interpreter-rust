use std::borrow::Cow;

use crate::lexer::LexerError;

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
pub enum ParserError<'a> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexer(crate::lexer::LexerError<'a>),
}

impl<'a> From<LexerError<'a>> for ParserError<'a> {
    fn from(error: LexerError<'a>) -> Self {
        Self::Lexer(error)
    }
}
