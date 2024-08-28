use core::fmt;
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
#[error("[line {line_number}] Error at {}: {msg}", location)]
pub struct MissingItemError<'a> {
    #[source_code]
    line: Cow<'a, str>,
    line_number: usize,
    location: MissingItemLocation<'a>,
    #[label("here")]
    span: miette::SourceSpan,
    #[help]
    msg: &'static str,
}

#[derive(Debug, Clone)]
pub enum MissingItemLocation<'a> {
    Str(&'a str),
    Token(Token<'a>),
    Char(char),
    End,
}

impl<'a> fmt::Display for MissingItemLocation<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MissingItemLocation::Str(s) => write!(f, "'{s}'"),
            MissingItemLocation::Token(t) => write!(f, "'{}'", t.lexeme()),
            MissingItemLocation::Char(c) => write!(f, "'{c}'"),
            MissingItemLocation::End => write!(f, "end"),
        }
    }
}

impl<'a> MissingItemError<'a> {
    pub fn new(
        line: &'a str,
        line_number: usize,
        location: MissingItemLocation<'a>,
        span: miette::SourceSpan,
        msg: &'static str,
    ) -> Self {
        Self {
            line: line.into(),
            line_number,
            location,
            span,
            msg,
        }
    }
}
