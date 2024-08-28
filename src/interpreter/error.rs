use std::borrow::Cow;

use crate::lexer::Span;

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
#[error("{msg}\n[line {line}]")]
pub struct RuntimeError {
    #[source_code]
    src: Cow<'static, str>,
    line: usize,
    #[label("here")]
    span: miette::SourceSpan,
    msg: &'static str,
}

impl RuntimeError {
    pub fn new(msg: &'static str, src: impl Into<Cow<'static, str>>, span: Span) -> Self {
        Self {
            msg,
            src: src.into(),
            line: span.line,
            span: (span.start..span.end).into(),
        }
    }

    pub fn operands_must_be_numbers(span: Span) -> Self {
        Self::new("Operands must be numbers.", "", span)
    }

    pub fn operands_must_be_strings_or_numbers(span: Span) -> Self {
        Self::new("Operands must be two numbers or two strings.", "", span)
    }

    pub fn operand_must_be_a_number(span: Span) -> Self {
        Self::new("Operand must be a number.", "", span)
    }

    pub fn add_source(mut self, src: impl Into<Cow<'static, str>>) -> Self {
        self.src = src.into();
        self
    }
}
