use std::borrow::Cow;

use crate::lexer::Span;

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
#[error("{msg}\n[line {line}]")]
pub struct RuntimeError {
    #[source_code]
    src: Option<Cow<'static, str>>,
    line: usize,
    #[label("here")]
    span: miette::SourceSpan,
    msg: Cow<'static, str>,
}

pub struct RuntimeErrorBuilder {
    msg: Option<Cow<'static, str>>,
    line: Option<usize>,
    span: Option<miette::SourceSpan>,
    src: Option<Cow<'static, str>>,
}

impl RuntimeErrorBuilder {
    pub fn new() -> Self {
        Self {
            msg: None,
            line: None,
            span: None,
            src: None,
        }
    }

    pub fn operands_must_be_numbers() -> Self {
        Self::new().msg("Operands must be numbers.")
    }

    pub fn operands_must_be_strings_or_numbers() -> Self {
        Self::new().msg("Operands must be two numbers or two strings.")
    }

    pub fn operand_must_be_a_number() -> Self {
        Self::new().msg("Operand must be a number.")
    }

    pub fn msg(mut self, msg: impl Into<Cow<'static, str>>) -> Self {
        self.msg = Some(msg.into());
        self
    }

    pub fn span(mut self, span: Span) -> Self {
        self.line = Some(span.line);
        self.span = Some(span.into());
        self
    }

    pub fn src(mut self, src: impl Into<Cow<'static, str>>) -> Self {
        self.src = Some(src.into());
        self
    }

    pub fn build(self) -> RuntimeError {
        RuntimeError {
            src: self.src,
            line: self.line.expect("line must be set"),
            span: self.span.expect("span must be set"),
            msg: self.msg.expect("msg must be set"),
        }
    }
}