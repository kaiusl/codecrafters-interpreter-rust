use std::borrow::Cow;

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
#[error("{msg}")]
pub struct RuntimeError {
    msg: &'static str,
}

impl RuntimeError {
    pub fn new(msg: &'static str) -> Self {
        Self { msg }
    }
}
