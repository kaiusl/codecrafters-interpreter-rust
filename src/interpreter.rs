use std::fmt;

use crate::lexer::{Lexer, Span};
use crate::parser::{
    BinaryExpr, BinaryOp, Expr, ExprKind, Parser, ParserError, UnaryExpr, UnaryOp,
};
use miette::Result;

use self::error::RuntimeError;

mod error;
#[cfg(test)]
mod tests;

pub fn eval(s: &str) -> Result<Result<Object>, ParserError<'_>> {
    Ok(Interpreter::from_str(s)?.eval())
}

#[derive(Debug, Clone)]
struct Interpreter<'a> {
    ast: Option<Expr>,
    input: &'a str,
}

impl<'a> Interpreter<'a> {
    pub fn from_str(s: &'a str) -> Result<Self, ParserError<'_>> {
        let lexer = Lexer::new(s);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse()?;
        Ok(Self {
            ast: Some(ast),
            input: s,
        })
    }

    pub fn new(ast: Expr, input: &'a str) -> Self {
        Self {
            ast: Some(ast),
            input,
        }
    }

    pub fn eval(&mut self) -> Result<Object> {
        Self::eval_expr(self.ast.take().unwrap())
            .map(|o| o.kind)
            .map_err(|e| e.add_source(self.input.to_string()).into())
    }

    fn eval_expr(expr: Expr) -> Result<ObjectInternal, RuntimeError> {
        match expr.kind {
            ExprKind::Binary(inner) => Self::eval_binary_expr(*inner, expr.span),
            ExprKind::Unary(inner) => Self::eval_unary_expr(*inner, expr.span),
            ExprKind::Group(inner) => Self::eval_expr(*inner),
            ExprKind::String(s) => Ok(ObjectInternal::new(Object::String(s), expr.span)),
            ExprKind::Number(n) => Ok(ObjectInternal::new(Object::Number(n), expr.span)),
            ExprKind::Bool(b) => Ok(ObjectInternal::new(Object::Bool(b), expr.span)),
            ExprKind::Nil => Ok(ObjectInternal::new(Object::Nil, expr.span)),
        }
    }

    fn eval_unary_expr(expr: UnaryExpr, span: Span) -> Result<ObjectInternal, RuntimeError> {
        let UnaryExpr { op, right } = expr;

        let right = Self::eval_expr(right)?;
        match op {
            UnaryOp::Neg => match right.kind {
                Object::Number(n) => Ok(ObjectInternal::new(Object::Number(-n), span)),
                _ => Err(RuntimeError::operand_must_be_a_number(right.span).into()),
            },
            UnaryOp::Not => Ok(ObjectInternal::new(Object::Bool(!right.is_truthy()), span)),
        }
    }

    fn eval_binary_expr(expr: BinaryExpr, span: Span) -> Result<ObjectInternal, RuntimeError> {
        let BinaryExpr { left, op, right } = expr;

        let fail_must_be_numbers =
            || Err(RuntimeError::operands_must_be_numbers(span.clone()).into());

        let left = Self::eval_expr(left)?;
        let right = Self::eval_expr(right)?;
        let kind = match op {
            BinaryOp::Add => match (left.kind, right.kind) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l + r),
                (Object::String(l), Object::String(r)) => Object::String(l + &r),
                _ => return Err(RuntimeError::operands_must_be_strings_or_numbers(span).into()),
            },
            BinaryOp::Sub => match (left.kind, right.kind) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l - r),
                _ => return fail_must_be_numbers(),
            },
            BinaryOp::Mul => match (left.kind, right.kind) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l * r),
                _ => return fail_must_be_numbers(),
            },
            BinaryOp::Div => match (left.kind, right.kind) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l / r),
                _ => return fail_must_be_numbers(),
            },
            BinaryOp::Eq => Object::Bool(left == right),
            BinaryOp::NotEq => Object::Bool(left != right),
            BinaryOp::Lt => match (left.kind, right.kind) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l < r),
                _ => return fail_must_be_numbers(),
            },
            BinaryOp::Gt => match (left.kind, right.kind) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l > r),
                _ => return fail_must_be_numbers(),
            },
            BinaryOp::LtEq => match (left.kind, right.kind) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l <= r),
                _ => return fail_must_be_numbers(),
            },
            BinaryOp::GtEq => match (left.kind, right.kind) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l >= r),
                _ => return fail_must_be_numbers(),
            },
        };

        Ok(ObjectInternal::new(kind, span))
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ObjectInternal {
    kind: Object,
    span: Span,
}

impl ObjectInternal {
    pub fn new(kind: Object, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn is_truthy(&self) -> bool {
        self.kind.is_truthy()
    }
}

impl fmt::Display for ObjectInternal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Nil | Object::Bool(false))
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Number(n) => write!(f, "{}", n),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Nil => write!(f, "nil"),
        }
    }
}
