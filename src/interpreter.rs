use std::fmt;

use crate::lexer::{Lexer, Span, Spanned};
use crate::parser::{BinaryExpr, BinaryOp, Expr, Parser, ParserError, UnaryExpr, UnaryOp};
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
    ast: Option<Spanned<Expr>>,
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

    pub fn eval(&mut self) -> Result<Object> {
        Self::eval_expr(self.ast.take().unwrap())
            .map(|o| o.item)
            .map_err(|e| e.add_source(self.input.to_string()).into())
    }

    fn eval_expr(expr: Spanned<Expr>) -> Result<Spanned<Object>, RuntimeError> {
        match expr.item {
            Expr::Binary(inner) => Self::eval_binary_expr(*inner, expr.span),
            Expr::Unary(inner) => Self::eval_unary_expr(*inner, expr.span),
            Expr::Group(inner) => Self::eval_expr(*inner),
            Expr::String(s) => Ok(Spanned::new(Object::String(s), expr.span)),
            Expr::Number(n) => Ok(Spanned::new(Object::Number(n), expr.span)),
            Expr::Bool(b) => Ok(Spanned::new(Object::Bool(b), expr.span)),
            Expr::Nil => Ok(Spanned::new(Object::Nil, expr.span)),
        }
    }

    fn eval_unary_expr(expr: UnaryExpr, span: Span) -> Result<Spanned<Object>, RuntimeError> {
        let UnaryExpr { op, right } = expr;

        let right = Self::eval_expr(right)?;
        match op {
            UnaryOp::Neg => match right.item {
                Object::Number(n) => Ok(Spanned::new(Object::Number(-n), span)),
                _ => Err(RuntimeError::operand_must_be_a_number(right.span)),
            },
            UnaryOp::Not => Ok(Spanned::new(Object::Bool(!right.is_truthy()), span)),
        }
    }

    fn eval_binary_expr(expr: BinaryExpr, span: Span) -> Result<Spanned<Object>, RuntimeError> {
        let BinaryExpr { left, op, right } = expr;

        let left = Self::eval_expr(left)?;
        let right = Self::eval_expr(right)?;

        let item = match op {
            BinaryOp::Add => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l + r),
                (Object::String(l), Object::String(r)) => Object::String(l + &r),
                _ => return Err(RuntimeError::operands_must_be_strings_or_numbers(span)),
            },
            BinaryOp::Sub => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l - r),
                _ => return Err(RuntimeError::operands_must_be_numbers(span)),
            },
            BinaryOp::Mul => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l * r),
                _ => return Err(RuntimeError::operands_must_be_numbers(span)),
            },
            BinaryOp::Div => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l / r),
                _ => return Err(RuntimeError::operands_must_be_numbers(span)),
            },
            BinaryOp::Eq => Object::Bool(left.item.eq_wo_span(&right.item)),
            BinaryOp::NotEq => Object::Bool(!left.item.eq_wo_span(&right.item)),
            BinaryOp::Lt => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l < r),
                _ => return Err(RuntimeError::operands_must_be_numbers(span)),
            },
            BinaryOp::Gt => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l > r),
                _ => return Err(RuntimeError::operands_must_be_numbers(span)),
            },
            BinaryOp::LtEq => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l <= r),
                _ => return Err(RuntimeError::operands_must_be_numbers(span)),
            },
            BinaryOp::GtEq => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l >= r),
                _ => return Err(RuntimeError::operands_must_be_numbers(span)),
            },
        };

        Ok(Spanned::new(item, span))
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
    pub fn eq_wo_span(&self, other: &Object) -> bool {
        // Not necessary atm, but when later objects can contain other objects it avoids wrong comparison
        match (self, other) {
            (Object::String(l), Object::String(r)) => l == r,
            (Object::Number(l), Object::Number(r)) => l == r,
            (Object::Bool(l), Object::Bool(r)) => l == r,
            (Object::Nil, Object::Nil) => true,
            _ => false,
        }
    }

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
