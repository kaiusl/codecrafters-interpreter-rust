use std::fmt;

use crate::lexer::Lexer;
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
    }

    fn eval_expr(expr: Expr) -> Result<Object> {
        match expr {
            Expr::Binary(inner) => Self::eval_binary_expr(*inner),
            Expr::Unary(inner) => Self::eval_unary_expr(*inner),
            Expr::Group(inner) => Self::eval_expr(*inner),
            Expr::String(s) => Ok(Object::String(s)),
            Expr::Number(n) => Ok(Object::Number(n)),
            Expr::Bool(b) => Ok(Object::Bool(b)),
            Expr::Nil => Ok(Object::Nil),
        }
    }

    fn eval_unary_expr(expr: UnaryExpr) -> Result<Object> {
        let UnaryExpr { op, right } = expr;

        let right = Self::eval_expr(right)?;
        match op {
            UnaryOp::Neg => match right {
                Object::Number(n) => Ok(Object::Number(-n)),
                _ => Err(RuntimeError::new("Operand must be a number").into()),
            },
            UnaryOp::Not => Ok(Object::Bool(!right.is_truthy())),
        }
    }

    fn eval_binary_expr(expr: BinaryExpr) -> Result<Object> {
        let BinaryExpr { left, op, right } = expr;

        let left = Self::eval_expr(left)?;
        let right = Self::eval_expr(right)?;
        Ok(match op {
            BinaryOp::Add => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l + r),
                (Object::String(l), Object::String(r)) => Object::String(l + &r),
                _ => todo!(),
            },
            BinaryOp::Sub => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l - r),
                _ => todo!(),
            },
            BinaryOp::Mul => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l * r),
                _ => todo!(),
            },
            BinaryOp::Div => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l / r),
                _ => todo!(),
            },
            BinaryOp::Eq => Object::Bool(left == right),
            BinaryOp::NotEq => Object::Bool(left != right),
            BinaryOp::Lt => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l < r),
                _ => todo!(),
            },
            BinaryOp::Gt => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l > r),
                _ => todo!(),
            },
            BinaryOp::LtEq => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l <= r),
                _ => todo!(),
            },
            BinaryOp::GtEq => match (left, right) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l >= r),
                _ => todo!(),
            },
        })
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
