use std::fmt;

use crate::lexer::Lexer;
use crate::parser::{BinaryExpr, BinaryOp, Expr, Parser, ParserError, UnaryExpr, UnaryOp};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct Interpreter {
    ast: Expr,
}

impl Interpreter {
    pub fn from_str(s: &str) -> Result<Self, ParserError<'_>> {
        let lexer = Lexer::new(s);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse()?;
        Ok(Self { ast })
    }

    pub fn new(ast: Expr) -> Self {
        Self { ast }
    }

    pub fn eval(self) -> Object {
        Self::eval_expr(self.ast)
    }

    fn eval_expr(expr: Expr) -> Object {
        match expr {
            Expr::Binary(inner) => Self::eval_binary_expr(*inner),
            Expr::Unary(inner) => Self::eval_unary_expr(*inner),
            Expr::Group(inner) => Self::eval_expr(*inner),
            Expr::String(s) => Object::String(s),
            Expr::Number(n) => Object::Number(n),
            Expr::Bool(b) => Object::Bool(b),
            Expr::Nil => Object::Nil,
        }
    }

    fn eval_unary_expr(expr: UnaryExpr) -> Object {
        let UnaryExpr { op, right } = expr;

        let right = Self::eval_expr(right);
        match op {
            UnaryOp::Neg => match right {
                Object::String(_) => todo!(),
                Object::Number(n) => Object::Number(-n),
                Object::Bool(_) => todo!(),
                Object::Nil => todo!(),
            },
            UnaryOp::Not => Object::Bool(!right.is_truthy()),
        }
    }

    fn eval_binary_expr(expr: BinaryExpr) -> Object {
        let BinaryExpr { left, op, right } = expr;

        let left = Self::eval_expr(left);
        let right = Self::eval_expr(right);
        match op {
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
            BinaryOp::Eq => todo!(),
            BinaryOp::NotEq => todo!(),
            BinaryOp::Lt => todo!(),
            BinaryOp::Gt => todo!(),
            BinaryOp::LtEq => todo!(),
            BinaryOp::GtEq => todo!(),
        }
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
