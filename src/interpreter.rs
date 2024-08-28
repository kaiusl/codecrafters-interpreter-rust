use std::fmt;

use crate::parser::Expr;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct Interpreter {
    ast: Expr,
}

impl Interpreter {
    pub fn new(ast: Expr) -> Self {
        Self { ast }
    }

    pub fn eval(self) -> Object {
        Self::eval_expr(self.ast)
    }

    fn eval_expr(expr: Expr) -> Object {
        match expr {
            Expr::Binary(_) => todo!(),
            Expr::Unary(_) => todo!(),
            Expr::Group(inner) => Self::eval_expr(*inner),
            Expr::String(s) => Object::String(s),
            Expr::Number(n) => Object::Number(n),
            Expr::Bool(b) => Object::Bool(b),
            Expr::Nil => Object::Nil,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
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
