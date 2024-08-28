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
        match self.ast {
            Expr::Binary(_) => todo!(),
            Expr::Unary(_) => todo!(),
            Expr::Group(_) => todo!(),
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
            Object::Number(n) if n.fract() == 0.0 => write!(f, "{}.0", n),
            Object::Number(n) => write!(f, "{}", n),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Nil => write!(f, "nil"),
        }
    }
}
