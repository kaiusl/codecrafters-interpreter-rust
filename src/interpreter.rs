use std::collections::HashMap;
use std::fmt;

use crate::lexer::{Lexer, Span, Spanned};
use crate::parser::{
    BinaryExpr, BinaryOp, Expr, Parser, ParserError, Stmt, UnaryExpr, UnaryOp, VarDeclaration,
};
use miette::Result;

use self::error::RuntimeError;

mod error;
#[cfg(test)]
mod tests;

pub fn interpret(input: &str) -> Result<Result<()>, ParserError<'_>> {
    Ok(Interpreter::from_str(input)?.eval())
}

pub fn eval(input: &str) -> Result<Result<()>, ParserError<'_>> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_expr()?;

    let mut stmts = Vec::new();
    stmts.push(Spanned::new(Stmt::Print(ast), Span::new(0, 0, input.len())));

    let mut interpreter = Interpreter::new(stmts, input);

    Ok(interpreter.eval())
}

#[derive(Debug, Clone)]
struct Interpreter<'a> {
    ast: Vec<Spanned<Stmt>>,
    input: &'a str,
    global_env: Env,
}

impl<'a> Interpreter<'a> {
    pub fn new(ast: Vec<Spanned<Stmt>>, input: &'a str) -> Self {
        Self {
            ast,
            input: "",
            global_env: Env::new(),
        }
    }

    pub fn from_str(s: &'a str) -> Result<Self, ParserError<'_>> {
        let lexer = Lexer::new(s);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse()?;
        Ok(Self {
            ast,
            input: s,
            global_env: Env::new(),
        })
    }

    pub fn eval(&mut self) -> Result<()> {
        for stmt in std::mem::take(&mut self.ast) {
            self.eval_stmt(stmt)?;
        }

        Ok(())
    }

    fn eval_stmt(&mut self, stmt: Spanned<Stmt>) -> Result<(), RuntimeError> {
        match stmt.item {
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
            }
            Stmt::Print(expr) => {
                let value = self.eval_expr(expr)?;
                println!("{}", value);
            }
            Stmt::Var(declaration) => {
                let VarDeclaration { ident, expr } = declaration;
                let value = match expr {
                    Some(expr) => self.eval_expr(expr)?,
                    None => Spanned::new(Object::Nil, stmt.span),
                };

                self.global_env.set(ident.item, value.item);
            }
        }

        Ok(())
    }

    fn eval_expr(&mut self, expr: Spanned<Expr>) -> Result<Spanned<Object>, RuntimeError> {
        match expr.item {
            Expr::Binary(inner) => self.eval_binary_expr(*inner, expr.span),
            Expr::Unary(inner) => self.eval_unary_expr(*inner, expr.span),
            Expr::Group(inner) => self.eval_expr(*inner),
            Expr::String(s) => Ok(Spanned::new(Object::String(s), expr.span)),
            Expr::Number(n) => Ok(Spanned::new(Object::Number(n), expr.span)),
            Expr::Bool(b) => Ok(Spanned::new(Object::Bool(b), expr.span)),
            Expr::Nil => Ok(Spanned::new(Object::Nil, expr.span)),
            Expr::GlobalVariable(ident) => {
                let value = self.global_env.get(&ident, &expr.span)?;
                // TODO: remove clone
                Ok(Spanned::new(value.clone(), expr.span))
            }
        }
    }

    fn eval_unary_expr(
        &mut self,
        expr: UnaryExpr,
        span: Span,
    ) -> Result<Spanned<Object>, RuntimeError> {
        let UnaryExpr { op, right } = expr;

        let right = self.eval_expr(right)?;
        match op {
            UnaryOp::Neg => match right.item {
                Object::Number(n) => Ok(Spanned::new(Object::Number(-n), span)),
                _ => Err(RuntimeError::operand_must_be_a_number(right.span)),
            },
            UnaryOp::Not => Ok(Spanned::new(Object::Bool(!right.is_truthy()), span)),
        }
    }

    fn eval_binary_expr(
        &mut self,
        expr: BinaryExpr,
        span: Span,
    ) -> Result<Spanned<Object>, RuntimeError> {
        let BinaryExpr { left, op, right } = expr;

        let left = self.eval_expr(left)?;
        let right = self.eval_expr(right)?;

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

#[derive(Debug, Clone)]
pub struct Env {
    pub values: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn get(&self, ident: &str, span: &Span) -> Result<&Object, RuntimeError> {
        match self.values.get(ident) {
            Some(value) => Ok(value),
            None => {
                todo!("handle undefined variable error")
            }
        }
    }

    pub fn set(&mut self, ident: String, value: Object) {
        self.values.insert(ident, value);
    }

    pub fn declare(&mut self, ident: String) {
        self.values.insert(ident, Object::Nil);
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
