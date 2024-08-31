use std::collections::HashMap;
use std::fmt;

use crate::lexer::{Lexer, Span, Spanned};
use crate::parser::{
    AssignmentExpr, BinaryExpr, BinaryOp, Expr, Parser, ParserError, Stmt, UnaryExpr, UnaryOp,
    VarDeclaration,
};
use miette::Result;

use self::error::RuntimeErrorBuilder;

mod error;
#[cfg(test)]
mod tests;

pub fn interpret(input: &str) -> Result<Result<()>, Vec<ParserError<'_>>> {
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
            input,
            global_env: Env::new(),
        }
    }

    pub fn from_str(s: &'a str) -> Result<Self, Vec<ParserError<'_>>> {
        let lexer = Lexer::new(s);
        let parser = Parser::new(lexer);
        let (ast, errors) = parser.parse();
        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(Self {
            ast,
            input: s,
            global_env: Env::new(),
        })
    }

    pub fn eval(&mut self) -> Result<()> {
        for stmt in std::mem::take(&mut self.ast) {
            self.eval_stmt(stmt)
                .map_err(|err| err.src(self.input.to_string()).build())?;
        }

        Ok(())
    }

    fn eval_stmt(&mut self, stmt: Spanned<Stmt>) -> Result<(), RuntimeErrorBuilder> {
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

    fn eval_expr(&mut self, expr: Spanned<Expr>) -> Result<Spanned<Object>, RuntimeErrorBuilder> {
        match expr.item {
            Expr::Binary(inner) => self.eval_binary_expr(*inner, expr.span),
            Expr::Unary(inner) => self.eval_unary_expr(*inner, expr.span),
            Expr::Group(inner) => self.eval_expr(*inner),
            Expr::String(s) => Ok(Spanned::new(Object::String(s), expr.span)),
            Expr::Number(n) => Ok(Spanned::new(Object::Number(n), expr.span)),
            Expr::Bool(b) => Ok(Spanned::new(Object::Bool(b), expr.span)),
            Expr::Nil => Ok(Spanned::new(Object::Nil, expr.span)),
            Expr::GlobalVariable(ident) => {
                let value = self.global_env.get(&ident);
                match value {
                    // TODO: remove .clone()
                    Ok(value) => Ok(Spanned::new(value.clone(), expr.span)),
                    Err(err) => Err(err.span(expr.span)),
                }
            }
            Expr::Assignment(assign) => {
                let span = expr.span;
                let AssignmentExpr { ident, expr } = *assign;
                let value = self.eval_expr(expr)?;
                self.global_env.assign(ident.item, value.item)?;
                Ok(Spanned::new(Object::Nil, span))
            }
        }
    }

    fn eval_unary_expr(
        &mut self,
        expr: UnaryExpr,
        span: Span,
    ) -> Result<Spanned<Object>, RuntimeErrorBuilder> {
        let UnaryExpr { op, right } = expr;

        let right = self.eval_expr(right)?;
        match op {
            UnaryOp::Neg => match right.item {
                Object::Number(n) => Ok(Spanned::new(Object::Number(-n), span)),
                _ => Err(RuntimeErrorBuilder::operand_must_be_a_number().span(span)),
            },
            UnaryOp::Not => Ok(Spanned::new(Object::Bool(!right.is_truthy()), span)),
        }
    }

    fn eval_binary_expr(
        &mut self,
        expr: BinaryExpr,
        span: Span,
    ) -> Result<Spanned<Object>, RuntimeErrorBuilder> {
        let BinaryExpr { left, op, right } = expr;

        let left = self.eval_expr(left)?;
        let right = self.eval_expr(right)?;

        let item = match op {
            BinaryOp::Add => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l + r),
                (Object::String(l), Object::String(r)) => Object::String(l + &r),
                _ => {
                    return Err(
                        RuntimeErrorBuilder::operands_must_be_strings_or_numbers().span(span)
                    )
                }
            },
            BinaryOp::Sub => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l - r),
                _ => return Err(RuntimeErrorBuilder::operands_must_be_numbers().span(span)),
            },
            BinaryOp::Mul => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l * r),
                _ => return Err(RuntimeErrorBuilder::operands_must_be_numbers().span(span)),
            },
            BinaryOp::Div => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Number(l / r),
                _ => return Err(RuntimeErrorBuilder::operands_must_be_numbers().span(span)),
            },
            BinaryOp::Eq => Object::Bool(left.item.eq_wo_span(&right.item)),
            BinaryOp::NotEq => Object::Bool(!left.item.eq_wo_span(&right.item)),
            BinaryOp::Lt => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l < r),
                _ => return Err(RuntimeErrorBuilder::operands_must_be_numbers().span(span)),
            },
            BinaryOp::Gt => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l > r),
                _ => return Err(RuntimeErrorBuilder::operands_must_be_numbers().span(span)),
            },
            BinaryOp::LtEq => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l <= r),
                _ => return Err(RuntimeErrorBuilder::operands_must_be_numbers().span(span)),
            },
            BinaryOp::GtEq => match (left.item, right.item) {
                (Object::Number(l), Object::Number(r)) => Object::Bool(l >= r),
                _ => return Err(RuntimeErrorBuilder::operands_must_be_numbers().span(span)),
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

    pub fn get(&self, ident: &str) -> Result<&Object, RuntimeErrorBuilder> {
        match self.values.get(ident) {
            Some(value) => Ok(value),
            None => {
                let err = RuntimeErrorBuilder::new().msg(format!("Undefined variable '{ident}'"));
                Err(err)
            }
        }
    }

    pub fn set(&mut self, ident: String, value: Object) {
        self.values.insert(ident, value);
    }

    pub fn assign(&mut self, ident: String, value: Object) -> Result<&Object, RuntimeErrorBuilder> {
        match self.values.entry(ident) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                entry.insert(value);
                Ok(entry.into_mut())
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                let ident = entry.into_key();
                let err = RuntimeErrorBuilder::new().msg(format!("Undefined variable '{ident}'"));
                Err(err)
            }
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
