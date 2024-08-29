use core::fmt;

use crate::lexer::{Keyword, Lexer, LexerError, Span, Spanned, Token};

mod error;
#[cfg(test)]
mod tests;

pub use error::*;

struct PeekableLexer<'a> {
    lexer: Lexer<'a>,
    peeked: Option<Result<Spanned<Token<'a>>, LexerError<'a>>>,
    line: usize,
}

impl<'a> PeekableLexer<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            peeked: None,
            line: 0,
        }
    }

    fn peek(&mut self) -> Option<&Result<Spanned<Token<'a>>, LexerError<'a>>> {
        if self.peeked.is_none() {
            self.peeked = self.lexer.next();
        }
        self.peeked.as_ref()
    }

    fn next_if<F>(&mut self, f: F) -> Option<Result<Spanned<Token<'a>>, LexerError<'a>>>
    where
        F: Fn(&Token<'a>) -> bool,
    {
        if let Some(peeked) = self.peek().and_then(|res| res.as_ref().ok()) {
            if f(&peeked) {
                self.line = self.lexer.line();
                return self.peeked.take();
            }
        }
        None
    }

    fn line(&self) -> usize {
        self.line
    }
}

impl<'a> Iterator for PeekableLexer<'a> {
    type Item = Result<Spanned<Token<'a>>, LexerError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = if self.peeked.is_some() {
            self.peeked.take()
        } else {
            self.lexer.next()
        };
        self.line = self.lexer.line();

        result
    }
}

pub struct Parser<'a> {
    input: &'a str,
    lexer: PeekableLexer<'a>,
    errors: Vec<ParserError<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            input: lexer.input(),
            lexer: PeekableLexer::new(lexer),
            errors: vec![],
        }
    }
    pub fn parse(&mut self) -> Result<Spanned<Expr>, ParserError<'a>> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Spanned<Expr>, ParserError<'a>> {
        let mut expr = self.parse_comparison()?;

        while let Some(token) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::EqEq | Token::BangEq))
        {
            let token = token?;
            let op = BinaryOp::try_from_token(&token).unwrap();
            let right = self.parse_comparison()?;
            let span = token.span.combine(&right.span).combine(&expr.span);
            let exprkind = Expr::binary(expr, op, right);
            expr = Spanned::new(exprkind, span);
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Spanned<Expr>, ParserError<'a>> {
        let mut expr = self.parse_term()?;

        while let Some(token) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::Gt | Token::Lt | Token::GtEq | Token::LtEq))
        {
            let token = token?;
            let op = BinaryOp::try_from_token(&token).unwrap();
            let right = self.parse_term()?;
            let span = token.span.combine(&right.span).combine(&expr.span);
            let exprkind = Expr::binary(expr, op, right);
            expr = Spanned::new(exprkind, span);
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Spanned<Expr>, ParserError<'a>> {
        let mut expr = self.parse_factor()?;

        while let Some(token) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::Minus | Token::Plus))
        {
            let token = token?;
            let op = BinaryOp::try_from_token(&token).unwrap();
            let right = self.parse_factor()?;
            let span = token.span.combine(&right.span).combine(&expr.span);
            let exprkind = Expr::binary(expr, op, right);
            expr = Spanned::new(exprkind, span);
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Spanned<Expr>, ParserError<'a>> {
        let mut expr = self.parse_unary()?;

        while let Some(token) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::Slash | Token::Star))
        {
            let token = token?;
            let op = BinaryOp::try_from_token(&token).unwrap();
            let right = self.parse_unary()?;
            let span = token.span.combine(&right.span).combine(&expr.span);
            let exprkind = Expr::binary(expr, op, right);
            expr = Spanned::new(exprkind, span);
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Spanned<Expr>, ParserError<'a>> {
        if let Some(token) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::Bang | Token::Minus))
        {
            let token = token?;
            let op = UnaryOp::try_from_token(&token).unwrap();
            let right = self.parse_unary()?;
            let span = token.span.combine(&right.span);
            let expr = Expr::unary(op, right);
            return Ok(Spanned::new(expr, span));
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Spanned<Expr>, ParserError<'a>> {
        fn error_unexpected_token<'a>(s: &mut Parser<'a>) -> ParserError<'a> {
            let (token, span, len) = match s.lexer.peek() {
                Some(Ok(token)) => (
                    &MissingItemLocation::Token(token.item.clone()),
                    &token.span,
                    token.lexeme().len(),
                ),
                Some(Err(e)) => return e.clone().into(),
                None => (
                    &MissingItemLocation::End,
                    &Span::from_len(s.lexer.line(), s.input.len() - 1, 1),
                    1,
                ),
            };

            let err = MissingItemError::new(
                s.input,
                span.line,
                token.clone(),
                (span.start, len).into(),
                "Expect expression.",
            );

            ParserError::MissingItem(err)
        }

        let Some(Ok(token)) = self.lexer.next_if(|tok| {
            matches!(
                tok,
                Token::Number { .. }
                    | Token::String { .. }
                    | Token::Keyword(Keyword::True)
                    | Token::Keyword(Keyword::False)
                    | Token::Keyword(Keyword::Nil)
                    | Token::LParen
            )
        }) else {
            return Err(error_unexpected_token(self));
        };

        match token.item {
            Token::Number { value, .. } => Ok(Spanned::new(Expr::Number(value), token.span)),
            Token::String { value, .. } => {
                Ok(Spanned::new(Expr::String(value.to_string()), token.span))
            }
            Token::Keyword(Keyword::True) => Ok(Spanned::new(Expr::Bool(true), token.span)),
            Token::Keyword(Keyword::False) => Ok(Spanned::new(Expr::Bool(false), token.span)),
            Token::Keyword(Keyword::Nil) => Ok(Spanned::new(Expr::Nil, token.span)),
            Token::LParen => self.parse_group(token),
            _ => unreachable!(),
        }
    }
    fn parse_group(
        &mut self,
        lparen: Spanned<Token<'a>>,
    ) -> Result<Spanned<Expr>, ParserError<'a>> {
        fn error_no_rparen<'a>(s: &mut Parser<'a>, lparen: Spanned<Token<'a>>) -> ParserError<'a> {
            let peek = s.lexer.peek();

            let end = if let Some(Ok(token)) = peek {
                token.span.start
            } else {
                s.input.len()
            };

            let err = MissingItemError::new(
                s.input,
                lparen.span.line,
                MissingItemLocation::Token(Token::RParen),
                (lparen.span.start..end).into(),
                "Expect ')' after expression.",
            );
            ParserError::MissingItem(err)
        }

        let inner_expr = self.parse()?;

        let Some(Ok(rparen)) = self.lexer.next_if(|tok| matches!(tok, Token::RParen)) else {
            return Err(error_no_rparen(self, lparen));
        };

        let span = inner_expr
            .span
            .clone()
            .combine(&lparen.span)
            .combine(&rparen.span);
        Ok(Spanned::new(Expr::group(inner_expr), span))
    }

    fn synchronize(&mut self) {
        let matches_keyword = |tok: &Token<'_>| {
            matches!(
                tok,
                Token::Keyword(
                    Keyword::Class
                        | Keyword::Fun
                        | Keyword::Var
                        | Keyword::For
                        | Keyword::If
                        | Keyword::While
                        | Keyword::Print
                        | Keyword::Return
                )
            )
        };

        while let Some(Ok(tok)) = self.lexer.next_if(|tok| !matches_keyword(tok)) {
            if tok.item == Token::Semicolon {
                return;
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Group(Box<Spanned<Expr>>),
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl Expr {
    pub fn group(expr: Spanned<Expr>) -> Self {
        Self::Group(Box::new(expr))
    }

    pub fn binary(left: Spanned<Expr>, op: BinaryOp, right: Spanned<Expr>) -> Self {
        Self::Binary(Box::new(BinaryExpr { left, op, right }))
    }

    pub fn unary(op: UnaryOp, right: Spanned<Expr>) -> Self {
        Self::Unary(Box::new(UnaryExpr { op, right }))
    }

    pub fn eq_wo_spans(&self, other: &Expr) -> bool {
        match (self, other) {
            (Expr::Binary(f0_self), Expr::Binary(f0_other)) => f0_self.eq_wo_spans(f0_other),
            (Expr::Unary(f0_self), Expr::Unary(f0_other)) => f0_self.eq_wo_spans(f0_other),
            (Expr::Group(f0_self), Expr::Group(f0_other)) => f0_self.eq_wo_spans(f0_other),
            (Expr::String(f0_self), Expr::String(f0_other)) => f0_self.eq(f0_other),
            (Expr::Number(f0_self), Expr::Number(f0_other)) => f0_self.eq(f0_other),
            (Expr::Bool(f0_self), Expr::Bool(f0_other)) => f0_self.eq(f0_other),
            (Expr::Nil, Expr::Nil) => true,
            _unused => false,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Binary(expr) => write!(f, "({})", expr),
            Expr::Unary(expr) => write!(f, "({})", expr),
            Expr::Group(expr) => write!(f, "(group {})", expr),
            Expr::String(s) => write!(f, "{}", s),
            Expr::Number(n) if n.fract() == 0.0 => write!(f, "{}.0", n),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    pub left: Spanned<Expr>,
    pub op: BinaryOp,
    pub right: Spanned<Expr>,
}

impl BinaryExpr {
    pub fn eq_wo_spans(&self, other: &BinaryExpr) -> bool {
        self.left.eq_wo_spans(&other.left)
            && self.op.eq(&other.op)
            && self.right.eq_wo_spans(&other.right)
    }
}

impl fmt::Display for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.op, self.left, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
}

impl BinaryOp {
    fn try_from_token(token: &Token<'_>) -> Option<Self> {
        match token {
            Token::Plus => Some(BinaryOp::Add),
            Token::Minus => Some(BinaryOp::Sub),
            Token::Star => Some(BinaryOp::Mul),
            Token::Slash => Some(BinaryOp::Div),
            Token::EqEq => Some(BinaryOp::Eq),
            Token::BangEq => Some(BinaryOp::NotEq),
            Token::Lt => Some(BinaryOp::Lt),
            Token::Gt => Some(BinaryOp::Gt),
            Token::LtEq => Some(BinaryOp::LtEq),
            Token::GtEq => Some(BinaryOp::GtEq),
            _ => None,
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::LtEq => write!(f, "<="),
            BinaryOp::GtEq => write!(f, ">="),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub right: Spanned<Expr>,
}

impl UnaryExpr {
    pub fn eq_wo_spans(&self, other: &UnaryExpr) -> bool {
        self.op.eq(&other.op) && self.right.eq_wo_spans(&other.right)
    }
}

impl fmt::Display for UnaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.op, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    fn try_from_token(token: &Token<'_>) -> Option<Self> {
        match token {
            Token::Minus => Some(UnaryOp::Neg),
            Token::Bang => Some(UnaryOp::Not),
            _ => None,
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}
