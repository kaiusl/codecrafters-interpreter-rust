use core::fmt;

use crate::lexer::{Keyword, Lexer, LexerError, Token, TokenMeta};

mod error;
#[cfg(test)]
mod tests;

pub use error::*;

type LexerReturnToken<'a> = (Token<'a>, TokenMeta);

struct PeekableLexer<'a> {
    lexer: Lexer<'a>,
    peeked: Option<Result<LexerReturnToken<'a>, LexerError<'a>>>,
}

impl<'a> PeekableLexer<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            peeked: None,
        }
    }

    fn peek(&mut self) -> Option<&Result<LexerReturnToken<'a>, LexerError<'a>>> {
        if self.peeked.is_none() {
            self.peeked = self.lexer.next();
        }
        self.peeked.as_ref()
    }

    fn next_if<F>(&mut self, f: F) -> Option<Result<LexerReturnToken<'a>, LexerError<'a>>>
    where
        F: Fn(&Token<'a>) -> bool,
    {
        if let Some(peeked) = self.peek().and_then(|res| res.as_ref().ok()) {
            if f(&peeked.0) {
                return self.peeked.take();
            }
        }
        None
    }

    fn line(&self) -> usize {
        self.lexer.line()
    }
}

impl<'a> Iterator for PeekableLexer<'a> {
    type Item = Result<LexerReturnToken<'a>, LexerError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.peeked.is_some() {
            self.peeked.take()
        } else {
            self.lexer.next()
        }
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

    pub fn parse(&mut self) -> Result<Expr, ParserError<'a>> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Expr, ParserError<'a>> {
        let mut expr = self.parse_comparison()?;

        while let Some(token) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::EqEq | Token::BangEq))
        {
            let op = BinaryOp::try_from_token(&token?.0).unwrap();
            let right = self.parse_comparison()?;
            expr = Expr::Binary(Box::new(BinaryExpr {
                left: expr,
                op,
                right,
            }));
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError<'a>> {
        let mut expr = self.parse_term()?;

        while let Some(token) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::Gt | Token::Lt | Token::GtEq | Token::LtEq))
        {
            let op = BinaryOp::try_from_token(&token?.0).unwrap();
            let right = self.parse_term()?;
            expr = Expr::Binary(Box::new(BinaryExpr {
                left: expr,
                op,
                right,
            }));
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParserError<'a>> {
        let mut expr = self.parse_factor()?;

        while let Some(token) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::Minus | Token::Plus))
        {
            let op = BinaryOp::try_from_token(&token?.0).unwrap();
            let right = self.parse_factor()?;
            expr = Expr::Binary(Box::new(BinaryExpr {
                left: expr,
                op,
                right,
            }));
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError<'a>> {
        let mut expr = self.parse_unary()?;

        while let Some(token) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::Slash | Token::Star))
        {
            let op = BinaryOp::try_from_token(&token?.0).unwrap();
            let right = self.parse_unary()?;
            expr = Expr::Binary(Box::new(BinaryExpr {
                left: expr,
                op,
                right,
            }));
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParserError<'a>> {
        if let Some(token) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::Bang | Token::Minus))
        {
            let op = UnaryOp::try_from_token(&token?.0).unwrap();
            let right = self.parse_unary()?;
            return Ok(Expr::Unary(Box::new(UnaryExpr { op, right })));
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError<'a>> {
        let Some(Ok((token, token_meta))) = self.lexer.next_if(|tok| {
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
            let (token, token_meta, len) = match self.lexer.peek() {
                Some(Ok((token, token_mtea))) => (
                    &MissingItemLocation::Token(token.clone()),
                    token_mtea,
                    token.lexeme().len(),
                ),
                Some(Err(e)) => return Err(e.clone().into()),
                None => (
                    &MissingItemLocation::End,
                    &TokenMeta {
                        line: self.lexer.line(),
                        start: self.input.len() - 1,
                    },
                    1,
                ),
            };

            let err = MissingItemError::new(
                self.input,
                token_meta.line,
                token.clone(),
                (token_meta.start, len).into(),
                "Expect expression.",
            );
            return Err(ParserError::MissingItem(err));
        };
        match token {
            Token::Number { value, .. } => Ok(Expr::Number(value)),
            Token::String { value, .. } => Ok(Expr::String(value.to_string())),
            Token::Keyword(Keyword::True) => Ok(Expr::Bool(true)),
            Token::Keyword(Keyword::False) => Ok(Expr::Bool(false)),
            Token::Keyword(Keyword::Nil) => Ok(Expr::Nil),
            Token::LParen => {
                let expr = self.parse()?;
                if let Some(Ok(_)) = self.lexer.next_if(|tok| matches!(tok, Token::RParen)) {
                } else {
                    let peek = self.lexer.peek();

                    let end = if let Some(Ok((_, meta))) = peek {
                        meta.start
                    } else {
                        self.input.len()
                    };

                    let err = MissingItemError::new(
                        self.input,
                        token_meta.line,
                        MissingItemLocation::Token(Token::RParen),
                        (token_meta.start..end).into(),
                        "Expect ')' after expression.",
                    );
                    return Err(ParserError::MissingItem(err));
                }
                Ok(Expr::Group(Box::new(expr)))
            }
            _ => unreachable!(),
        }
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

        while let Some(Ok((tok, _))) = self.lexer.next_if(|tok| !matches_keyword(tok)) {
            if tok == Token::Semicolon {
                return;
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Group(Box<Expr>),
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
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
    pub left: Expr,
    pub op: BinaryOp,
    pub right: Expr,
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
    pub right: Expr,
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
