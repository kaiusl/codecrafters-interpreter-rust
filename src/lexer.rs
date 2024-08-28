use std::borrow::Cow;
use std::fmt;

mod error;
#[cfg(test)]
mod tests;

pub use error::*;

#[derive(Debug, Clone)]
pub struct Chars<'a> {
    input: &'a str,
    inner: std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'a>>>,
}

impl<'a> Chars<'a> {
    pub fn new(input: &'a str) -> Self {
        Chars {
            input,
            inner: input.chars().enumerate().peekable(),
        }
    }

    #[inline]
    fn next_if_eq(&mut self, c: char) -> Option<(usize, char)> {
        self.inner.next_if(|(_, next)| c == *next)
    }

    #[inline]
    fn next_if_not_eq(&mut self, c: char) -> Option<(usize, char)> {
        self.inner.next_if(|(_, next)| c != *next)
    }

    #[inline]
    fn peek(&mut self) -> Option<&(usize, char)> {
        self.inner.peek()
    }

    #[inline]
    fn peek_if_eq(&mut self, c: char) -> Option<&(usize, char)> {
        self.inner.peek().filter(|(_, next)| c == *next)
    }

    #[inline]
    fn next_if<F>(&mut self, f: F) -> Option<(usize, char)>
    where
        F: Fn(char) -> bool,
    {
        self.inner.next_if(|(_, next)| f(*next))
    }
}

impl<'a> Iterator for Chars<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    chars: Chars<'a>,
    line: usize,
    eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            chars: Chars::new(input),
            line: 1,
            eof: false,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let Some((i, c)) = self.chars.next() else {
                if self.eof {
                    return None;
                } else {
                    self.eof = true;
                    return Some(Ok(Token::Eof));
                }
            };

            match c {
                '(' => return Some(Ok(Token::LParen)),
                ')' => return Some(Ok(Token::RParen)),
                '{' => return Some(Ok(Token::LBrace)),
                '}' => return Some(Ok(Token::RBrace)),
                ',' => return Some(Ok(Token::Comma)),
                '.' => return Some(Ok(Token::Dot)),
                '-' => return Some(Ok(Token::Minus)),
                '+' => return Some(Ok(Token::Plus)),
                ';' => return Some(Ok(Token::Semicolon)),
                '*' => return Some(Ok(Token::Star)),
                '!' if self.chars.next_if_eq('=').is_some() => return Some(Ok(Token::BangEq)),
                '!' => return Some(Ok(Token::Bang)),
                '=' if self.chars.next_if_eq('=').is_some() => return Some(Ok(Token::EqEq)),
                '=' => return Some(Ok(Token::Eq)),
                '<' if self.chars.next_if_eq('=').is_some() => return Some(Ok(Token::LtEq)),
                '<' => return Some(Ok(Token::Lt)),
                '>' if self.chars.next_if_eq('=').is_some() => return Some(Ok(Token::GtEq)),
                '>' => return Some(Ok(Token::Gt)),
                '/' if self.chars.next_if_eq('/').is_some() => {
                    // comment, ignore until end of line
                    while let Some((_, _)) = self.chars.next_if_not_eq('\n') {}
                    // Next character should be 'newline' or we reached the end of input
                    assert!(
                        self.chars.peek().map(|(_, c)| c) == Some(&'\n')
                            || self.chars.peek().is_none()
                    );
                }
                '/' => return Some(Ok(Token::Slash)),
                '"' => {
                    let start = i;
                    let mut end = i;
                    // TODO: support escape sequences
                    while let Some((i, c)) = self.chars.next_if_not_eq('"') {
                        end = i;
                        if c == '\n' {
                            self.line += 1;
                        }
                    }

                    if self.chars.peek().is_none() {
                        let err = UnterminatedStringError::new(
                            self.input,
                            self.line,
                            (i..end + 1).into(),
                        );
                        return Some(Err(LexerError::UnterminatedString(err)));
                    }

                    assert!(self.chars.next_if_eq('"').is_some());

                    let lexeme = &self.input[start..=end + 1];
                    let value = &self.input[start + 1..=end];

                    return Some(Ok(Token::String { lexeme, value }));
                }
                '\n' => {
                    self.line += 1;
                }
                ' ' | '\r' | '\t' => {}
                c if c.is_ascii_digit() => {
                    let start = i;
                    let mut end = i;
                    while let Some((i, _)) = self.chars.next_if(|c| c.is_ascii_digit()) {
                        end = i;
                    }

                    'decimal: {
                        let Some((i, _)) = self.chars.peek_if_eq('.') else {
                            break 'decimal;
                        };

                        let Some(c) = self.input.get(*i + 1..).and_then(|s| s.chars().next())
                        else {
                            break 'decimal;
                        };

                        if !c.is_ascii_digit() {
                            break 'decimal;
                        }

                        // floating point
                        self.chars.next(); // consume .
                        while let Some((i, _)) = self.chars.next_if(|c| c.is_ascii_digit()) {
                            end = i;
                        }
                    }

                    let lexeme = &self.input[start..=end];

                    return Some(Ok(Token::Number { lexeme, value: lexeme.parse().unwrap() }));
                }
                _ => {
                    let err =
                        UnexpectedCharacterError::new(self.input, self.line, c, (i, 1).into());
                    return Some(Err(LexerError::UnknownToken(err)));
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Slash,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    // Literals
    String { lexeme: &'a str, value: &'a str },
    Number { lexeme: &'a str, value: f64 },
    Eof,
}

impl<'a> Token<'a> {
    fn book_type_name(&self) -> &'static str {
        match self {
            Token::LParen => "LEFT_PAREN",
            Token::RParen => "RIGHT_PAREN",
            Token::LBrace => "LEFT_BRACE",
            Token::RBrace => "RIGHT_BRACE",
            Token::Comma => "COMMA",
            Token::Dot => "DOT",
            Token::Minus => "MINUS",
            Token::Plus => "PLUS",
            Token::Semicolon => "SEMICOLON",
            Token::Star => "STAR",
            Token::Eq => "EQUAL",
            Token::EqEq => "EQUAL_EQUAL",
            Token::Bang => "BANG",
            Token::BangEq => "BANG_EQUAL",
            Token::Lt => "LESS",
            Token::LtEq => "LESS_EQUAL",
            Token::Gt => "GREATER",
            Token::GtEq => "GREATER_EQUAL",
            Token::Slash => "SLASH",
            Token::String { .. } => "STRING",
            Token::Number { .. } => "NUMBER",
            Token::Eof => "EOF",
        }
    }

    fn lexeme(&self) -> &'_ str {
        match self {
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Minus => "-",
            Token::Plus => "+",
            Token::Semicolon => ";",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Eq => "=",
            Token::EqEq => "==",
            Token::Bang => "!",
            Token::BangEq => "!=",
            Token::Lt => "<",
            Token::LtEq => "<=",
            Token::Gt => ">",
            Token::GtEq => ">=",
            Token::String { lexeme, .. } | Token::Number { lexeme, .. } => lexeme,

            Token::Eof => "",
        }
    }

    fn literal(&self) -> Cow<'_, str> {
        match self {
            Token::String { value, .. } => Cow::Borrowed(value),
            Token::Number { lexeme, value, .. } => {
                if value.fract() == 0.0 {
                    Cow::Owned(format!("{}.0", value))
                } else {
                    Cow::Borrowed(lexeme)
                }
            }
            _ => Cow::Borrowed("null"),
        }
    }

    pub fn fmt_as_book(&self) -> BookTokenFmt<'_> {
        BookTokenFmt { token: self }
    }
}

#[test]
fn t() {
    println!("{}", 1.0);
}

pub struct BookTokenFmt<'a> {
    token: &'a Token<'a>,
}

impl fmt::Display for BookTokenFmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.token.book_type_name(),
            self.token.lexeme(),
            self.token.literal()
        )
    }
}
