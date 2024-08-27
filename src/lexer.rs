use std::borrow::Cow;
use std::fmt;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    chars: std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'a>>>,
    line: usize,
    eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            chars: input.chars().enumerate().peekable(),
            line: 1,
            eof: false,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = anyhow::Result<Token>;

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
                '\n' => {
                    self.line += 1;
                }
                _ => return Some(Err(anyhow::anyhow!("Unknown token: {}", c))),
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    LParen,
    RParen,
    Eof,
}

impl Token {
    fn book_type_name(&self) -> &'static str {
        match self {
            Token::LParen => "LEFT_PAREN",
            Token::RParen => "RIGHT_PAREN",
            Token::Eof => "EOF",
        }
    }

    fn lexeme(&self) -> &'static str {
        match self {
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Eof => "",
        }
    }

    fn literal(&self) -> Cow<'static, str> {
        match self {
            Token::LParen | Token::RParen | Token::Eof => Cow::Borrowed("null"),
            _ => todo!(),
        }
    }

    pub fn fmt_as_book(&self) -> BookTokenFmt<'_> {
        BookTokenFmt { token: self }
    }
}

pub struct BookTokenFmt<'a> {
    token: &'a Token,
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
