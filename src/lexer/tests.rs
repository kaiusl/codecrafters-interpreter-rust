use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum LexerTestErr {
    UnterminatedString,
    UnknownCharacter(char),
}

fn check_tokens<'a>(lexer: &mut Lexer<'a>, expected: &[Token]) {
    let mut count = 0;

    for (t, expected) in lexer.zip(expected) {
        assert_eq!(&t.unwrap(), expected);
        count += 1;
    }

    assert_eq!(count, expected.len());
    assert!(lexer.next().is_none());
}

fn check_tokens_with_errors<'a>(lexer: &mut Lexer<'a>, expected: &[Result<Token, LexerTestErr>]) {
    let mut count = 0;

    for t in lexer.zip(expected) {
        match t {
            (Ok(t), Ok(expected)) => assert_eq!(&t, expected),
            (Err(e), Err(expected)) => match (e, expected) {
                (LexerError::UnknownToken(c), LexerTestErr::UnknownCharacter(expected)) => {
                    let tok = c.token();
                    eprintln!("{:?}", miette::Report::new(c.to_owned()));
                    assert_eq!(tok, *expected)
                }
                (LexerError::UnterminatedString(e), LexerTestErr::UnterminatedString) => {
                    eprintln!("{:?}", miette::Report::new(e.to_owned()));
                }
                (a, b) => panic!("Expected {:?} but got {:?}", a, b),
            },
            (a, b) => panic!("Expected {:?} but got {:?}", a, b),
        }
        count += 1;
    }

    assert_eq!(count, expected.len());
    assert!(lexer.next().is_none());
}

#[test]
fn test_simple_tokens() {
    use Token as T;

    let mut lexer = Lexer::new("=(!)<{>}==,!=.<=->=+;*/");
    let expected = [
        T::Eq,
        T::LParen,
        T::Bang,
        T::RParen,
        T::Lt,
        T::LBrace,
        T::Gt,
        T::RBrace,
        T::EqEq,
        T::Comma,
        T::BangEq,
        T::Dot,
        T::LtEq,
        T::Minus,
        T::GtEq,
        T::Plus,
        T::Semicolon,
        T::Star,
        T::Slash,
        T::Eof,
    ];

    check_tokens(&mut lexer, &expected);
}

#[test]
fn test_comments() {
    use Token as T;

    let mut lexer = Lexer::new("!= // some comment");
    let expected = [T::BangEq];

    check_tokens(&mut lexer, &expected);

    let mut lexer = Lexer::new("!= // some comment\n==()     // another comment");
    let expected = [T::BangEq, T::EqEq, T::LParen, T::RParen];

    check_tokens(&mut lexer, &expected);
}

#[test]
fn test_strings() {
    use Token as T;

    let mut lexer = Lexer::new("\"hello world\"");
    let expected = [T::String {
        lexeme: "\"hello world\"",
        value: "hello world",
    }];
    check_tokens(&mut lexer, &expected);

    let mut lexer = Lexer::new("\"\"");
    let expected = [T::String {
        lexeme: "\"\"",
        value: "",
    }];
    check_tokens(&mut lexer, &expected);

    let mut lexer = Lexer::new("!,\"hello\n world\".");
    let expected = [
        T::Bang,
        T::Comma,
        T::String {
            lexeme: "\"hello\n world\"",
            value: "hello\n world",
        },
        T::Dot,
    ];
    check_tokens(&mut lexer, &expected);
}

#[test]
fn test_numbers() {
    use Token as T;

    let mut lexer = Lexer::new("123");
    let expected = [T::Number { lexeme: "123", value: 123.0 }];
    check_tokens(&mut lexer, &expected);

    let mut lexer = Lexer::new("123.0");
    let expected = [T::Number { lexeme: "123.0", value: 123.0 }];
    check_tokens(&mut lexer, &expected);

    let mut lexer = Lexer::new("123.456");
    let expected = [T::Number { lexeme: "123.456", value: 123.456 }];
    check_tokens(&mut lexer, &expected);

}

#[test]
fn test_simple_errors() {
    use Token as T;

    let mut lexer = Lexer::new("!$,\"hello world\nsagsd\ngda");
    let expected = [
        Ok(T::Bang),
        Err(LexerTestErr::UnknownCharacter('$')),
        Ok(T::Comma),
        Err(LexerTestErr::UnterminatedString),
    ];
    check_tokens_with_errors(&mut lexer, &expected);
}
