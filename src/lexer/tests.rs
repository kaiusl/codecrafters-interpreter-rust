use super::*;

fn check_tokens(lexer: &mut Lexer, expected: &[Token]) {
    let mut count = 0;
    for (token, expected) in lexer.zip(expected) {
        assert_eq!(&token.unwrap(), expected);
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

    let mut lexer = Lexer::new("!= // some comment\n==() // another comment");
    let expected = [T::BangEq, T::EqEq, T::LParen, T::RParen];

    check_tokens(&mut lexer, &expected);
}
