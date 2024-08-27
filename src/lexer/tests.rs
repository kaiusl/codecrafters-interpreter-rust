use super::*;

#[test]
fn test_parens() {
    use Token as T;

    let mut lexer = Lexer::new("(){},.-+;*");
    let expected = [
        T::LParen,
        T::RParen,
        T::LBrace,
        T::RBrace,
        T::Comma,
        T::Dot,
        T::Minus,
        T::Plus,
        T::Semicolon,
        T::Star,
        T::Eof,
    ];
    let mut count = 0;
    for (token, expected) in lexer.by_ref().zip(&expected) {
        assert_eq!(&token.unwrap(), expected);
        count += 1;
    }

    assert_eq!(count, expected.len());
    assert!(lexer.next().is_none());
}
