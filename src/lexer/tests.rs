use super::*;

#[test]
fn test_book_fmt() {
    let lexer = Lexer::new("()");
    let expected = ["LEFT_PAREN ( null", "RIGHT_PAREN ) null", "EOF  null"];
    for (token, expected) in lexer.zip(expected) {
        assert_eq!(token.unwrap().fmt_as_book().to_string(), expected);
    }
}

#[test]
fn test_parens() {
    use Token as T;

    let mut lexer = Lexer::new("(()");
    let expected = [T::LParen, T::LParen, T::RParen, T::Eof];
    let mut count = 0;
    for (token, expected) in lexer.by_ref().zip(&expected) {
        assert_eq!(&token.unwrap(), expected);
        count += 1;
    }

    assert_eq!(count, expected.len());
    assert!(lexer.next().is_none());
}
