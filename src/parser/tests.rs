use core::panic;

use super::*;

fn test_expr(input: &str, expected: Expr) {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_expr().unwrap();

    if ast.item.eq_wo_spans(&expected) {
    } else {
        assert_eq!(ast.item, expected);
    }
}

impl<T> Spanned<T> {
    fn wo_span(item: T) -> Self {
        Self::new(item, Span::new(0, 0, 0))
    }
}

impl Expr {
    fn groupt(item: Expr) -> Self {
        Self::group(Spanned::wo_span(item))
    }

    fn binaryt(left: Expr, op: BinaryOp, right: Expr) -> Self {
        Self::binary(Spanned::wo_span(left), op, Spanned::wo_span(right))
    }

    fn unaryt(op: UnaryOp, right: Expr) -> Self {
        Self::unary(op, Spanned::wo_span(right))
    }
}

#[test]
fn test_primary() {
    test_expr("1", Expr::Number(1.0));
    test_expr("1.0", Expr::Number(1.0));
    test_expr("true", Expr::Bool(true));
    test_expr("false", Expr::Bool(false));
    test_expr("nil", Expr::Nil);
    test_expr("\"hello\"", Expr::String("hello".to_string()));
    test_expr("(12)", Expr::groupt(Expr::Number(12.0)));
}

#[test]
fn test_unary() {
    test_expr("!true", Expr::unaryt(UnaryOp::Not, Expr::Bool(true)));
    test_expr("-1", Expr::unaryt(UnaryOp::Neg, Expr::Number(1.0)));
}

#[test]
fn test_binary_simple() {
    test_expr(
        "1 + 2",
        Expr::binaryt(Expr::Number(1.0), BinaryOp::Add, Expr::Number(2.0)),
    );

    test_expr(
        "1 - 2",
        Expr::binaryt(Expr::Number(1.0), BinaryOp::Sub, Expr::Number(2.0)),
    );

    test_expr(
        "1 * 2",
        Expr::binaryt(Expr::Number(1.0), BinaryOp::Mul, Expr::Number(2.0)),
    );

    test_expr(
        "1 / 2",
        Expr::binaryt(Expr::Number(1.0), BinaryOp::Div, Expr::Number(2.0)),
    );

    test_expr(
        "1 == 2",
        Expr::binaryt(Expr::Number(1.0), BinaryOp::Eq, Expr::Number(2.0)),
    );

    test_expr(
        "1 != 2",
        Expr::binaryt(Expr::Number(1.0), BinaryOp::NotEq, Expr::Number(2.0)),
    );
}

#[test]
fn test_binary_complex() {
    let lexer = Lexer::new("1 + 2 * (3 + 5)");
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_expr().unwrap();
    assert_eq!("(+ 1.0 (* 2.0 (group (+ 3.0 5.0))))", format!("{}", ast));
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn test_mix() {
    let lexer = Lexer::new("1 + !true * (!!3 + -5)");
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_expr().unwrap();
    assert_eq!(
        "(+ 1.0 (* (! true) (group (+ (! (! 3.0)) (- 5.0)))))",
        format!("{}", ast)
    );
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn test_errors() {
    let lexer = Lexer::new("(3 + 5");
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_expr();
    match ast {
        Ok(_) => panic!("expected error"),
        Err(e) => println!("{:?}", miette::Report::new(e)),
    }
}

#[test]
fn test_errors2() {
    let lexer = Lexer::new("(3 +)");
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_expr();
    match ast {
        Ok(_) => panic!("expected error"),
        Err(e) => println!("{:?}", miette::Report::new(e)),
    }
}

#[test]
fn test_errors3() {
    let lexer = Lexer::new("\"world");
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_expr();
    match ast {
        Ok(_) => panic!("expected error"),
        Err(e) => println!("{:?}", miette::Report::new(e)),
    }
}

#[test]
fn test_print() {
    let lexer = Lexer::new("print 10;");
    let mut parser = Parser::new(lexer);
    let (ast, errors) = parser.parse();

    assert!(errors.is_empty());
    assert_eq!("print 10.0;", ast[0].to_string());

    insta::assert_debug_snapshot!(ast);
}

#[test]
fn test_print2() {
    let lexer = Lexer::new("\"asf\" + \"bc\";");
    let mut parser = Parser::new(lexer);
    let (ast, errors) = parser.parse();
    assert!(errors.is_empty());
    assert_eq!("(+ asf bc);", ast[0].to_string());

    insta::assert_debug_snapshot!(ast);
}

#[test]
fn test_print4() {
    let lexer = Lexer::new("print;");
    let mut parser = Parser::new(lexer);
    let (ast, errors) = parser.parse();

    assert!(!errors.is_empty());
    assert!(ast.is_empty());
}

#[test]
fn test_var() {
    let mut parser = Parser::from_str("var a = 10; print a;");
    let (ast, errors) = parser.parse();

    assert!(errors.is_empty());
    assert_eq!("var a = 10.0;", ast[0].to_string());
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn test_assignment() {
    let mut parser = Parser::from_str("var a; a = 10; print a;");
    let (ast, errors) = parser.parse();

    assert!(errors.is_empty());
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn test_block() {
    let mut parser = Parser::from_str("var a = 10; { print a; var b = 15; }");
    let (ast, errors) = parser.parse();

    assert!(errors.is_empty());
    insta::assert_debug_snapshot!(ast);
}
