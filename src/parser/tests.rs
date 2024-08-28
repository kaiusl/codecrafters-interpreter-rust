use super::*;

fn test(input: &str, expected: Expr) {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse().unwrap();

    assert_eq!(ast, expected);
}

#[test]
fn test_primary() {
    test("1", Expr::Number(1.0));
    test("1.0", Expr::Number(1.0));
    test("true", Expr::Bool(true));
    test("false", Expr::Bool(false));
    test("nil", Expr::Nil);
    test("\"hello\"", Expr::String("hello".to_string()));
    test("(12)", Expr::Group(Box::new(Expr::Number(12.0))));
}

#[test]
fn test_unary() {
    test(
        "!true",
        Expr::Unary(Box::new(UnaryExpr {
            op: UnaryOp::Not,
            right: Expr::Bool(true),
        })),
    );

    test(
        "-1",
        Expr::Unary(Box::new(UnaryExpr {
            op: UnaryOp::Neg,
            right: Expr::Number(1.0),
        })),
    );
}
