use core::panic;

use super::*;

// fn test(input: &str, expected: ExprKind) {
//     let lexer = Lexer::new(input);
//     let mut parser = Parser::new(lexer);
//     let ast = parser.parse().unwrap();

//     assert_eq!(ast, expected);
// }

// #[test]
// fn test_primary() {
//     test("1", ExprKind::Number(1.0));
//     test("1.0", ExprKind::Number(1.0));
//     test("true", ExprKind::Bool(true));
//     test("false", ExprKind::Bool(false));
//     test("nil", ExprKind::Nil);
//     test("\"hello\"", ExprKind::String("hello".to_string()));
//     test("(12)", ExprKind::Group(Box::new(ExprKind::Number(12.0))));
// }

// #[test]
// fn test_unary() {
//     test(
//         "!true",
//         ExprKind::Unary(Box::new(UnaryExpr {
//             op: UnaryOp::Not,
//             right: ExprKind::Bool(true),
//         })),
//     );

//     test(
//         "-1",
//         ExprKind::Unary(Box::new(UnaryExpr {
//             op: UnaryOp::Neg,
//             right: ExprKind::Number(1.0),
//         })),
//     );
// }

// #[test]
// fn test_binary_simple() {
//     test(
//         "1 + 2",
//         ExprKind::Binary(Box::new(BinaryExpr {
//             left: ExprKind::Number(1.0),
//             op: BinaryOp::Add,
//             right: ExprKind::Number(2.0),
//         })),
//     );

//     test(
//         "1 - 2",
//         ExprKind::Binary(Box::new(BinaryExpr {
//             left: ExprKind::Number(1.0),
//             op: BinaryOp::Sub,
//             right: ExprKind::Number(2.0),
//         })),
//     );

//     test(
//         "1 * 2",
//         ExprKind::Binary(Box::new(BinaryExpr {
//             left: ExprKind::Number(1.0),
//             op: BinaryOp::Mul,
//             right: ExprKind::Number(2.0),
//         })),
//     );

//     test(
//         "1 / 2",
//         ExprKind::Binary(Box::new(BinaryExpr {
//             left: ExprKind::Number(1.0),
//             op: BinaryOp::Div,
//             right: ExprKind::Number(2.0),
//         })),
//     );

//     test(
//         "1 == 2",
//         ExprKind::Binary(Box::new(BinaryExpr {
//             left: ExprKind::Number(1.0),
//             op: BinaryOp::Eq,
//             right: ExprKind::Number(2.0),
//         })),
//     );

//     test(
//         "1 != 2",
//         ExprKind::Binary(Box::new(BinaryExpr {
//             left: ExprKind::Number(1.0),
//             op: BinaryOp::NotEq,
//             right: ExprKind::Number(2.0),
//         })),
//     );
// }

#[test]
fn test_binary_complex() {
    let lexer = Lexer::new("1 + 2 * (3 + 5)");
    let mut parser = Parser::new(lexer);
    let ast = parser.parse().unwrap();
    assert_eq!("(+ 1.0 (* 2.0 (group (+ 3.0 5.0))))", format!("{}", ast));
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn test_mix() {
    let lexer = Lexer::new("1 + !true * (!!3 + -5)");
    let mut parser = Parser::new(lexer);
    let ast = parser.parse().unwrap();
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
    let ast = parser.parse();
    match ast {
        Ok(_) => panic!("expected error"),
        Err(e) => println!("{:?}", miette::Report::new(e)),
    }
}

#[test]
fn test_errors2() {
    let lexer = Lexer::new("(3 +)");
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    match ast {
        Ok(_) => panic!("expected error"),
        Err(e) => println!("{:?}", miette::Report::new(e)),
    }
}

#[test]
fn test_errors3() {
    let lexer = Lexer::new("\"world");
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    match ast {
        Ok(_) => panic!("expected error"),
        Err(e) => println!("{:?}", miette::Report::new(e)),
    }
}
