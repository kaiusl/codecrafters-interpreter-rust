use rstest::rstest;

use super::*;

// fn test_eval(input: &str, expected: Object) {
//     let result = super::eval(input).unwrap().unwrap();
//     assert_eq!(result.item, expected);
// }

// #[test]
// fn test_unary_op() {
//     test_eval("-1", Object::Number(-1.0));
//     test_eval("!(true)", Object::Bool(false));
//     test_eval("!(nil)", Object::Bool(true));
//     test_eval("!(1.5)", Object::Bool(false));
//     test_eval("!!\"asf\"", Object::Bool(true));
// }

// #[test]
// fn test_arithmetic_ops() {
//     test_eval("1 + 2", Object::Number(3.0));
//     test_eval("1 - 2", Object::Number(-1.0));
//     test_eval("23 + 28 - (-(61 - 99))", Object::Number(13.0));
//     test_eval("\"1\" + \"2\"", Object::String("12".to_string()));
// }

// #[test]
// fn test_relational_ops() {
//     test_eval("1 < 2", Object::Bool(true));
//     test_eval("1 > 2", Object::Bool(false));
//     test_eval("(1+6) <= (2*6)", Object::Bool(true));
// }

// #[test]
// fn test_equality_ops() {
//     test_eval("1 == 1", Object::Bool(true));
//     test_eval("1 == 2", Object::Bool(false));
//     test_eval("true == true", Object::Bool(true));
//     test_eval("\"sf\" == 5", Object::Bool(false));
//     test_eval("\"world\" == \"world\"", Object::Bool(true));
// }

#[rstest]
#[should_panic(expected = "Operand must be a number")]
#[case("-\"fsa\"")]
#[should_panic(expected = "Operand must be a number")]
#[case("-true")]
#[should_panic(expected = "Operand must be a number")]
#[case("1 > -true")]
#[should_panic(expected = "Operands must be numbers")]
#[case("1 - true")]
fn test_unary_errors(#[case] input: &str) {
    super::eval(input).unwrap().unwrap();
}

#[test]
fn test_print() {
    super::interpret("print 10;").unwrap().unwrap();
    super::interpret("print \"abc\" + \"def\";")
        .unwrap()
        .unwrap();
}

#[test]
fn test_global_var() {
    super::interpret("var bb = 10; print bb;").unwrap().unwrap();
    super::interpret("var aa = true; var bb = 3; print bb; print aa;")
        .unwrap()
        .unwrap();
    super::interpret("var aa = true; var aa = 3; print aa; print aa;")
        .unwrap()
        .unwrap();
}

#[test]
#[should_panic(expected = "Undefined variable 'bb'")]
fn test_undefined_variable() {
    super::interpret("print bb;").unwrap().unwrap();
}


#[test]
fn test_assignment() {
    super::interpret("var bb = 10; bb = true; print bb;").unwrap().unwrap();
}