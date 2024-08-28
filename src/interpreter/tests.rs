use rstest::rstest;

use super::*;

fn test_eval(input: &str, expected: Object) {
    let result = super::eval(input).unwrap().unwrap();
    assert_eq!(result, expected);
}

#[test]
fn test_unary_op() {
    test_eval("-1", Object::Number(-1.0));
    test_eval("!(true)", Object::Bool(false));
    test_eval("!(nil)", Object::Bool(true));
    test_eval("!(1.5)", Object::Bool(false));
    test_eval("!!\"asf\"", Object::Bool(true));
}

#[test]
fn test_arithmetic_ops() {
    test_eval("1 + 2", Object::Number(3.0));
    test_eval("1 - 2", Object::Number(-1.0));
    test_eval("23 + 28 - (-(61 - 99))", Object::Number(13.0));
    test_eval("\"1\" + \"2\"", Object::String("12".to_string()));
}

#[test]
fn test_relational_ops() {
    test_eval("1 < 2", Object::Bool(true));
    test_eval("1 > 2", Object::Bool(false));
    test_eval("(1+6) <= (2*6)", Object::Bool(true));
}

#[test]
fn test_equality_ops() {
    test_eval("1 == 1", Object::Bool(true));
    test_eval("1 == 2", Object::Bool(false));
    test_eval("true == true", Object::Bool(true));
    test_eval("\"sf\" == 5", Object::Bool(false));
}

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
