use super::*;

#[test]
fn test_unary_op() {
    let interpreter = Interpreter::from_str("-1").unwrap();
    assert_eq!(Object::Number(-1.0), interpreter.eval());

    let interpreter = Interpreter::from_str("!(true)").unwrap();
    assert_eq!(Object::Bool(false), interpreter.eval());

    let interpreter = Interpreter::from_str("!nil").unwrap();
    assert_eq!(Object::Bool(true), interpreter.eval());

    let interpreter = Interpreter::from_str("!1.5").unwrap();
    assert_eq!(Object::Bool(false), interpreter.eval());

    let interpreter = Interpreter::from_str("!!\"asf\"").unwrap();
    assert_eq!(Object::Bool(true), interpreter.eval());
}

#[test]
fn test_arithmetic_ops() {
    let interpreter = Interpreter::from_str("1 + 2").unwrap();
    assert_eq!(Object::Number(3.0), interpreter.eval());

    let interpreter = Interpreter::from_str("18*3/(3*6)").unwrap();
    assert_eq!(Object::Number(3.0), interpreter.eval());

    let interpreter = Interpreter::from_str("23 + 28 - (-(61 - 99))").unwrap();
    assert_eq!(Object::Number(13.0), interpreter.eval());

    let interpreter = Interpreter::from_str("\"1\" + \"2\"").unwrap();
    assert_eq!(Object::String("12".to_string()), interpreter.eval());
}
