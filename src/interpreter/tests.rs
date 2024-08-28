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