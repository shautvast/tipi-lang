use std::cell::{Ref, RefMut};
use crate::builtins::{FunctionMap, Signature, add};
use crate::compiler::tokens::TokenType::{DateTime, StringType, Void};
use crate::errors::RuntimeError;
use crate::value::Value;
use std::collections::HashMap;
use std::sync::LazyLock;
use crate::compiler::ast_pass::Parameter;

pub(crate) static GLOBAL_FUNCTIONS: LazyLock<FunctionMap> = LazyLock::new(|| {
    let mut global_functions: FunctionMap = HashMap::new();
    let functions = &mut global_functions;
    add(functions, "now", Signature::new(vec![], DateTime, now));
    add(functions, "print", Signature::new(vec![Parameter::new("text", StringType)], Void, print));
    add(functions, "println", Signature::new(vec![Parameter::new("text", StringType)], Void, println));

    global_functions
});

fn println(_self_val: RefMut<Value>, args: Vec<Value>) -> Result<Value, RuntimeError> {
    print(_self_val, args)?;
    println!();
    Ok(Value::Void)
}

fn print(_self_val: RefMut<Value>, args: Vec<Value>) -> Result<Value, RuntimeError> {
    for arg in args {
        print!("{}", arg);
    }
    Ok(Value::Void)
}

fn now(_self_val: RefMut<Value>, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::DateTime(Box::new(chrono::DateTime::from(
        chrono::Utc::now(),
    ))))
}
