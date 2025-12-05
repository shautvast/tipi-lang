use std::cell::{Ref, RefMut};
use crate::builtins::{FunctionMap, Parameter, Signature, add, expected};
use crate::errors::RuntimeError;
use crate::compiler::tokens::TokenType::{StringType, U64};
use crate::value::{Value, bool, string, u64};
use regex::Regex;
use std::collections::HashMap;

pub(crate) fn string_functions() -> FunctionMap {
    let mut string_functions: FunctionMap = HashMap::new();
    let functions = &mut string_functions;
    add(functions, "len", Signature::new(vec![], U64, string_len));
    add(
        functions,
        "to_uppercase",
        Signature::new(vec![], StringType, string_to_uppercase),
    );
    add(
        functions,
        "to_lowercase",
        Signature::new(vec![], StringType, string_to_lowercase),
    );
    add(functions, "contains", Signature::new(vec![Parameter::new("key", StringType)], StringType, string_contains));
    add(functions, "reverse", Signature::new(vec![], StringType, string_reverse));
    add(functions, "trim", Signature::new(vec![], StringType, string_trim));
    add(
        functions,
        "trim_start",
        Signature::new(vec![], StringType, string_trim_start),
    );
    add(functions, "trim_end", Signature::new(vec![], StringType, string_trim_end));
    add(
        functions,
        "replace_all",
        Signature::new(
            vec![
                Parameter::new("pattern", StringType),
                Parameter::new("replacement", StringType),
            ],
            StringType,
            string_replace_all,
        ),
    );
    string_functions
}

fn string_len(self_val: RefMut<Value>, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    match self_val.clone() {
        Value::String(s) => Ok(u64(s.len() as u64)),
        _ => Err(expected_a_string()),
    }
}

fn string_to_uppercase(self_val: RefMut<Value>, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    match self_val.clone() {
        Value::String(s) => Ok(string(s.to_uppercase())),
        _ => Err(expected_a_string()),
    }
}

fn string_to_lowercase(self_val: RefMut<Value>, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    match self_val.clone() {
        Value::String(s) => Ok(string(s.to_lowercase())),
        _ => Err(expected_a_string()),
    }
}

fn string_contains(self_val: RefMut<Value>, args: Vec<Value>) -> Result<Value, RuntimeError> {
    match (self_val.clone(), args.first()) {
        (Value::String(s), Some(Value::String(pat))) => Ok(bool(s.contains(pat.as_str()))),
        _ => Err(expected_a_string()),
    }
}

fn string_reverse(self_val: RefMut<Value>, _: Vec<Value>) -> Result<Value, RuntimeError> {
    match self_val.clone() {
        Value::String(s) => Ok(s.chars().rev().collect::<String>().into()),
        _ => Err(expected_a_string()),
    }
}

fn string_trim(self_val: RefMut<Value>, _: Vec<Value>) -> Result<Value, RuntimeError> {
    match self_val.clone() {
        Value::String(s) => Ok(string(s.trim())),
        _ => Err(expected_a_string()),
    }
}

fn string_trim_start(self_val: RefMut<Value>, _: Vec<Value>) -> Result<Value, RuntimeError> {
    match self_val.clone() {
        Value::String(s) => Ok(string(s.trim_start())),
        _ => Err(expected_a_string()),
    }
}

fn string_trim_end(self_val: RefMut<Value>, _: Vec<Value>) -> Result<Value, RuntimeError> {
    match self_val.clone() {
        Value::String(s) => Ok(string(s.trim_end())),
        _ => Err(expected_a_string()),
    }
}
fn string_replace_all(receiver: RefMut<Value>, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let pattern = if let Value::String(s) = &args[0] {
        Regex::new(s).map_err(|_| RuntimeError::IllegalArgumentException("Invalid regex".into()))?
    } else {
        return Err(RuntimeError::IllegalArgumentException(
            format!("Illegal pattern. Expected a string, but got {}", &args[0]),
        ));
    };
    let replacement = if let Value::String(repl) = &args[1] {
        repl
    } else {
        return Err(RuntimeError::IllegalArgumentException(
            format!(
                "Illegal replacement. Expected a string but got {}",
                &args[1]
            )
        ));
    };
    match receiver.clone() {
        Value::String(ref str) => Ok(string(pattern.replace_all(str, replacement))),
        _ => Err(expected_a_string()),
    }
}

fn expected_a_string() -> RuntimeError {
    expected("string")
}
