use crate::builtins::{FunctionMap, Signature, add, expected};
use crate::compiler::ast_pass::Parameter;
use crate::compiler::tokens::TokenType;
use crate::compiler::tokens::TokenType::U64;
use crate::errors::RuntimeError;
use crate::value::{Value, u64};
use std::cell::RefMut;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

macro_rules! mut_list_fn {
    (mut $list:ident, mut $args:ident => $body:expr) => {
        |self_val: RefMut<Value>, mut $args: Vec<Value>| -> Result<Value, RuntimeError> {
            match self_val {
                Value::List(mut $list) => $body,
                _ => Err(expected_a_list()),
            }
        }
    };
}

macro_rules! list_fn {
    ($list:ident, $args:ident => $body:expr) => {
        |self_val: Ref<Value>, $args: Vec<Value>| -> Result<Value, RuntimeError> {
            match self_val {
                Value::List($list) => $body,
                _ => Err(expected_a_list()),
            }
        }
    };
}

pub(crate) fn list_functions() -> FunctionMap {
    let mut list_functions: FunctionMap = HashMap::new();
    let functions = &mut list_functions;
    add(functions, "len", Signature::new(vec![], U64, len));
    add(
        functions,
        "push",
        Signature::new(vec![Parameter::new("element", TokenType::Any)], U64, push),
    );
    add(
        functions,
        "remove",
        Signature::new(vec![Parameter::new("index", U64)], U64, remove),
    );
    list_functions
}

fn remove(mut self_val: RefMut<Value>, mut args: Vec<Value>) -> Result<Value, RuntimeError> {
    if let Value::List(list) = self_val.deref_mut() {
        let index= args.remove(0).cast_usize()?;
        if index >= list.len() {
            return Err(RuntimeError::IndexOutOfBounds(index, list.len()));
        }
        list.remove(index);
        Ok(self_val.deref().clone())
    } else {
        Err(expected_a_list())
    }
}

fn push(mut self_val: RefMut<Value>, mut args: Vec<Value>) -> Result<Value, RuntimeError> {
    if let Value::List(list) = self_val.deref_mut() {
        list.push(args.remove(0));
        Ok(self_val.deref().clone())
    } else {
        Err(expected_a_list())
    }
}

fn len(self_val: RefMut<Value>, _args: Vec<Value>) -> Result<Value, RuntimeError> {
    if let Value::List(list) = self_val.deref() {
        Ok(u64(list.len() as u64))
    } else {
        Err(expected_a_list())
    }
}

fn expected_a_list() -> RuntimeError {
    expected("list")
}
