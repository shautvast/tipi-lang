mod string;
mod list;
pub(crate) mod globals;

use std::cell::{Ref, RefCell, RefMut};
use crate::builtins::string::string_functions;
use crate::errors::{CompilerError, RuntimeError};
use crate::compiler::tokens::TokenType;
use crate::value::Value;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::LazyLock;
use crate::compiler::ast_pass::Parameter;
use crate::builtins::list::list_functions;

pub(crate) struct Signature {
    pub(crate) parameters: Vec<Parameter>,
    pub(crate) return_type: TokenType,
    pub(crate) function: FunctionFn,
}

impl Signature {
    pub(crate) fn new(
        parameters: Vec<Parameter>,
        return_type: TokenType,
        function: FunctionFn,
    ) -> Self {
        Self {
            parameters,
            return_type,
            function,
        }
    }

    pub(crate) fn arity(&self) -> usize {
        self.parameters.len()
    }
}

pub(crate) type FunctionFn = fn(RefMut<Value>, Vec<Value>) -> Result<Value, RuntimeError>;
/// maps function names to the signature
pub(crate) type FunctionMap = HashMap<String, Signature>;
/// maps receiver type name to a function map
pub(crate) type FunctionTable = HashMap<String, FunctionMap>;

static FUNCTIONS: LazyLock<FunctionTable> = LazyLock::new(|| {
    let mut table: FunctionTable = HashMap::new();
    table.insert("string".to_string(), string_functions());
    table.insert("list".to_string(), list_functions());

    table
});

pub(crate) fn add(m: &mut FunctionMap, name: &str, method: Signature) {
    m.insert(name.to_string(), method);
}

pub(crate) fn lookup(type_name: &str, method_name: &str) -> Result<&'static Signature, CompilerError> {
     FUNCTIONS
        .get(type_name)
        .and_then(|methods| methods.get(method_name))
        .ok_or_else(|| CompilerError::FunctionNotFound(format!("{}.{}", type_name, method_name)))
}

pub(crate) fn call(
    type_name: &str,
    method_name: &str,
    self_val: Rc<RefCell<Value>>,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    (lookup(type_name,method_name).map_err(|e|RuntimeError::FunctionNotFound(e.to_string()))?.function)(self_val.borrow_mut(), args)
}

pub(crate) fn expected(expected_type: &str) -> RuntimeError {
    RuntimeError::ExpectedType(expected_type.to_string())
}
