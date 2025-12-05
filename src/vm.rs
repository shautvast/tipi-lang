use crate::AsmRegistry;
use crate::builtins::globals::GLOBAL_FUNCTIONS;
use crate::compiler::assembly_pass::{AsmChunk, Op};
use crate::compiler::tokens::TokenType;
use crate::errors::{RuntimeError, ValueError};
use crate::value::{Object, Value};
use arc_swap::Guard;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::debug;

pub async fn interpret_async(
    registry: Guard<Arc<HashMap<String, AsmChunk>>>,
    function: &str,
    uri: &str,
    query_params: HashMap<String, String>,
    headers: HashMap<String, String>,
) -> Result<Value, RuntimeError> {
    let chunk = registry.get(function);
    if let Some(chunk) = chunk {
        let mut vm = Vm::new(&registry);
        vm.local_vars
            .insert("path".to_string(), Value::String(uri.into()));
        vm.local_vars
            .insert("query".to_string(), Value::Map(value_map(query_params)));
        vm.local_vars
            .insert("headers".to_string(), Value::Map(value_map(headers)));
        vm.run(&get_context(function), chunk)
    } else {
        Err(RuntimeError::FunctionNotFound(function.to_string()))
    }
}

pub fn interpret(registry: Guard<Arc<AsmRegistry>>, function: &str) -> Result<Value, RuntimeError> {
    let chunk = registry.get(function).unwrap().clone();
    let mut vm = Vm::new(&registry);
    vm.run(&get_context(function), &chunk)
}

pub fn interpret_function(chunk: &AsmChunk, args: Vec<Value>) -> Result<Value, RuntimeError> {
    let mut vm = Vm::new(&Arc::new(HashMap::new()));
    vm.run_function(chunk, args)
}

pub(crate) struct Vm {
    ip: usize,
    stack: Vec<Value>,
    local_vars: HashMap<String, Value>,
    error_occurred: bool,
    pub(crate) registry: Arc<AsmRegistry>,
}

impl Vm {
    pub(crate) fn new(registry: &Arc<AsmRegistry>) -> Self {
        Self {
            ip: 0,
            stack: vec![],
            local_vars: HashMap::new(),
            error_occurred: false,
            registry: registry.clone(),
        }
    }

    fn run_function(
        &mut self,
        chunk: &AsmChunk,
        mut args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // arguments -> locals
        for (_, name) in chunk.vars.iter() {
            self.local_vars.insert(name.clone(), args.remove(0));
        }
        self.run("", chunk)
    }

    pub(crate) fn run(&mut self, context: &str, chunk: &AsmChunk) -> Result<Value, RuntimeError> {
        self.ip = 0;
        loop {
            let opcode = &chunk.code[self.ip];
            self.ip += 1;
            match opcode {
                Op::Constant(c) => {
                    let value = &chunk.constants[*c];
                    self.push(value.clone());
                }
                Op::Add => binary_op(self, |a, b| a + b),
                Op::Subtract => binary_op(self, |a, b| a - b),
                Op::Multiply => binary_op(self, |a, b| a * b),
                Op::Divide => binary_op(self, |a, b| a / b),
                Op::And => binary_op(self, |a, b| {
                    if let (Value::Bool(a), Value::Bool(b)) = (a, b) {
                        Ok(Value::Bool(*a && *b))
                    } else {
                        Err(ValueError::Some("Cannot and"))
                    }
                }),
                Op::Or => binary_op(self, |a, b| {
                    if let (Value::Bool(a), Value::Bool(b)) = (a, b) {
                        Ok(Value::Bool(*a || *b))
                    } else {
                        Err(ValueError::Some("Cannot compare"))
                    }
                }),
                Op::Not => unary_op(self, |a| !a),
                Op::BitAnd => binary_op(self, |a, b| a & b),
                Op::BitOr => binary_op(self, |a, b| a | b),
                Op::BitXor => binary_op(self, |a, b| a ^ b),
                Op::Negate => unary_op(self, |a| -a),
                Op::Return => {
                    return if self.stack.is_empty() {
                        Ok(Value::Void)
                    } else {
                        Ok(self.pop())
                    };
                }
                Op::Shl => binary_op(self, |a, b| a << b),
                Op::Shr => binary_op(self, |a, b| a >> b),
                Op::Equal => binary_op(self, |a, b| Ok(Value::Bool(a == b))),
                Op::Greater => binary_op(self, |a, b| Ok(Value::Bool(a > b))),
                Op::GreaterEqual => binary_op(self, |a, b| Ok(Value::Bool(a >= b))),
                Op::Less => binary_op(self, |a, b| Ok(Value::Bool(a < b))),
                Op::LessEqual => binary_op(self, |a, b| Ok(Value::Bool(a <= b))),
                Op::NotEqual => binary_op(self, |a, b| Ok(Value::Bool(a != b))),
                Op::Print => {
                    debug!("print {:?}", self.stack);
                    let v = self.pop();
                    println!("{}", v);
                }
                Op::DefList(len) => {
                    let mut list = vec![];
                    for _ in 0..*len {
                        let value = self.pop();
                        list.push(value);
                    }
                    list.reverse();
                    self.push(Value::List(list));
                }
                Op::Assign(var_index) => {
                    let (var_type, name) = chunk.vars.get(*var_index).unwrap();
                    let value = self.pop();
                    let value = number(var_type, value)?;
                    self.local_vars.insert(name.to_string(), value); //insert or update
                }
                Op::DefMap(len) => {
                    let mut map = HashMap::new();
                    for _ in 0..*len {
                        let value = self.pop();
                        let key = self.pop();
                        map.insert(key, value);
                    }
                    self.push(Value::Map(map));
                }
                Op::Get(var_index) => {
                    let (_, name_index) = chunk.vars.get(*var_index).unwrap();
                    let value = self.local_vars.get(name_index).unwrap().clone();
                    self.push(value);
                }
                Op::ListGet => {
                    let index = self.pop().cast_usize()?;
                    let list = self.pop();
                    if let Value::List(list) = list {
                        if list.len() <= index {
                            return Err(RuntimeError::IndexOutOfBounds(list.len(), index));
                        } else {
                            self.push(list.get(index).cloned().unwrap())
                        }
                    }
                }
                Op::CallBuiltin(function_name_index, function_type_index, num_args) => {
                    let function_name = chunk.constants[*function_name_index].to_string();
                    let receiver_type_name = chunk.constants[*function_type_index].to_string();

                    let mut args = vec![];
                    for _ in 0..*num_args {
                        let arg = self.pop();
                        args.push(arg);
                    }
                    args.reverse();
                    let receiver = self.pop();
                    let return_value =
                        crate::builtins::call(&receiver_type_name, &function_name, receiver, args)?;
                    self.push(return_value);
                }
                Op::Pop => {
                    self.pop();
                }
                Op::Call(function_name_index, num_args) => {
                    let mut args = vec![];
                    for _ in 0..*num_args {
                        let arg = self.pop();
                        args.push(arg);
                    }
                    args.reverse();

                    let function_name = chunk.constants[*function_name_index].to_string();
                    if let Some(fun) = GLOBAL_FUNCTIONS.get(&function_name) {
                        let return_value = (fun.function)(Value::Void, args)?;
                        self.push(return_value);
                    } else {
                        let function_chunk = self.registry.get(&function_name).or_else(|| {
                            self.registry.get(&format!("{}/{}", context, function_name))
                        });

                        if function_chunk.is_none() {
                            let constructor = chunk.object_defs.get(&function_name);

                            if let Some(params) = constructor {
                                if params.len() != args.len() {
                                    return Err(RuntimeError::IllegalArgumentsException(
                                        function_name,
                                        params.len(),
                                        args.len(),
                                    ));
                                }

                                let mut fields = vec![];
                                params.iter().zip(args).for_each(|(param, arg)| {
                                    fields.push((param.name.lexeme.clone(), arg))
                                });
                                let new_instance = Value::ObjectType(Box::new(Object {
                                    definition: function_name,
                                    fields,
                                }));
                                self.push(new_instance);
                            } else {
                                return Err(RuntimeError::FunctionNotFound(function_name));
                            }
                        } else {
                            let result = interpret_function(function_chunk.unwrap(), args)?;
                            self.push(result);
                        }
                    }
                }
                Op::GotoIfNot(goto_addr) => {
                    let b = self.pop();
                    if b == Value::Bool(false) {
                        self.ip = *goto_addr;
                    }
                }
                Op::GotoIf(goto_addr) => {
                    let b = self.pop();
                    if b == Value::Bool(true) {
                        self.ip = *goto_addr;
                    }
                }
                Op::Goto(goto_addr) => {
                    self.ip = *goto_addr;
                }
                Op::Dup => {
                    let value = self.pop();
                    self.push(value.clone());
                    self.push(value);
                }
            }
        }
    }

    fn push(&mut self, value: Value) {
        if value != Value::Void {
            self.stack.push(value);
        }
    }

    fn pop(&mut self) -> Value {
        self.stack
            .pop()
            .unwrap_or_else(|| Value::Error("Stack underflow".to_string()))
    }
}

fn binary_op(vm: &mut Vm, op: impl Fn(&Value, &Value) -> Result<Value, ValueError> + Copy) {
    let b = vm.pop();
    let a = vm.pop();

    let result = op(&a, &b);
    match result {
        Ok(result) => vm.push(result),
        Err(e) => {
            vm.error_occurred = true;
            println!("Error: {} {:?} and {:?}", e, a, b);
        }
    }
}

fn unary_op(stack: &mut Vm, op: impl Fn(&Value) -> Result<Value, ValueError> + Copy) {
    let a = stack.pop();
    let result = op(&a);
    match result {
        Ok(result) => stack.push(result),
        Err(e) => panic!("Error: {:?} {:?}", e, a),
    }
}

pub(crate) fn get_context(path: &str) -> String {
    let mut parts: Vec<&str> = path.split('/').collect();
    if parts.len() >= 2 {
        parts.truncate(parts.len() - 2);
    }
    parts.join("/")
}

fn number(var_type: &TokenType, value: Value) -> Result<Value, RuntimeError> {
    let value = match var_type {
        TokenType::U32 => value.cast_u32()?,
        TokenType::U64 => value.cast_u64()?,
        TokenType::F32 => value.cast_f32()?,
        TokenType::I32 => value.cast_i32()?,
        _ => value,
    };
    Ok(value)
}

fn value_map(strings: HashMap<String, String>) -> HashMap<Value, Value> {
    strings
        .into_iter()
        .map(|(k, v)| (Value::String(k.to_string()), Value::String(v.to_string())))
        .collect()
}
