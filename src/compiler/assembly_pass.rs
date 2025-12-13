use crate::builtins::globals::GLOBAL_FUNCTIONS;
use crate::builtins::lookup;
use crate::compiler::assembly_pass::Op::{
    Add, And, Assign, BitAnd, BitOr, BitXor, Call, CallBuiltin, Constant, DefList, DefMap, Divide,
    Dup, Equal, Get, Goto, GotoIf, GotoIfNot, Greater, GreaterEqual, Less, LessEqual, ListGet,
    Multiply, Negate, Not, NotEqual, Or, Pop, Return, Shr, Subtract,
};
use crate::compiler::ast_pass::Expression::{IfElseExpression, IfExpression, NamedParameter};
use crate::compiler::ast_pass::{Expression, Function, Parameter, Statement};
use crate::compiler::tokens::TokenType;
use crate::compiler::tokens::TokenType::Unknown;
use crate::errors::CompilerError::{IncompatibleTypes, UndeclaredVariable};
use crate::errors::{CompilerError, CompilerErrorAtLine};
use crate::symbol_builder::{Symbol, calculate_type, infer_type};
use crate::value::Value;
use crate::{AsmRegistry, SymbolTable};
use std::collections::HashMap;
use std::ops::Deref;

pub fn compile(
    qualified_name: Option<&str>,
    ast: &Vec<Statement>,
    symbols: &SymbolTable,
    registry: &mut AsmRegistry,
) -> Result<(), CompilerErrorAtLine> {
    compile_in_namespace(ast, qualified_name, symbols, registry)
}

pub fn compile_function(
    function: &Function,
    symbols: &SymbolTable,
    registry: &mut AsmRegistry,
    namespace: &str,
) -> Result<AsmChunk, CompilerErrorAtLine> {
    let fn_name = &function.name.lexeme;
    let mut compiler = AsmPass::new(fn_name);
    for parm in &function.parameters {
        let name = parm.name.lexeme.clone();
        let var_index = compiler.chunk.add_var(&parm.var_type, &parm.name.lexeme);

        compiler.vars.insert(name, var_index);
    }
    let mut chunk = compiler.compile(&function.body, symbols, registry, namespace)?;
    chunk.function_parameters = function.parameters.to_vec();
    Ok(chunk)
}

pub fn compile_in_namespace(
    ast: &Vec<Statement>,
    namespace: Option<&str>,
    symbols: &SymbolTable,
    registry: &mut AsmRegistry,
) -> Result<(), CompilerErrorAtLine> {
    let name = namespace.unwrap_or("main");
    let mut compiler = AsmPass::new(name);
    let chunk = compiler.compile(ast, symbols, registry, name)?;
    registry.insert(name.to_string(), chunk);
    Ok(())
}

#[derive(Clone)]
pub struct AsmChunk {
    pub(crate) name: String,
    pub code: Vec<Op>,
    pub constants: Vec<Value>,
    lines: Vec<usize>,
    pub(crate) object_defs: HashMap<String, Vec<Parameter>>,
    pub(crate) function_parameters: Vec<Parameter>,
    pub vars: Vec<(TokenType, String)>,
}

impl AsmChunk {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            code: Vec::new(),
            constants: vec![],
            lines: vec![],
            object_defs: HashMap::new(),
            function_parameters: vec![],
            vars: vec![],
        }
    }

    pub(crate) fn add(&mut self, op: Op, line: usize) {
        self.code.push(op);
        self.lines.push(line);
    }

    pub(crate) fn add_constant(&mut self, value: impl Into<Value>) -> usize {
        self.constants.push(value.into());
        self.constants.len() - 1
    }

    pub(crate) fn find_constant(&self, p0: &String) -> Option<usize> {
        for (i, constant) in self.constants.iter().enumerate() {
            if let Value::String(s) = constant
                && s == p0
            {
                return Some(i);
            }
        }
        None
    }

    pub(crate) fn add_var(&mut self, var_type: &TokenType, name: &str) -> usize {
        self.vars.push((var_type.clone(), name.to_string()));
        self.vars.len() - 1
    }

    pub(crate) fn add_object_def(&mut self, name: &str, fields: &[Parameter]) {
        self.object_defs.insert(name.to_string(), fields.to_vec());
    }
}

pub struct AsmPass {
    chunk: AsmChunk,
    _had_error: bool,
    current_line: usize,
    vars: HashMap<String, usize>,
}

impl AsmPass {
    pub fn new(name: &str) -> Self {
        Self {
            chunk: AsmChunk::new(name),
            _had_error: false,
            current_line: 0,
            vars: HashMap::new(),
        }
    }

    /// compile the entire AST into a chunk, adding a RETURN OP
    pub fn compile(
        &mut self,
        ast: &Vec<Statement>,
        symbols: &SymbolTable,
        registry: &mut AsmRegistry,
        namespace: &str,
    ) -> Result<AsmChunk, CompilerErrorAtLine> {
        self.compile_statements(ast, symbols, registry, namespace)?;
        self.emit(Return);
        let chunk = self.chunk.clone();
        self.chunk.code.clear(); // in case the compiler is reused, clear it for the next compilation. This is for the REPL
        Ok(chunk)
    }

    /// compile the entire AST into a chunk
    fn compile_statements(
        &mut self,
        ast: &Vec<Statement>,
        symbols: &SymbolTable,
        registry: &mut AsmRegistry,
        namespace: &str,
    ) -> Result<(), CompilerErrorAtLine> {
        for statement in ast {
            self.compile_statement(statement, symbols, registry, namespace)?;
        }
        Ok(())
    }

    /// compile a single statement
    fn compile_statement(
        &mut self,
        statement: &Statement,
        symbols: &SymbolTable,
        registry: &mut AsmRegistry,
        namespace: &str,
    ) -> Result<(), CompilerErrorAtLine> {
        self.current_line = statement.line();
        match statement {
            Statement::ExpressionStmt { expression } => {
                self.compile_expression(namespace, expression, symbols, registry)?;
            }
            Statement::FunctionStmt { function } => {
                let function_name = function.name.lexeme.clone();
                let compiled_function = compile_function(function, symbols, registry, namespace)?;
                registry.insert(
                    format!("{}/{}", self.chunk.name, function_name),
                    compiled_function,
                );
            }
            Statement::ObjectStmt { name, fields } => {
                self.chunk
                    .add_object_def(&format!("{}/{}", namespace, &name.lexeme), fields);
            }
            Statement::GuardStatement { .. } => {
                unimplemented!("guard statement")
            }
        }
        Ok(())
    }

    fn compile_expression(
        &mut self,
        namespace: &str,
        expression: &Expression,
        symbols: &SymbolTable,
        registry: &mut AsmRegistry,
    ) -> Result<(), CompilerErrorAtLine> {
        match expression {
            IfExpression {
                condition,
                then_branch,
            } => {
                self.compile_expression(namespace, condition, symbols, registry)?;

                self.emit(Dup);
                self.emit(GotoIfNot(0)); // placeholder
                let goto_addr1 = self.chunk.code.len() - 1;
                self.emit(Pop);
                self.compile_statements(then_branch, symbols, registry, namespace)?;
                self.emit(Goto(0));
                let goto_addr2 = self.chunk.code.len() - 1; // placeholder
                self.chunk.code[goto_addr1] = GotoIfNot(self.chunk.code.len());
                self.chunk.code[goto_addr2] = Goto(self.chunk.code.len());
            }
            IfElseExpression {
                condition,
                then_branch,
                else_branch,
            } => {
                self.compile_expression(namespace, condition, symbols, registry)?;

                self.emit(Dup);
                self.emit(GotoIfNot(0)); // placeholder
                let goto_addr1 = self.chunk.code.len() - 1;
                self.emit(Pop);
                self.compile_statements(then_branch, symbols, registry, namespace)?;
                self.emit(Goto(0));
                let goto_addr2 = self.chunk.code.len() - 1; // placeholder
                self.chunk.code[goto_addr1] = GotoIfNot(self.chunk.code.len());
                self.compile_statements(else_branch, symbols, registry, namespace)?;
                self.chunk.code[goto_addr2] = Op::Goto(self.chunk.code.len());
            }
            Expression::LetExpression {
                name, initializer, ..
            } => {
                let name = name.lexeme.as_str();
                let var = symbols.get(name);
                if let Some(Symbol::Variable { var_type, .. }) = var {
                    let inferred_type =
                        infer_type(initializer, symbols).map_err(|e| self.error_at_line(e))?;
                    let calculated_type = calculate_type(var_type, &inferred_type)
                        .map_err(|e| self.error_at_line(e))?;
                    if var_type != &Unknown && var_type != &calculated_type {
                        return Err(self
                            .error_at_line(IncompatibleTypes(var_type.clone(), calculated_type)));
                    }
                    let name_index = self.chunk.add_var(var_type, name);
                    self.vars.insert(name.to_string(), name_index);
                    self.compile_expression(namespace, initializer, symbols, registry)?;
                    self.emit(Assign(name_index));
                } else {
                    return Err(self.error_at_line(UndeclaredVariable(name.to_string())));
                }
            }
            Expression::FunctionCall {
                name, arguments, ..
            } => {
                let qname = format!("{}/{}", namespace, name);
                let function = symbols.get(&qname).or(symbols.get(name));
                match function {
                    Some(Symbol::Function { parameters, .. }) => {
                        let name_index = self.chunk.find_constant(&qname).unwrap_or_else(|| {
                            self.chunk.add_constant(Value::String(qname.to_string()))
                        });

                        self.get_arguments_in_order(
                            namespace, symbols, registry, arguments, parameters,
                        )?;

                        self.emit(Call(name_index, arguments.len()));
                    }
                    // constructor function
                    Some(Symbol::Object { fields, .. }) => {
                        let name_index = self.chunk.find_constant(&qname).unwrap_or_else(|| {
                            self.chunk.add_constant(Value::String(qname.to_string()))
                        });
                        self.get_arguments_in_order(
                            namespace, symbols, registry, arguments, fields,
                        )?;
                        self.emit(Call(name_index, arguments.len()));
                    }
                    // maybe global function
                    _ => {
                        if let Some(fun) = GLOBAL_FUNCTIONS.get(name) {
                            self.get_arguments_in_order(
                                namespace,
                                symbols,
                                registry,
                                arguments,
                                &fun.parameters,
                            )?;
                            let name_index = self.chunk.find_constant(name).unwrap_or_else(|| {
                                self.chunk.add_constant(Value::String(name.to_string()))
                            });
                            self.emit(Call(name_index, fun.arity()));
                        } else {
                            return Err(self
                                .error_at_line(CompilerError::FunctionNotFound(name.to_string())));
                        }
                    }
                }
            }
            Expression::MethodCall {
                receiver,
                method_name,
                arguments,
                ..
            } => {
                self.compile_expression(namespace, receiver, symbols, registry)?;
                let receiver_type = infer_type(receiver, symbols)
                    .map_err(|e| self.error_at_line(e))?
                    .to_string();

                let type_index = self.chunk.find_constant(&receiver_type).unwrap_or_else(|| {
                    self.chunk
                        .add_constant(Value::String(receiver_type.clone()))
                });

                let name_index = self.chunk.find_constant(method_name).unwrap_or_else(|| {
                    self.chunk
                        .add_constant(Value::String(method_name.to_string()))
                });
                let signature =
                    lookup(&receiver_type, method_name).map_err(|e| self.error_at_line(e))?;
                if signature.arity() != arguments.len() {
                    return Err(self.error_at_line(CompilerError::IllegalArgumentsException(
                        format!("{}.{}", receiver_type, method_name),
                        signature.parameters.len(),
                        arguments.len(),
                    )));
                }
                self.get_arguments_in_order(
                    namespace,
                    symbols,
                    registry,
                    arguments,
                    &signature.parameters,
                )?;
                self.emit(CallBuiltin(name_index, type_index, arguments.len()));
            }
            Expression::Variable { name, .. } => {
                let name_index = self.vars.get(name);
                if let Some(name_index) = name_index {
                    self.emit(Get(*name_index));
                } else {
                    return Err(self.error_at_line(UndeclaredVariable(name.to_string())));
                }
            }
            Expression::Assignment {
                variable_name,
                value,
                ..
            } => {
                self.compile_expression(namespace, value, symbols, registry)?;
                let name_index = self.vars.get(variable_name);
                if let Some(name_index) = name_index {
                    self.emit(Assign(*name_index));
                } else {
                    return Err(self.error_at_line(UndeclaredVariable(variable_name.to_string())));
                }
            }
            Expression::Literal { value, .. } => {
                self.emit_constant(value.clone());
            }
            Expression::List { values, .. } => {
                for expr in values {
                    self.compile_expression(namespace, expr, symbols, registry)?;
                }
                self.emit(DefList(values.len()));
            }
            Expression::Map { entries, .. } => {
                for (key, value) in entries {
                    self.compile_expression(namespace, key, symbols, registry)?;
                    self.compile_expression(namespace, value, symbols, registry)?;
                }
                self.emit(DefMap(entries.len()));
            }
            Expression::Grouping { expression, .. } => {
                self.compile_expression(namespace, expression, symbols, registry)?
            }
            Expression::Unary {
                operator, right, ..
            } => {
                self.compile_expression(namespace, right, symbols, registry)?;
                match operator.token_type {
                    TokenType::Minus => {
                        self.emit(Negate);
                    }
                    TokenType::Bang => {
                        self.emit(Not);
                    }
                    _ => unimplemented!("unary other than ! and -"),
                }
            }
            Expression::Binary {
                left,
                operator,
                right,
                ..
            } => {
                self.compile_expression(namespace, left, symbols, registry)?;
                self.compile_expression(namespace, right, symbols, registry)?;
                match operator.token_type {
                    TokenType::BitAnd => self.emit(BitAnd),
                    TokenType::BitXor => self.emit(BitXor),
                    TokenType::Equal => {
                        if let Expression::Variable { name, .. } = left.deref() {
                            let index = self.vars.get(name).unwrap();
                            self.emit(Assign(*index));
                            self.emit(Pop);
                        } else {
                            return Err(self.error_at_line(UndeclaredVariable("".to_string())));
                        }
                    }
                    TokenType::EqualEqual => self.emit(Equal),
                    TokenType::BangEqual => self.emit(NotEqual),
                    TokenType::Greater => self.emit(Greater),
                    TokenType::GreaterEqual => self.emit(GreaterEqual),
                    TokenType::GreaterGreater => self.emit(Shr),
                    TokenType::Less => self.emit(Less),
                    TokenType::LessEqual => self.emit(LessEqual),
                    TokenType::LessLess => self.emit(Op::Shl),
                    TokenType::LogicalAnd => self.emit(And),
                    TokenType::LogicalOr => self.emit(Or),
                    TokenType::Minus => self.emit(Subtract),
                    TokenType::Pipe => self.emit(BitOr),
                    TokenType::Plus => self.emit(Add),
                    TokenType::Slash => self.emit(Divide),
                    TokenType::Star => self.emit(Multiply),
                    _ => unimplemented!("binary other than plus, minus, star, slash"),
                }
            }
            Expression::Stop { .. } => {}
            NamedParameter { value, .. } => {
                self.compile_expression(namespace, value, symbols, registry)?
            }
            Expression::ListGet { index, list } => {
                self.compile_expression(namespace, list, symbols, registry)?;
                self.compile_expression(namespace, index, symbols, registry)?;
                self.emit(ListGet);
            }
            Expression::MapGet { .. } => {}
            Expression::FieldGet { .. } => {}
            Expression::Range { lower, upper, .. } => {
                // opposite order, because we have to assign last one first to the loop variable
                // self.compile_expression(namespace, upper, symbols, registry)?;
                // self.compile_expression(namespace, lower, symbols, registry)?;
            }
            Expression::ForStatement {
                loop_var,
                range,
                body,
            } => {
                // 1. step var index
                let step_const_index = self.emit_constant(Value::I64(1));
                // 2. range expression
                // self.compile_expression(namespace, range, symbols, registry)?;
                // //save the constants for lower and upper bounds of the range
                // let start_index = self.chunk.constants.len() - 1;
                // let end_index = self.chunk.constants.len() - 2;

                let name = loop_var.lexeme.as_str();
                let loop_var_name_index = self.chunk.add_var(&loop_var.token_type, name);
                self.vars.insert(name.to_string(), loop_var_name_index);

                // 3. start index
                let end = if let Expression::Range { lower, upper, .. } = range.deref() {
                    self.compile_expression(namespace, lower, symbols, registry)?;
                    upper.clone()
                } else {
                    unreachable!("range expression should be a range expression")
                };

                self.emit(Assign(loop_var_name_index));

                let return_addr = self.chunk.code.len();
                self.compile_statements(body, symbols, registry, namespace)?;
                self.emit(Get(loop_var_name_index));
                self.emit(Constant(step_const_index));
                self.emit(Add);
                self.emit(Assign(loop_var_name_index));
                self.compile_expression(namespace, &end, symbols, registry)?;
                self.emit(Get(loop_var_name_index));
                self.emit(GreaterEqual);
                self.emit(GotoIf(return_addr));
            }
        }
        Ok(())
    }

    // any unnamed parameters must be passed in order
    // named parameters do not have to be passed in order, but they do need to be evaluated in the order of the called function/constructor
    fn get_arguments_in_order(
        &mut self,
        namespace: &str,
        symbols: &SymbolTable,
        registry: &mut AsmRegistry,
        arguments: &[Expression],
        parameters: &[Parameter],
    ) -> Result<(), CompilerErrorAtLine> {
        for argument in arguments {
            for parameter in parameters {
                if let NamedParameter { name, value, .. } = argument {
                    if name.lexeme == parameter.name.lexeme {
                        let value_type =
                            infer_type(value, symbols).map_err(|e| self.error_at_line(e))?;
                        if parameter.var_type != value_type {
                            return Err(self.error_at_line(IncompatibleTypes(
                                parameter.var_type.clone(),
                                value_type,
                            )));
                        } else {
                            self.compile_expression(namespace, argument, symbols, registry)?;
                            break;
                        }
                    }
                } else {
                    self.compile_expression(namespace, argument, symbols, registry)?;
                    break;
                }
            }
        }
        Ok(())
    }

    fn emit(&mut self, op: Op) {
        self.chunk.add(op, self.current_line);
    }

    fn emit_constant(&mut self, value: Value) -> usize {
        let index = self.chunk.add_constant(value);
        self.emit(Constant(index));
        index
    }

    fn error_at_line(&self, error: CompilerError) -> CompilerErrorAtLine {
        CompilerErrorAtLine::raise(error, self.current_line)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Constant(usize),
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
    Call(usize, usize),
    And,
    Or,
    Not,
    Equal,
    Greater,
    Less,
    NotEqual,
    GreaterEqual,
    LessEqual,
    BitAnd,
    BitOr,
    BitXor,
    Shr,
    Shl,
    Pop,
    Get(usize),
    DefList(usize),
    DefMap(usize),
    Assign(usize),
    ListGet,
    CallBuiltin(usize, usize, usize),
    Dup,
    GotoIf(usize),
    GotoIfNot(usize),
    Goto(usize),
}
