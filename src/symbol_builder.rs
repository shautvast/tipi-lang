use crate::compiler::ast_pass::{Expression, Parameter, Statement};
use crate::builtins::lookup;
use crate::errors::CompilerError;
use crate::errors::CompilerError::{IncompatibleTypes, UndeclaredVariable};
use crate::compiler::tokens::TokenType::{Bool, DateTime, F32, F64, FloatingPoint, Greater, GreaterEqual, I32, I64, Integer, Less, LessEqual, ListType, MapType, Minus, ObjectType, Plus, SignedInteger, StringType, U32, U64, Unknown, UnsignedInteger, Void};
use crate::compiler::tokens::{Token, TokenType};
use log::debug;
use std::collections::HashMap;
use std::ops::Deref;
use crate::compiler::assembly_pass::Op::Assign;

pub enum Symbol {
    Function {
        name: String,
        parameters: Vec<Parameter>,
        return_type: TokenType,
        body: Vec<Statement>,
    },
    Variable {
        name: String,
        var_type: TokenType,
    },
    Object {
        name: String,
        fields: Vec<Parameter>,
    },
}

fn make_qname(path: &str, name: &Token) -> String {
    if path.is_empty() {
        name.lexeme.to_string()
    } else {
        format!("{}.{}", path, name.lexeme)
    }
}

pub fn build(path: &str, ast: &[Statement], symbols: &mut HashMap<String, Symbol>) {
    for statement in ast {
        match statement {
            Statement::FunctionStmt { function } => {
                symbols.insert(
                    make_qname(path, &function.name),
                    Symbol::Function {
                        name: function.name.lexeme.to_string(),
                        parameters: function.parameters.to_vec(),
                        return_type: function.return_type.clone(),
                        body: function.body.to_vec(),
                    },
                );
            }
            Statement::ObjectStmt { name, fields } => {
                symbols.insert(
                    make_qname(path, name),
                    Symbol::Object {
                        name: name.lexeme.to_string(),
                        fields: fields.to_vec(),
                    },
                );
            }
            Statement::ExpressionStmt { expression } => {
                match expression{
                    Expression::LetExpression { name, var_type, initializer } => {
                        let key = make_qname(path, name);
                        symbols.entry(key).or_insert_with(|| Symbol::Variable {
                            name: name.lexeme.to_string(),
                            var_type: var_type.clone(),
                        });
                    }
                    _ =>{}
                }
            }
            _ => {}
        }
    }
}

pub fn calculate_type(
    declared_type: &TokenType,
    inferred_type: &TokenType,
) -> Result<TokenType, CompilerError> {
    Ok(if declared_type != &Unknown {
        if declared_type != inferred_type {
            match (declared_type, inferred_type) {
                (I64, Unknown) => I64,
                (I32, I64) => I32, //need this?
                (I32, Integer) => I32,
                (U32, I64) => U32,
                (U32, Integer) => U32,
                (F32, F64) => F32,
                (F32, FloatingPoint) => F32,
                (F64, I64) => F64,
                (F64, FloatingPoint) => F64,
                (U64, I64) => U64,
                (U64, I32) => U64,
                (I64, Integer) => I64,
                (StringType, _) => StringType, // meh, this all needs rigorous testing. Update: this is in progress
                _ => {
                    return Err(IncompatibleTypes(
                        declared_type.clone(),
                        inferred_type.clone(),
                    ));
                }
            }
        } else {
            declared_type.clone()
        }
    } else {
        match inferred_type {
            Integer | I64 => I64,
            FloatingPoint => F64,
            Bool => Bool,
            DateTime => DateTime,
            ListType => ListType,
            MapType => MapType,
            ObjectType(p) => ObjectType(p.clone()),
            _ => return Err(CompilerError::UnexpectedType(inferred_type.clone())),
        }
    })
}

pub fn infer_type(expr: &Expression, symbols: &HashMap<String, Symbol>) -> Result<TokenType, CompilerError> {
    match expr {
        Expression::Binary {
            left,
            operator,
            right,
            ..
        } => {
            let left_type = infer_type(left, symbols)?;
            let right_type = infer_type(right, symbols)?;
            if [Greater, Less, GreaterEqual, LessEqual].contains(&operator.token_type) {
                Ok(Bool)
            } else if left_type == right_type {
                // map to determined numeric type if yet undetermined (32 or 64 bits)
                Ok(match left_type {
                    FloatingPoint => F64,
                    Integer => I64,
                    _ => left_type,
                })
            } else if let Plus = operator.token_type {
                // includes string concatenation with numbers
                // followed by type coercion to 64 bits for numeric types
                debug!("coerce {} : {}", left_type, right_type);
                match (left_type, right_type) {
                    (_, StringType) => Ok(StringType),
                    (StringType, _) => Ok(StringType),
                    (FloatingPoint, _) => Ok(F64),
                    (Integer, FloatingPoint) => Ok(F64),
                    (Integer, _) => Ok(I64),
                    (I64, Integer) => Ok(I64),
                    (F64, _) => Ok(F64),
                    (U64, U32) => Ok(U64),
                    (I64, I32) => Ok(I64),
                    // could add a date and a duration. future work
                    // could add a List and a value. also future work
                    // could add a Map and a tuple. Will I add tuple types? Future work!
                    _ => Err(CompilerError::Failure), //TODO better error message
                }
                // could have done some fall through here, but this will fail less gracefully,
                // so if my thinking is wrong or incomplete it will panic
            } else {
                // type coercion to 64 bits for numeric types
                debug!("coerce {} : {}", left_type, right_type);
                match (left_type, right_type) {
                    (FloatingPoint, _) => Ok(F64),
                    (Integer, FloatingPoint) => Ok(F64),
                    (Integer, I64) => Ok(I64),
                    (I64, FloatingPoint) => Ok(F64),
                    (F64, _) => Ok(F64),
                    (U64, U32) => Ok(U64),
                    (I64, I32) => Ok(I64),
                    (I64, Integer) => Ok(I64),
                    _ => Err(CompilerError::Failure), // TODO
                }
            }
        }
        Expression::Grouping { expression, .. } => infer_type(expression, symbols),
        Expression::Literal { literaltype, .. } => Ok(literaltype.clone()),
        Expression::List { literaltype, .. } => Ok(literaltype.clone()),
        Expression::Map { literaltype, .. } => Ok(literaltype.clone()),
        Expression::Unary {
            right, operator, ..
        } => {
            let literal_type = infer_type(right, symbols)?;
            if literal_type == Integer && operator.token_type == Minus {
                Ok(SignedInteger)
            } else {
                Ok(UnsignedInteger)
            }
        }
        Expression::Variable { var_type, .. } => Ok(var_type.clone()),
        Expression::Assignment { value, .. } => infer_type(value, symbols),
        Expression::FunctionCall { name, .. } => {
            let symbol = symbols.get(name);
            match symbol {
                Some(Symbol::Function { return_type, .. }) => Ok(return_type.clone()),
                Some(Symbol::Object { name, .. }) => Ok(ObjectType(name.clone())),
                _ => Err(CompilerError::Failure), // TODO
            }
        }
        Expression::MethodCall {
            receiver,
            method_name,
            ..
        } => {
            if let Expression::Literal { value, .. } = receiver.deref() {
                if let Ok(signature) = lookup(&value.to_string(), method_name) {
                    Ok(signature.return_type.clone())
                } else {
                    unreachable!() //?
                }
            } else {
                infer_type(receiver, symbols)
            }
        }
        Expression::Stop { .. } => Ok(Unknown),
        Expression::NamedParameter { .. } => Ok(Unknown),
        Expression::ListGet { .. } => Ok(Unknown),
        Expression::MapGet { .. } => Ok(Unknown),
        Expression::FieldGet { .. } => Ok(Unknown),
        Expression::Range { lower, .. } => infer_type(lower, symbols),
        Expression::IfExpression {  .. } => Ok(Unknown),
        Expression::IfElseExpression {  then_branch, else_branch, .. } => {
            let mut then_type = Void;
            for statement in then_branch {
                if let Statement::ExpressionStmt { expression } = statement {
                    then_type = infer_type(expression, symbols)?
                }
            }

            let mut else_type = Void;
            for statement in else_branch {
                if let Statement::ExpressionStmt { expression } = statement {
                    else_type = infer_type(expression, symbols)?
                }
            }
            if then_type != else_type{
                Err(CompilerError::IfElseBranchesDoNotMatch(then_type, else_type))
            } else {
                Ok(then_type)
            }
        },
        Expression::LetExpression { .. } => Ok(Void),
        Expression::ForStatement { .. } => Ok(Void),
    }
}
