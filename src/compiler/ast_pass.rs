use crate::builtins::globals::GLOBAL_FUNCTIONS;
use crate::compiler::ast_pass::Expression::{
    Assignment, FieldGet, FunctionCall, IfExpression, ListGet, MapGet, MethodCall, NamedParameter,
    Stop, Variable,
};
use crate::compiler::tokens::TokenType::{
    Bang, Bool, Char, Colon, DateTime, Dot, Else, Eof, Eol, Equal, False, FloatingPoint, Fn, For,
    Greater, GreaterEqual, GreaterGreater, Identifier, If, In, Indent, Integer, LeftBrace,
    LeftBracket, LeftParen, Less, LessEqual, LessLess, Let, ListType, MapType, Minus, Object, Plus,
    Print, Range, RightBrace, RightBracket, RightParen, SingleRightArrow, Slash, Star, StringType,
    True, U32, U64, Unknown,
};
use crate::compiler::tokens::{Token, TokenType};
use crate::errors::CompilerError::{
    self, Expected, ParseError, TooManyParameters, UnexpectedIndent, UninitializedVariable,
};
use crate::errors::CompilerErrorAtLine;
use crate::symbol_builder::{Symbol, calculate_type, infer_type};
use crate::value::Value;
use crate::{DATE_FORMAT_TIMEZONE, Expr, Stmt, SymbolTable};
use log::debug;
use std::collections::HashMap;

pub fn compile(
    path: Option<&str>,
    tokens: Vec<Token>,
    symbol_table: &mut SymbolTable,
) -> Result<Vec<Statement>, CompilerErrorAtLine> {
    let mut compiler = AstCompiler::new(path.unwrap_or(""), tokens);
    compiler.compile_tokens(symbol_table)
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Token,
    pub parameters: Vec<Parameter>,
    pub return_type: TokenType,
    pub body: Vec<Statement>,
}

struct AstCompiler {
    tokens: Vec<Token>,
    current: usize,
    had_error: bool,
    indent: Vec<usize>,
}

impl AstCompiler {
    fn new(_name: &str, tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            had_error: false,
            indent: vec![0],
        }
    }

    fn reset(&mut self) {
        self.current = 0;
    }

    fn compile_tokens(
        &mut self,
        symbol_table: &mut HashMap<String, Symbol>,
    ) -> Result<Vec<Statement>, CompilerErrorAtLine> {
        self.reset();
        self.compile(symbol_table)
    }

    fn compile(
        &mut self,
        symbol_table: &mut SymbolTable,
    ) -> Result<Vec<Statement>, CompilerErrorAtLine> {
        if !self.had_error {
            let mut statements = vec![];
            while !self.is_at_end() {
                if self.match_token(&[Eol]) {
                    continue;
                }
                let statement = self.indent(symbol_table)?;
                if let Some(statement) = statement {
                    statements.push(statement);
                } else {
                    break;
                }
            }
            debug!("AST {:?}", statements);
            Ok(statements)
        } else {
            Err(self.raise(CompilerError::Failure))
        }
    }

    fn raise(&self, error: CompilerError) -> CompilerErrorAtLine {
        CompilerErrorAtLine::raise(error, self.current_line())
    }

    fn indent(
        &mut self,
        symbol_table: &mut SymbolTable,
    ) -> Result<Option<Statement>, CompilerErrorAtLine> {
        let expected_indent = *self.indent.last().unwrap();
        // skip empty lines
        while self.check(&Eol) {
            self.advance();
        }

        let mut indent_on_line = 0;
        // keep track of indent level
        while self.match_token(&[Indent]) {
            indent_on_line += 1;
        }
        if indent_on_line > expected_indent {
            Err(self.raise(UnexpectedIndent(indent_on_line, expected_indent)))
        } else if indent_on_line < expected_indent {
            self.indent.pop();
            Ok(None)
        } else {
            Ok(Some(self.declaration(symbol_table)?))
        }
    }

    fn declaration(&mut self, symbol_table: &mut SymbolTable) -> Stmt {
        if self.match_token(&[Fn]) {
            self.function_declaration(symbol_table)
        } else if self.match_token(&[Let]) {
            self.let_declaration(symbol_table)
        } else if self.match_token(&[Object]) {
            self.object_declaration(symbol_table)
        } else if self.match_token(&[TokenType::Pipe]) {
            self.guard_declaration(symbol_table)
        } else {
            self.statement(symbol_table)
        }
    }

    //  | /. -> service.get_all()
    //  | /{uuid} -> service.get(uuid)?
    //  | ?{query.firstname} -> service.get_by_firstname(fname)?
    fn guard_declaration(&mut self, symbol_table: &mut SymbolTable) -> Stmt {
        let if_expr = self.guard_if_expr(symbol_table)?;
        let then_expr = self.expression(symbol_table)?;
        Ok(Statement::GuardStatement { if_expr, then_expr })
    }

    fn guard_if_expr(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        if !self.check(&SingleRightArrow) {
            return if self.match_token(&[Slash]) {
                self.path_guard_expr()
            } else if self.match_token(&[TokenType::Question]) {
                self.query_guard_expr(symbol_table)
            } else {
                Err(self.raise(Expected("-> or ?")))
            };
        }
        Ok(Stop {
            line: self.peek().line,
        })
    }

    fn query_guard_expr(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        if self.match_token(&[LeftBrace]) {
            let query_params = self.expression(symbol_table)?;
            self.consume(&RightBrace, Expected("'}' after guard expression."))?;
            Ok(query_params)
        } else {
            Ok(Stop {
                line: self.peek().line,
            })
        }
    }

    fn path_guard_expr(&mut self) -> Expr {
        if self.match_token(&[LeftBrace]) {
            let path_params = self.match_expression()?;
            self.consume(&RightBrace, Expected("'}' after guard expression."))?;
            Ok(path_params)
        } else {
            Ok(Stop {
                line: self.peek().line,
            })
        }
    }

    fn match_expression(&mut self) -> Expr {
        Err(self.raise(Expected("unimplemented")))
    }

    fn object_declaration(&mut self, symbol_table: &mut SymbolTable) -> Stmt {
        let type_name = self.consume(&Identifier, Expected("object name."))?;
        self.consume(&Colon, Expected("':' after object name."))?;
        self.consume(&Eol, Expected("end of line."))?;

        let mut fields = vec![];

        let expected_indent = self.indent.last().unwrap() + 1;
        let mut done = false;
        while !done && !self.match_token(&[Eof]) {
            for _ in 0..expected_indent {
                if self.peek().token_type == Indent {
                    self.advance();
                } else {
                    done = true;
                }
            }
            if !done {
                let field_name = self.consume(&Identifier, Expected("an object field name."))?;
                self.consume(&Colon, Expected("':' after field name."))?;
                let field_type = self.peek().token_type.clone();
                if field_type.is_type() {
                    self.advance();
                } else {
                    Err(self.raise(Expected("a type")))?
                }
                fields.push(Parameter {
                    name: field_name,
                    var_type: field_type,
                });
            }
        }
        self.consume(&Eol, Expected("end of line."))?;

        let type_name_as_str = type_name.lexeme.clone();
        symbol_table.insert(
            type_name_as_str.clone(),
            Symbol::Object {
                name: type_name_as_str.clone(),
                fields: fields.clone(),
            },
        ); // name name name

        Ok(Statement::ObjectStmt {
            name: type_name,
            fields,
        })
    }

    fn function_declaration(&mut self, symbol_table: &mut SymbolTable) -> Stmt {
        let name_token = self.consume(&Identifier, Expected("function name."))?;
        if GLOBAL_FUNCTIONS.contains_key(name_token.lexeme.as_str()) {
            return Err(self.raise(CompilerError::ReservedFunctionName(
                name_token.lexeme.clone(),
            )));
        }
        self.consume(&LeftParen, Expected("'(' after function name."))?;
        let mut parameters = vec![];
        while !self.check(&RightParen) {
            if parameters.len() >= 25 {
                return Err(self.raise(TooManyParameters));
            }
            let parm_name = self.consume(&Identifier, Expected("a parameter name."))?;

            self.consume(&Colon, Expected(": after parameter name"))?;
            let var_type = self.peek().token_type.clone();

            self.advance();
            parameters.push(Parameter {
                name: parm_name,
                var_type,
            });
            if self.peek().token_type == TokenType::Comma {
                self.advance();
            }
        }
        self.consume(&RightParen, Expected(" ')' after parameters."))?;
        let return_type = if self.check(&SingleRightArrow) {
            self.consume(&SingleRightArrow, Expected("->"))?;
            self.advance().token_type.clone()
        } else {
            TokenType::Void
        };
        self.consume(&Colon, Expected("colon (:) after function declaration."))?;
        self.consume(&Eol, Expected("end of line."))?;

        self.inc_indent();

        let body = self.compile(symbol_table)?;

        let function = Function {
            name: name_token.clone(),
            parameters,
            return_type,
            body,
        };

        Ok(Statement::FunctionStmt { function })
    }

    fn let_declaration(&mut self, symbol_table: &mut SymbolTable) -> Stmt {
        if self.peek().token_type.is_type() {
            return Err(self.raise(CompilerError::KeywordNotAllowedAsIdentifier(
                self.peek().token_type.clone(),
            )));
        }
        let name_token = self.consume(&Identifier, Expected("variable name."))?;

        let declared_type = if self.check(&Colon) {
            self.advance();
            Some(self.advance().token_type.clone())
        } else {
            None
        };

        if self.match_token(&[Equal]) {
            let initializer = self.expression(symbol_table)?;
            let declared_type = declared_type.unwrap_or(Unknown);
            let inferred_type = infer_type(&initializer, symbol_table);
            let var_type =
                calculate_type(&declared_type, &inferred_type).map_err(|e| self.raise(e))?;
            symbol_table.insert(
                name_token.lexeme.clone(),
                Symbol::Variable {
                    name: name_token.lexeme.clone(),
                    var_type: var_type.clone(),
                },
            );

            self.consume(&Eol, Expected("end of line after initializer."))?;

            Ok(Statement::VarStmt {
                name: name_token,
                var_type,
                initializer,
            })
        } else {
            Err(self.raise(UninitializedVariable))?
        }
    }

    fn statement(&mut self, symbol_table: &mut SymbolTable) -> Stmt {
        if self.match_token(&[For]) {
            self.for_statement(symbol_table)
        } else {
            self.expr_statement(symbol_table)
        }
    }

    fn for_statement(&mut self, symbol_table: &mut SymbolTable) -> Stmt {
        let loop_var = self.consume(&Identifier, Expected("loop variable name."))?;
        self.consume(&In, Expected("'in' after loop variable name."))?;
        let range = self.expression(symbol_table)?;
        self.consume(&Colon, Expected("colon after range expression"))?;
        self.consume(&Eol, Expected("end of line after for expression."))?;
        self.inc_indent();
        let body = self.compile(symbol_table)?;

        Ok(Statement::ForStatement {
            loop_var,
            range,
            body,
        })
    }

    fn inc_indent(&mut self) {
        self.indent.push(self.indent.last().unwrap() + 1);
    }

    fn expr_statement(&mut self, symbol_table: &mut SymbolTable) -> Stmt {
        let expr = self.expression(symbol_table)?;
        if !self.is_at_end() {
            self.consume(&Eol, Expected("end of line after expression."))?;
        }
        Ok(Statement::ExpressionStmt { expression: expr })
    }

    fn expression(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        self.or(symbol_table)
    }

    fn or(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.and(symbol_table)?;
        self.binary(&[TokenType::LogicalOr], expr, symbol_table)
    }

    fn and(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.bit_and(symbol_table)?;
        self.binary(&[TokenType::LogicalAnd], expr, symbol_table)
    }

    fn bit_and(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.bit_or(symbol_table)?;
        self.binary(&[TokenType::BitAnd], expr, symbol_table)
    }

    fn bit_or(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.bit_xor(symbol_table)?;
        self.binary(&[TokenType::Pipe], expr, symbol_table)
    }

    fn bit_xor(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.assignment(symbol_table)?;
        self.binary(&[TokenType::BitXor], expr, symbol_table)
    }

    fn assignment(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.equality(symbol_table)?;
        if self.match_token(&[Equal]) {
            let operator = self.previous().clone();
            let right = self.comparison(symbol_table)?;
            if let Variable { name, .. } = expr {
                Ok(Assignment {
                    line: operator.line,
                    variable_name: name.to_string(),
                    value: Box::new(right),
                })
            } else {
                Err(self.raise(CompilerError::Failure))
            }
        } else {
            Ok(expr)
        }
    }

    fn equality(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.comparison(symbol_table)?;
        self.binary(
            &[TokenType::EqualEqual, TokenType::BangEqual],
            expr,
            symbol_table,
        )
    }

    fn comparison(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.range(symbol_table)?;
        self.binary(
            &[Greater, GreaterEqual, Less, LessEqual],
            expr,
            symbol_table,
        )
    }

    fn range(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let mut expr = self.bitshift(symbol_table)?;
        if self.match_token(&[Range]) {
            let operator = self.previous().clone();
            let right = self.expression(symbol_table)?;
            expr = Expression::Range {
                line: operator.line,
                lower: Box::new(expr),
                upper: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn bitshift(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.term(symbol_table)?;
        self.binary(&[GreaterGreater, LessLess], expr, symbol_table)
    }

    fn term(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.factor(symbol_table)?;
        self.binary(&[Minus, Plus], expr, symbol_table)
    }

    fn factor(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.unary(symbol_table)?;
        self.binary(&[Slash, Star], expr, symbol_table)
    }

    fn binary(
        &mut self,
        types: &[TokenType],
        mut expr: Expression,
        symbol_table: &mut SymbolTable,
    ) -> Expr {
        while self.match_token(types) {
            let operator = self.previous().clone();
            let right = self.comparison(symbol_table)?;
            expr = Expression::Binary {
                line: operator.line,
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn unary(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        if self.match_token(&[Bang, Minus]) {
            let operator = self.previous().clone();
            let right = self.unary(symbol_table)?;
            Ok(Expression::Unary {
                line: self.peek().line,
                operator,
                right: Box::new(right),
            })
        } else {
            self.equals(symbol_table)
        }
    }

    fn equals(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        if self.match_token(&[If]) {
            let condition = self.expression(symbol_table)?;
            self.consume(&Colon, Expected("':' after if condition."))?;

            self.inc_indent();
            let then_branch = self.compile(symbol_table)?;

            let else_branch = if self.check(&Else) {
                self.consume(&Else, Expected("'else' after if condition."))?;
                self.consume(&Colon, Expected("':' after 'else'."))?;

                self.inc_indent();
                Some(self.compile(symbol_table)?)
            } else {
                None
            };
            Ok(IfExpression {
                condition: Box::new(condition),
                then_branch,
                else_branch,
            })
        } else {
            self.get(symbol_table)
        }
    }

    fn get(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let expr = self.primary(symbol_table)?;

        if self.match_token(&[LeftParen]) {
            let name = self.peek().clone();
            self.advance();
            self.function_call(name, symbol_table)
        } else if self.match_token(&[LeftBracket]) {
            let index = self.expression(symbol_table)?;
            self.index(expr, index)
        } else if self.match_token(&[Dot]) {
            let name = self.peek().clone();
            self.advance();
            self.field_or_method(expr, name, symbol_table)
        } else {
            Ok(expr)
        }
    }

    fn index(&mut self, operand: Expression, index: Expression) -> Expr {
        let get = match &operand {
            Expression::Map { .. } => MapGet {
                map: Box::new(operand),
                key: Box::new(index),
            },
            Expression::List { .. } => ListGet {
                list: Box::new(operand),
                index: Box::new(index),
            },
            Variable { var_type, .. } => match var_type {
                ListType => ListGet {
                    list: Box::new(operand),
                    index: Box::new(index),
                },
                MapType => MapGet {
                    map: Box::new(operand),
                    key: Box::new(index),
                },
                _ => {
                    return Err(self.raise(CompilerError::IllegalTypeToIndex(var_type.to_string())));
                }
            },
            _ => return Err(self.raise(CompilerError::IllegalTypeToIndex("Unknown".to_string()))),
        };
        self.consume(&RightBracket, Expected("']' after index."))?;
        Ok(get)
    }

    // work in progress
    fn field_or_method(
        &mut self,
        receiver: Expression,
        op: Token,
        symbol_table: &mut SymbolTable,
    ) -> Expr {
        if self.match_token(&[LeftParen]) {
            let arguments = self.arguments(symbol_table)?;
            Ok(MethodCall {
                receiver: Box::new(receiver.clone()),
                method_name: op.lexeme,
                arguments,
                line: op.line,
            })
        } else {
            // no test yet
            Ok(FieldGet {
                receiver: Box::new(receiver.clone()),
                field: op.lexeme.clone(),
            })
        }
    }

    fn primary(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        Ok(if self.match_token(&[LeftBracket]) {
            self.list(symbol_table)?
        } else if self.match_token(&[LeftBrace]) {
            self.map(symbol_table)?
        } else if self.match_token(&[False]) {
            Expression::Literal {
                line: self.peek().line,
                literaltype: Bool,
                value: Value::Bool(false),
            }
        } else if self.match_token(&[True]) {
            Expression::Literal {
                line: self.peek().line,
                literaltype: Bool,
                value: Value::Bool(true),
            } //, FloatingPoint, Text
        } else if self.match_token(&[Integer]) {
            Expression::Literal {
                line: self.peek().line,
                literaltype: Integer,
                value: Value::I64(
                    self.previous()
                        .lexeme
                        .parse()
                        .map_err(|e| self.raise(ParseError(format!("{:?}", e))))?,
                ),
            }
        } else if self.match_token(&[U32]) {
            Expression::Literal {
                line: self.peek().line,
                literaltype: Integer,
                value: Value::U32(
                    u32::from_str_radix(self.previous().lexeme.trim_start_matches("0x"), 16)
                        .map_err(|e| self.raise(ParseError(format!("{:?}", e))))?,
                ),
            }
        } else if self.match_token(&[U64]) {
            Expression::Literal {
                line: self.peek().line,
                literaltype: Integer,
                value: Value::U64(
                    u64::from_str_radix(self.previous().lexeme.trim_start_matches("0x"), 16)
                        .map_err(|e| self.raise(ParseError(format!("{:?}", e))))?,
                ),
            }
        } else if self.match_token(&[FloatingPoint]) {
            Expression::Literal {
                line: self.peek().line,
                literaltype: FloatingPoint,
                value: Value::F64(
                    self.previous()
                        .lexeme
                        .parse()
                        .map_err(|e| self.raise(ParseError(format!("{:?}", e))))?,
                ),
            }
        } else if self.match_token(&[StringType]) {
            Expression::Literal {
                line: self.peek().line,
                literaltype: StringType,
                value: Value::String(self.previous().lexeme.clone()),
            }
        } else if self.match_token(&[Char]) {
            Expression::Literal {
                line: self.peek().line,
                literaltype: Char,
                value: Value::Char(self.previous().lexeme.chars().next().unwrap()),
            }
        } else if self.match_token(&[DateTime]) {
            Expression::Literal {
                line: self.peek().line,
                literaltype: DateTime,
                value: Value::DateTime(Box::new(
                    chrono::DateTime::parse_from_str(&self.previous().lexeme, DATE_FORMAT_TIMEZONE)
                        .map_err(|_| self.raise(ParseError(self.previous().lexeme.clone())))?
                        .into(),
                )),
            }
        } else if self.match_token(&[LeftParen]) {
            let expr = self.expression(symbol_table)?;
            self.consume(&RightParen, Expected("')' after expression."))?;
            Expression::Grouping {
                line: self.peek().line,
                expression: Box::new(expr),
            }
        } else {
            let token = self.advance().clone();
            debug!("{:?}", token);
            if self.match_token(&[LeftParen]) {
                self.function_call(token.clone(), symbol_table)?
            } else if self.match_token(&[Colon]) {
                self.named_parameter(&token, symbol_table)?
            } else {
                self.variable_lookup(&token, symbol_table)?
            }
        })
    }

    fn named_parameter(&mut self, name: &Token, symbol_table: &mut SymbolTable) -> Expr {
        let value = self.expression(symbol_table)?;
        let line = name.line;
        Ok(NamedParameter {
            name: name.clone(),
            value: Box::new(value),
            line,
        })
    }

    fn list(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let mut list = vec![];
        while !self.match_token(&[RightBracket]) {
            list.push(self.expression(symbol_table)?);
            if self.peek().token_type == TokenType::Comma {
                self.advance();
            } else {
                self.consume(&RightBracket, Expected("']' at the end of the list."))?;
                break;
            }
        }
        Ok(Expression::List {
            values: list,
            literaltype: ListType,
            line: self.peek().line,
        })
    }

    fn map(&mut self, symbol_table: &mut SymbolTable) -> Expr {
        let mut entries = vec![];
        while !self.match_token(&[RightBrace]) {
            let key = self.expression(symbol_table)?;
            self.consume(&Colon, Expected("':' after map key."))?;
            let value = self.expression(symbol_table)?;
            entries.push((key, value));
            if self.peek().token_type == TokenType::Comma {
                self.advance();
            } else {
                self.consume(&RightBrace, Expected("'}' after map."))?;
                break;
            }
        }
        Ok(Expression::Map {
            entries,
            literaltype: MapType,
            line: self.peek().line,
        })
    }

    fn variable_lookup(&mut self, name: &Token, symbol_table: &mut SymbolTable) -> Expr {
        let var = symbol_table.get(&name.lexeme);
        let var_type = if let Some(Symbol::Variable { var_type, .. }) = var {
            var_type
        } else {
            &Unknown
        };
        Ok(Variable {
            name: name.lexeme.to_string(),
            var_type: var_type.clone(),
            line: name.line,
        })
    }

    fn function_call(&mut self, name: Token, symbol_table: &mut SymbolTable) -> Expr {
        let arguments = self.arguments(symbol_table)?;
        Ok(FunctionCall {
            line: self.peek().line,
            name: name.lexeme.to_string(),
            arguments,
        })
    }

    fn arguments(
        &mut self,
        symbol_table: &mut SymbolTable,
    ) -> Result<Vec<Expression>, CompilerErrorAtLine> {
        let mut arguments = vec![];
        while !self.match_token(&[RightParen]) {
            if arguments.len() >= 25 {
                return Err(self.raise(TooManyParameters));
            }
            let arg = self.expression(symbol_table)?;
            arguments.push(arg);
            if self.peek().token_type == TokenType::Comma {
                self.advance();
            } else {
                self.consume(&RightParen, Expected("')' after arguments."))?;
                break;
            }
        }
        Ok(arguments)
    }

    fn consume(
        &mut self,
        token_type: &TokenType,
        message: CompilerError,
    ) -> Result<Token, CompilerErrorAtLine> {
        if self.check(token_type) {
            self.advance();
        } else {
            self.had_error = true;
            return Err(self.raise(message));
        }
        Ok(self.previous().clone())
    }

    fn match_token(&mut self, tokens: &[TokenType]) -> bool {
        for tt in tokens {
            if self.check(tt) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            &self.peek().token_type == token_type
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == Eof
    }

    fn current_line(&self) -> usize {
        self.peek().line
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExpressionStmt {
        expression: Expression,
    },
    VarStmt {
        name: Token,
        var_type: TokenType,
        initializer: Expression,
    },
    FunctionStmt {
        function: Function,
    },
    ObjectStmt {
        name: Token,
        fields: Vec<Parameter>,
    },
    GuardStatement {
        if_expr: Expression,
        then_expr: Expression,
    },
    ForStatement {
        loop_var: Token,
        range: Expression,
        body: Vec<Statement>,
    },
}

impl Statement {
    pub fn line(&self) -> usize {
        match self {
            Statement::ExpressionStmt { expression } => expression.line(),
            Statement::VarStmt { name, .. } => name.line,
            Statement::FunctionStmt { function, .. } => function.name.line,
            Statement::ObjectStmt { name, .. } => name.line,
            Statement::GuardStatement { if_expr, .. } => if_expr.line(),
            Statement::ForStatement { loop_var, .. } => loop_var.line,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Token,
    pub var_type: TokenType,
}

impl Parameter {
    pub fn new(name: impl Into<String>, value_type: TokenType) -> Self {
        Self {
            name: Token {
                token_type: TokenType::StringType,
                lexeme: name.into(),
                line: 0,
            },
            var_type: value_type,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Binary {
        line: usize,
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Unary {
        line: usize,
        operator: Token,
        right: Box<Expression>,
    },
    Grouping {
        line: usize,
        expression: Box<Expression>,
    },
    Literal {
        line: usize,
        literaltype: TokenType,
        value: Value,
    },
    Range {
        line: usize,
        lower: Box<Expression>,
        upper: Box<Expression>,
    },
    List {
        line: usize,
        literaltype: TokenType,
        values: Vec<Expression>,
    },
    Map {
        line: usize,
        literaltype: TokenType,
        entries: Vec<(Expression, Expression)>,
    },
    Variable {
        line: usize,
        name: String,
        var_type: TokenType,
    },
    Assignment {
        line: usize,
        variable_name: String,
        value: Box<Expression>,
    },
    FunctionCall {
        line: usize,
        name: String,
        arguments: Vec<Expression>,
    },
    MethodCall {
        line: usize,
        receiver: Box<Expression>,
        method_name: String,
        arguments: Vec<Expression>,
    },
    Stop {
        line: usize,
    },
    NamedParameter {
        line: usize,
        name: Token,
        value: Box<Expression>,
    },
    MapGet {
        map: Box<Expression>,
        key: Box<Expression>,
    },
    ListGet {
        list: Box<Expression>,
        index: Box<Expression>,
    },
    FieldGet {
        receiver: Box<Expression>,
        field: String,
    },
    IfExpression {
        condition: Box<Expression>,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },
}

impl Expression {
    pub fn line(&self) -> usize {
        match self {
            Self::Binary { line, .. } => *line,
            Self::Unary { line, .. } => *line,
            Self::Grouping { line, .. } => *line,
            Self::Literal { line, .. } => *line,
            Self::Range { line, .. } => *line,
            Self::List { line, .. } => *line,
            Self::Map { line, .. } => *line,
            Variable { line, .. } => *line,
            Assignment { line, .. } => *line,
            FunctionCall { line, .. } => *line,
            MethodCall { line, .. } => *line,
            Stop { line } => *line,
            NamedParameter { line, .. } => *line,
            MapGet { .. } => 0,
            ListGet { .. } => 0,
            FieldGet { .. } => 0,
            IfExpression { condition, .. } => condition.line(),
        }
    }
}
