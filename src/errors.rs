use crate::compiler::tokens::TokenType;
use std::fmt::Display;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum TipiLangError {
    #[error("Compilation failed: {0}")]
    Compiler(#[from] CompilerErrorAtLine),

    #[error(transparent)]
    Runtime(#[from] RuntimeError),
    #[error("Platform error {0}")]
    Platform(String),
}

#[derive(Error, Debug, PartialEq)]
pub struct CompilerErrorAtLine {
    pub error: CompilerError,
    pub line: usize,
}

impl CompilerErrorAtLine {
    pub(crate) fn raise(error: CompilerError, line: usize) -> Self {
        Self { error, line }
    }
}

impl Display for CompilerErrorAtLine {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "error at line {}, {}", self.line, self.error)
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum CompilerError {
    #[error("Compilation failed")]
    Failure,
    #[error("Too many parameters")]
    TooManyParameters,
    #[error("Expected {0}")]
    Expected(&'static str),
    #[error("unexpected indent level {0} vs expected {1}")]
    UnexpectedIndent(usize, usize),
    #[error("Type mismatch: {0}")]
    TypeError(Box<CompilerError>),
    #[error("Uninitialized variables are not allowed.")]
    UninitializedVariable,
    #[error("Expected {0}, found {1}")]
    IncompatibleTypes(TokenType, TokenType),
    #[error("Error parsing number {0}")]
    ParseError(String),
    #[error("Undeclared variable: '{0}'")]
    UndeclaredVariable(String),
    #[error("Unexpected identifier")]
    UnexpectedIdentifier,
    #[error("Unterminated {0}")]
    Unterminated(&'static str),
    #[error("Illegal char length for {0}")]
    IllegalCharLength(String),
    #[error("Unexpected type {0}")]
    UnexpectedType(TokenType),
    #[error("'{0}' is a keyword. You cannot use it as an identifier")]
    KeywordNotAllowedAsIdentifier(TokenType),
    #[error("Tipi does not support numbers above 2^64")]
    Overflow,
    #[error("Undeclared function: '{0}'")]
    FunctionNotFound(String),
    #[error("Illegal argument: '{0}' cannot be used as an index into a list")]
    IllegalIndexArgument(TokenType),
    #[error("Illegal argument: '{0}' cannot be indexed")]
    IllegalTypeToIndex(String),
    #[error("The number of of arguments for {0} is not correct. Should be {1}, got {2}")]
    IllegalArgumentsException(String, usize, usize),
    #[error("Function name {0} is a global function and cannot be used here.")]
    ReservedFunctionName(String),
    #[error("The if expression branches do not match. Was {0} and {1}")]
    IfElseBranchesDoNotMatch(TokenType, TokenType),

}

#[derive(Error, Debug, PartialEq)]
pub enum RuntimeError {
    #[error("Error while executing")]
    ValueError(#[from] ValueError),
    #[error("Error occurred")]
    Something,
    #[error("Expected {0}, got {1}")]
    Expected(&'static str, &'static str),
    #[error("Function {0} not found")]
    FunctionNotFound(String),
    #[error("The number of of arguments for {0} is not correct. Should be {1}, got {2}")]
    IllegalArgumentsException(String, usize, usize),
    #[error("{0}")]
    IllegalArgumentException(String),
    #[error("Expected {0}")]
    ExpectedType(String),
    #[error("Index out of bounds: {0} > {1}")]
    IndexOutOfBounds(usize, usize),
}

#[derive(Error, Debug, PartialEq)]
pub enum ValueError {
    #[error("{0}")]
    CannotAnd(&'static str),
    #[error("{0}")]
    Some(&'static str),
    #[error("Illegal cast")]
    IllegalCast,
}
