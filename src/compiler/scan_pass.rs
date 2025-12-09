use crate::compiler::tokens::TokenType::{BitXor, FloatingPoint, In, Integer, Question, U32, U64};
use crate::compiler::tokens::{
    Token,
    TokenType::{self},
};
use crate::errors::CompilerError::{IllegalCharLength, UnexpectedIdentifier, Unterminated};
use crate::errors::{CompilerError, CompilerErrorAtLine};
use crate::keywords;
use axum::http::header::ToStrError;

pub fn scan(source: &str) -> Result<Vec<Token>, CompilerErrorAtLine> {
    let scanner = Scanner {
        chars: source.chars().collect(),
        current: 0,
        start: 0,
        line: 1,
        tokens: vec![],
        new_line: true,
    };
    scanner.scan()
}

impl Scanner {
    fn scan(mut self) -> Result<Vec<Token>, CompilerErrorAtLine> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        self.add_token(TokenType::Eol);
        self.add_token(TokenType::Eof);
        Ok(self.tokens)
    }

    fn scan_token(&mut self) -> Result<(), CompilerErrorAtLine> {
        let c = self.advance();
        if self.new_line && (c == ' ' || c == '\t') {
            self.add_token(TokenType::Indent);
            self.new_line = false;
        } else {
            if c != '\n' {
                self.new_line = false;
            }
            match c {
                '(' => self.add_token(TokenType::LeftParen),
                ')' => self.add_token(TokenType::RightParen),
                '{' => self.add_token(TokenType::LeftBrace),
                '}' => self.add_token(TokenType::RightBrace),
                '[' => self.add_token(TokenType::LeftBracket),
                ']' => self.add_token(TokenType::RightBracket),
                ',' => self.add_token(TokenType::Comma),
                '.' => {
                    let t = if self.match_next('.') {
                        TokenType::Range
                    } else {
                        TokenType::Dot
                    };
                    self.add_token(t);
                }
                '-' => {
                    let t = if self.match_next('>') {
                        TokenType::SingleRightArrow
                    } else {
                        TokenType::Minus
                    };
                    self.add_token(t);
                }
                '#' => self.add_token(TokenType::Hash),
                '+' => self.add_token(TokenType::Plus),
                ':' => self.add_token(TokenType::Colon),
                ';' => println!("Warning: Ignoring semicolon at line {}", self.line),
                '*' => self.add_token(TokenType::Star),
                '!' => {
                    let t = if self.match_next('=') {
                        TokenType::BangEqual
                    } else {
                        TokenType::Bang
                    };
                    self.add_token(t);
                }
                '=' => {
                    let t = if self.match_next('=') {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    };
                    self.add_token(t);
                }
                '<' => {
                    let t = if self.match_next('=') {
                        TokenType::LessEqual
                    } else if self.match_next('<') {
                        TokenType::LessLess
                    } else {
                        TokenType::Less
                    };
                    self.add_token(t)
                }
                '>' => {
                    let t = if self.match_next('=') {
                        TokenType::GreaterEqual
                    } else if self.match_next('>') {
                        TokenType::GreaterGreater
                    } else {
                        TokenType::Greater
                    };
                    self.add_token(t);
                }
                '/' => {
                    if self.match_next('/') {
                        // todo make distinction between comment and doc
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        self.add_token(TokenType::Slash);
                    }
                }
                '\'' => self.char()?,
                '"' => self.string()?,
                'd' if self.match_next('"') => {
                    self.datetime()?;
                }
                '\r' | '\t' | ' ' => {}
                '\n' => {
                    self.line += 1;
                    self.new_line = true;
                    self.add_token(TokenType::Eol);
                }
                '&' => {
                    let t = if self.match_next('&') {
                        TokenType::LogicalAnd
                    } else {
                        TokenType::BitAnd
                    };
                    self.add_token(t);
                }
                '|' => {
                    let t = if self.match_next('|') {
                        TokenType::LogicalOr
                    } else {
                        TokenType::Pipe
                    };
                    self.add_token(t);
                }
                '^' => self.add_token(BitXor),
                '?' => self.add_token(Question),
                _ => {
                    if c == '0' && self.peek() == 'x' {
                        self.hex_number()?;
                    } else if c.is_ascii_digit() {
                        self.number_or_range();
                    } else if is_alpha(c) {
                        self.identifier();
                    } else {
                        return Err(self.raise(UnexpectedIdentifier));
                    }
                }
            }
        }
        Ok(())
    }

    fn identifier(&mut self) {
        while is_alphanumeric(self.peek()) {
            self.advance();
        }
        let value: String = self.chars[self.start..self.current].iter().collect();
        let tokentype = keywords::get_keyword(&value).unwrap_or(TokenType::Identifier);

        self.add_token_with_value(tokentype, value);
    }

    fn hex_number(&mut self) -> Result<(), CompilerErrorAtLine> {
        self.advance();
        self.advance();
        while self.peek().is_ascii_hexdigit() {
            self.advance();
        }
        let value: String = self.chars[self.start..self.current].iter().collect();
        if value.len() < 5 {
            self.add_token_with_value(U32, value);
        } else if value.len() < 9 {
            self.add_token_with_value(U64, value);
        } else {
            return Err(self.raise(CompilerError::Overflow));
        }
        Ok(())
    }

    fn number_or_range(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        let mut has_dot = false;
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            has_dot = true;
            self.advance();
        }
        if self.peek() == '.' && self.peek_next() == '.' {
            self.range_expression()
        } else {
            while is_digit_or_scientific(self.peek()) {
                self.advance();
            }
            let value: String = self.chars[self.start..self.current].iter().collect();
            self.add_token_with_value(if has_dot { FloatingPoint } else { Integer }, value);
        }
    }

    fn range_expression(&mut self) {
        let lower: String = self.chars[self.start..self.current].iter().collect();
        self.match_next('.');
        self.match_next('.');
        let tokentype = if lower.parse::<i64>().is_err() {
            TokenType::Identifier
        } else {
            Integer
        };
        self.add_token_with_value(tokentype, lower);
        self.add_token(TokenType::Range);
        self.start = self.current;
        let tokentype = if self.peek().is_ascii_digit() {
            Integer
        } else {
            TokenType::Identifier
        };
        if self.peek().is_ascii_digit() {
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        } else {
            self.advance();
            while self.peek().is_alphanumeric() {
                self.advance();
            }
        }
        let upper: String = self.chars[self.start..self.current].iter().collect();
        self.add_token_with_value(tokentype, upper);
    }

    fn char(&mut self) -> Result<(), CompilerErrorAtLine> {
        while self.peek() != '\'' && !self.is_at_end() {
            self.advance();
        }

        if self.is_at_end() {
            return Err(CompilerErrorAtLine::raise(Unterminated("char"), self.line));
        }

        self.advance();

        let value: String = self.chars[self.start + 1..self.current - 1]
            .iter()
            .collect();
        if value.len() != 1 {
            return Err(self.raise(IllegalCharLength(value)));
        }
        self.add_token_with_value(TokenType::Char, value);
        Ok(())
    }

    fn raise(&self, error: CompilerError) -> CompilerErrorAtLine {
        CompilerErrorAtLine::raise(error, self.line)
    }

    fn datetime(&mut self) -> Result<(), CompilerErrorAtLine> {
        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
        }
        self.advance();
        let value: String = self.chars[self.start + 2..self.current - 1]
            .iter()
            .collect();
        self.add_token_with_value(TokenType::DateTime, value);
        Ok(())
    }

    fn string(&mut self) -> Result<(), CompilerErrorAtLine> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(self.raise(Unterminated("string")));
        }

        self.advance();

        let value: String = self.chars[self.start + 1..self.current - 1]
            .iter()
            .collect();
        self.add_token_with_value(TokenType::StringType, value);
        Ok(())
    }

    fn peek(&self) -> char {
        if self.current >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current + 1]
        }
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.chars[self.current] != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn add_token(&mut self, tokentype: TokenType) {
        self.tokens
            .push(Token::new(tokentype, "".to_string(), self.line));
    }

    fn add_token_with_value(&mut self, tokentype: TokenType, value: String) {
        self.tokens.push(Token::new(tokentype, value, self.line));
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.chars[self.current - 1]
    }
    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }
}

struct Scanner {
    chars: Vec<char>,
    current: usize,
    start: usize,
    tokens: Vec<Token>,
    line: usize,
    new_line: bool,
}

fn is_digit_or_scientific(c: char) -> bool {
    c.is_ascii_digit() || c == 'e' || c == 'E'
}

fn is_alphanumeric(c: char) -> bool {
    is_alpha(c) || c.is_ascii_digit()
}

fn is_alpha(c: char) -> bool {
    // (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '$'
    // ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_' || c == '$'
    c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_' || c == '$'
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test() {
        let tokens = scan(
            r#"struct Customer:
                        id: u32,
                        first_name: string,
                        last_name: string,
                        date_fetched: date,"#,
        );
        let tokenstring = format!("{:?}", tokens);
        println!("{}", tokenstring);
        // assert_eq!(tokenstring,r#"[Token { tokentype: Fn, lexeme: "fn", line: 2 }, Token { tokentype: Identifier, lexeme: "get", line: 2 }, Token { tokentype: LeftParen, lexeme: "", line: 2 }, Token { tokentype: Identifier, lexeme: "id", line: 2 }, Token { tokentype: Colon, lexeme: "", line: 2 }, Token { tokentype: Identifier, lexeme: "u32", line: 2 }, Token { tokentype: RightParen, lexeme: "", line: 2 }, Token { tokentype: Minus, lexeme: "", line: 2 }, Token { tokentype: Greater, lexeme: "", line: 2 }, Token { tokentype: Identifier, lexeme: "Customer", line: 2 }, Token { tokentype: Colon, lexeme: "", line: 2 }, Token { tokentype: Identifier, lexeme: "service", line: 3 }, Token { tokentype: Dot, lexeme: "", line: 3 }, Token { tokentype: Identifier, lexeme: "get", line: 3 }, Token { tokentype: LeftParen, lexeme: "", line: 3 }, Token { tokentype: Identifier, lexeme: "id", line: 3 }, Token { tokentype: RightParen, lexeme: "", line: 3 }]"#)
    }
}
