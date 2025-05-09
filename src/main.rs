#![allow(dead_code)]
use std::process;

// Expand this
#[derive(Debug)]
enum TokenType {
    Number,
    Identifier,
    Equals,
    Let,
    OpenParen,
    CloseParen,
    BinaryOperator,
}

#[derive(Debug)]
struct Token {
    value: String,
    t_type: TokenType,
}

fn token(value: String, t_type: TokenType) -> Token {
    Token {
        value,
        t_type
    }
}

// Maybe rework this func but see how this works for now
fn tokenize(source: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    // Maybe find another way???
    let mut src: Vec<_> = source.chars().collect(); 
    
    while src.len() > 0 {
        match src[0] {
            '(' => {
               tokens.push(token(String::from(src.remove(0)), TokenType::OpenParen));
            }
            ')' => {
               tokens.push(token(String::from(src.remove(0)), TokenType::CloseParen));
            }
            '+' | '-' | '*' | '/' => {
               tokens.push(token(String::from(src.remove(0)), TokenType::BinaryOperator));
            }
            '=' => {
                tokens.push(token(String::from(src.remove(0)), TokenType::Equals));
            }
            ' ' | '\t' | '\n' => {
                src.remove(0);
            }
            _ => {
                // Handle multi-chars tokens
                if src[0].is_ascii_digit() {
                    let mut num = String::new();

                    while src.len() > 0 && src[0].is_ascii_digit() {
                        let c = src.remove(0);
                        num.push(c);
                    }
                    tokens.push(token(num, TokenType::Number));
                } else if src[0].is_alphabetic() {
                    let mut ident = String::new();

                    while src.len() > 0 && src[0].is_alphabetic() {
                        let c = src.remove(0);
                        ident.push(c);
                    }

                    // Expand for other keywords
                    match ident.as_str() {
                        "let" => {
                            tokens.push(token(ident, TokenType::Let));
                        }
                        _ => {
                            tokens.push(token(ident, TokenType::Identifier));
                        }
                    }
                } else {
                    println!("Unrecognized char found in source {}", src[0]);
                    process::exit(1);
                }
            }
        }
    }

    tokens
}

fn main() {
    let tokens = tokenize(String::from("123 let hello "));
    println!("{:?}", tokens);
}
