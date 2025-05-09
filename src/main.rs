use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::Command;

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    Number,
    Identifier,
    Equals,
    Let,
    Print,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Semicolon,
    BinaryOperator,
    StringLiteral,
}

#[derive(Debug, Clone)]
struct Position {
    line: usize,
    column: usize,
}

#[derive(Debug, Clone)]
struct Token {
    value: String,
    t_type: TokenType,
    pos: Position,
}

/// Errors produced during lexing (tokenization)
#[derive(Debug)]
enum LexError {
    UnexpectedChar { found: char, pos: Position },
    UnterminatedString { pos: Position },
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::UnexpectedChar { found, pos } =>
                write!(f, "Lex error at {}:{}: unexpected character '{}'", pos.line, pos.column, found),
            LexError::UnterminatedString { pos } =>
                write!(f, "Lex error at {}:{}: unterminated string literal", pos.line, pos.column),
        }
    }
}

impl std::error::Error for LexError {}

/// Errors produced during parsing
#[derive(Debug)]
enum ParseError {
    UnexpectedToken { expected: String, found: String, pos: Position },
    UnexpectedEof { expected: String },
    MissingMain,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found, pos } =>
                write!(f, "Parse error at {}:{}: expected {}, found '{}'", pos.line, pos.column, expected, found),
            ParseError::UnexpectedEof { expected } =>
                write!(f, "Parse error: unexpected end of input, expected {}", expected),
            ParseError::MissingMain =>
                write!(f, "Parse error: missing `main()` function"),
        }
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug)]
enum Statement {
    Let(String, Expression, Position),
    Print(Expression, Position),
}

#[derive(Debug)]
enum Expression {
    IntLiteral(i32, Position),
    Var(String, Position),
    StringLiteral(String, Position),
    BinaryOp(Box<Expression>, String, Box<Expression>, Position),
}

fn tokenize(source: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();
    let mut line = 1;
    let mut column = 1;
    while let Some(&c) = chars.peek() {
        let pos = Position { line, column };
        match c {
            '(' => { chars.next(); tokens.push(Token{value:"(".into(), t_type: TokenType::OpenParen, pos}); column+=1 }
            ')' => { chars.next(); tokens.push(Token{value:")".into(), t_type: TokenType::CloseParen, pos}); column+=1 }
            '{' => { chars.next(); tokens.push(Token{value:"{".into(), t_type: TokenType::OpenCurly, pos}); column+=1 }
            '}' => { chars.next(); tokens.push(Token{value:"}".into(), t_type: TokenType::CloseCurly, pos}); column+=1 }
            ';' => { chars.next(); tokens.push(Token{value:";".into(), t_type: TokenType::Semicolon, pos}); column+=1 }
            '+'|'-'|'*'|'/' => {
                let op = chars.next().unwrap();
                tokens.push(Token{value:op.to_string(), t_type: TokenType::BinaryOperator, pos});
                column+=1
            }
            '=' => { chars.next(); tokens.push(Token{value:"=".into(), t_type: TokenType::Equals, pos}); column+=1 }
            '"' => {
                chars.next(); column+=1;
                let mut s = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch == '"' { break }
                    if ch == '\n' {
                        return Err(LexError::UnterminatedString{ pos });
                    }
                    s.push(ch);
                    chars.next();
                    column+=1;
                }
                if chars.peek() != Some(&'"') {
                    return Err(LexError::UnterminatedString{ pos });
                }
                chars.next(); column+=1;
                tokens.push(Token{value:s, t_type: TokenType::StringLiteral, pos});
            }
            c if c.is_ascii_digit() => {
                let mut num = String::new();
                while let Some(&ch) = chars.peek() {
                    if !ch.is_ascii_digit() { break }
                    num.push(ch);
                    chars.next();
                    column+=1;
                }
                tokens.push(Token{value:num, t_type: TokenType::Number, pos});
            }
            c if c.is_alphabetic() => {
                let mut id = String::new();
                while let Some(&ch) = chars.peek() {
                    if !ch.is_alphabetic() { break }
                    id.push(ch);
                    chars.next();
                    column+=1;
                }
                let t_type = match id.as_str() {
                    "let" => TokenType::Let,
                    "print" => TokenType::Print,
                    _ => TokenType::Identifier,
                };
                tokens.push(Token{value:id, t_type, pos});
            }
            ' ' => { chars.next(); column+=1 }
            '\n' => { chars.next(); line+=1; column=1 }
            other => {
                return Err(LexError::UnexpectedChar{ found: other, pos });
            }
        }
    }
    Ok(tokens)
}

fn parse(tokens: Vec<Token>) -> Result<Vec<Statement>, ParseError> {
    let mut iter = tokens.into_iter().peekable();
    // Expect `main`
    let first = iter.next().ok_or(ParseError::MissingMain)?;
    if first.t_type != TokenType::Identifier || first.value != "main" {
        return Err(ParseError::UnexpectedToken {
            expected: "main".into(),
            found: first.value,
            pos: first.pos,
        });
    }
    // Expect ()
    let open = iter.next().ok_or(ParseError::UnexpectedEof{ expected:"(".into() })?;
    if open.t_type != TokenType::OpenParen {
        return Err(ParseError::UnexpectedToken{
            expected: "(".into(), found: open.value, pos: open.pos
        });
    }
    let close = iter.next().ok_or(ParseError::UnexpectedEof{ expected:")".into() })?;
    if close.t_type != TokenType::CloseParen {
        return Err(ParseError::UnexpectedToken{
            expected: ")".into(), found: close.value, pos: close.pos
        });
    }
    // Expect {
    let brace = iter.next().ok_or(ParseError::UnexpectedEof{ expected:"{".into() })?;
    if brace.t_type != TokenType::OpenCurly {
        return Err(ParseError::UnexpectedToken{
            expected: "{".into(), found: brace.value, pos: brace.pos
        });
    }

    let mut stmts = Vec::new();
    while let Some(tok) = iter.peek() {
        if tok.t_type == TokenType::CloseCurly {
            iter.next();
            break;
        }
        stmts.push(parse_stmt(&mut iter)?);
    }
    Ok(stmts)
}

fn parse_stmt<I>(iter: &mut std::iter::Peekable<I>) -> Result<Statement, ParseError>
where I: Iterator<Item=Token> {
    let tok = iter.next().unwrap();
    match tok.t_type {
        TokenType::Let => {
            let name_tok = iter.next().ok_or(ParseError::UnexpectedEof{ expected:"identifier".into() })?;
            if name_tok.t_type != TokenType::Identifier {
                return Err(ParseError::UnexpectedToken{
                    expected:"identifier".into(), found:name_tok.value, pos:name_tok.pos
                });
            }
            let eq = iter.next().ok_or(ParseError::UnexpectedEof{ expected:"=".into() })?;
            if eq.t_type != TokenType::Equals {
                return Err(ParseError::UnexpectedToken{
                    expected:"=".into(), found:eq.value, pos:eq.pos
                });
            }
            let expr = parse_expr(iter)?;
            let semi = iter.next().ok_or(ParseError::UnexpectedEof{ expected:";".into() })?;
            if semi.t_type != TokenType::Semicolon {
                return Err(ParseError::UnexpectedToken{
                    expected:";".into(), found:semi.value, pos:semi.pos
                });
            }
            Ok(Statement::Let(name_tok.value, expr, name_tok.pos))
        }
        TokenType::Print => {
            let ob = iter.next().ok_or(ParseError::UnexpectedEof{ expected:"(".into() })?;
            if ob.t_type != TokenType::OpenParen {
                return Err(ParseError::UnexpectedToken{
                    expected:"(".into(), found:ob.value, pos:ob.pos
                });
            }
            let expr = parse_expr(iter)?;
            let cb = iter.next().ok_or(ParseError::UnexpectedEof{ expected:")".into() })?;
            if cb.t_type != TokenType::CloseParen {
                return Err(ParseError::UnexpectedToken{
                    expected:")".into(), found:cb.value, pos:cb.pos
                });
            }
            let semi = iter.next().ok_or(ParseError::UnexpectedEof{ expected:";".into() })?;
            if semi.t_type != TokenType::Semicolon {
                return Err(ParseError::UnexpectedToken{
                    expected:";".into(), found:semi.value, pos:semi.pos
                });
            }
            Ok(Statement::Print(expr, tok.pos))
        }
        _ => Err(ParseError::UnexpectedToken{
            expected:"statement".into(), found:format!("{:?}",tok.t_type), pos:tok.pos
        })
    }
}

fn parse_expr<I>(iter: &mut std::iter::Peekable<I>) -> Result<Expression, ParseError>
where I: Iterator<Item=Token> {
    let first = iter.next().ok_or(ParseError::UnexpectedEof{ expected:"expression".into() })?;
    let mut left = match first.t_type {
        TokenType::Number => Expression::IntLiteral(first.value.parse().unwrap(), first.pos),
        TokenType::Identifier => Expression::Var(first.value, first.pos),
        TokenType::StringLiteral => Expression::StringLiteral(first.value, first.pos),
        _ => return Err(ParseError::UnexpectedToken{
            expected:"literal or identifier".into(), found:first.value, pos:first.pos
        })
    };
    if let Some(peek) = iter.peek() {
        if peek.t_type == TokenType::BinaryOperator {
            let op = iter.next().unwrap();
            let right = parse_expr(iter)?;
            left = Expression::BinaryOp(Box::new(left), op.value, Box::new(right), op.pos);
        }
    }
    Ok(left)
}

fn gen_expr(e: Expression) -> String {
    match e {
        Expression::IntLiteral(n, _) => n.to_string(),
        Expression::Var(v, _) => v,
        Expression::StringLiteral(s, _) => format!("\"{}\"", s),
        Expression::BinaryOp(l, op, r, _) => {
            format!("({} {} {})", gen_expr(*l), op, gen_expr(*r))
        }
    }
}

fn gen_c(stmts: Vec<Statement>) -> String {
    let mut out = String::from("#include <stdio.h>\n\n");
    out.push_str("void print_int(int v) { printf(\"%d\\n\", v); }\n\n");
    out.push_str("int main() {\n");
    for s in stmts {
        match s {
            Statement::Let(name, expr, _) =>
                out.push_str(&format!("    int {} = {};\n", name, gen_expr(expr))),
            Statement::Print(expr, _) =>
                out.push_str(&format!("    print_int({});\n", gen_expr(expr))),
        }
    }
    out.push_str("    return 0;\n}\n");
    out
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: psyduck <input.psy>");
        std::process::exit(1);
    }
    let in_path = Path::new(&args[1]);
    let src = fs::read_to_string(in_path)?;
    let tokens = tokenize(&src)?;
    let ast = parse(tokens)?;
    let code = gen_c(ast);

    let c_path = in_path.with_extension("c");
    fs::write(&c_path, code)?;

    let exe = in_path.with_extension("");
    let status = Command::new("gcc")
        .args(&["-O2", c_path.to_str().unwrap(), "-o", exe.to_str().unwrap()])
        .status()?;
    if !status.success() {
        std::process::exit(1);
    }
    fs::remove_file(c_path)?;
    println!("Generated executable at {:?}", exe);
    Ok(())
}

