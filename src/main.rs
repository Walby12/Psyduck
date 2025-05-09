use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    Number,
    Identifier,
    Let,
    Print,
    If,
    Else,
    While,
    Return,
    Equals,
    EqEq,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Plus,
    Minus,
    Star,
    Slash,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Semicolon,
    StringLiteral,
}

#[derive(Debug, Clone)]
struct Position {
    line: usize,
    column: usize,
}

#[derive(Debug, Clone)]
struct Token {
    t_type: TokenType,
    value: String,
    pos: Position,
}

#[derive(Debug)]
enum Statement {
    Let(String, Expression),
    Print(Expression),
    Assign(String, Expression),
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
    While(Expression, Vec<Statement>),
    Return(Option<Expression>),
}

#[derive(Debug)]
enum Expression {
    Int(i32),
    Var(String),
    Str(String),
    Binary(Box<Expression>, String, Box<Expression>),
}

// --- Lexer --------------------------------------------------------------

fn tokenize(src: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut line = 1;
    let mut col = 1;
    let mut chars = src.chars().peekable();

    while let Some(&c) = chars.peek() {
        let pos = Position { line, column: col };
        match c {
            ' ' | '\t' => { chars.next(); col += 1; }
            '\n' => { chars.next(); line += 1; col = 1; }
            '(' => { tokens.push(Token { t_type: TokenType::OpenParen, value: "(".into(), pos }); chars.next(); col += 1; }
            ')' => { tokens.push(Token { t_type: TokenType::CloseParen, value: ")".into(), pos }); chars.next(); col += 1; }
            '{' => { tokens.push(Token { t_type: TokenType::OpenCurly, value: "{".into(), pos }); chars.next(); col += 1; }
            '}' => { tokens.push(Token { t_type: TokenType::CloseCurly, value: "}".into(), pos }); chars.next(); col += 1; }
            ';' => { tokens.push(Token { t_type: TokenType::Semicolon, value: ";".into(), pos }); chars.next(); col += 1; }
            '+' => { tokens.push(Token { t_type: TokenType::Plus, value: "+".into(), pos }); chars.next(); col += 1; }
            '-' => { tokens.push(Token { t_type: TokenType::Minus, value: "-".into(), pos }); chars.next(); col += 1; }
            '*' => { tokens.push(Token { t_type: TokenType::Star, value: "*".into(), pos }); chars.next(); col += 1; }
            '/' => { tokens.push(Token { t_type: TokenType::Slash, value: "/".into(), pos }); chars.next(); col += 1; }
            '=' => {
                chars.next(); col += 1;
                if chars.peek() == Some(&'=') {
                    chars.next(); col += 1;
                    tokens.push(Token { t_type: TokenType::EqEq, value: "==".into(), pos });
                } else {
                    tokens.push(Token { t_type: TokenType::Equals, value: "=".into(), pos });
                }
            }
            '!' => {
                chars.next(); col += 1;
                if chars.peek() == Some(&'=') {
                    chars.next(); col += 1;
                    tokens.push(Token { t_type: TokenType::NotEq, value: "!=".into(), pos });
                } else {
                    panic!("Unexpected '!' at line {}, column {}", line, col);
                }
            }
            '<' => {
                chars.next(); col += 1;
                if chars.peek() == Some(&'=') {
                    chars.next(); col += 1;
                    tokens.push(Token { t_type: TokenType::LessEq, value: "<=".into(), pos });
                } else {
                    tokens.push(Token { t_type: TokenType::Less, value: "<".into(), pos });
                }
            }
            '>' => {
                chars.next(); col += 1;
                if chars.peek() == Some(&'=') {
                    chars.next(); col += 1;
                    tokens.push(Token { t_type: TokenType::GreaterEq, value: ">=".into(), pos });
                } else {
                    tokens.push(Token { t_type: TokenType::Greater, value: ">".into(), pos });
                }
            }
            '"' => {
                chars.next(); col += 1;
                let mut s = String::new();
                while let Some(&ch2) = chars.peek() {
                    if ch2 == '"' { break; }
                    s.push(ch2);
                    chars.next(); col += 1;
                }
                if chars.next().is_none() {
                    panic!("Unterminated string literal at line {}, column {}", line, col);
                }
                col += 1;
                tokens.push(Token { t_type: TokenType::StringLiteral, value: s, pos });
            }
            c if c.is_ascii_digit() => {
                let mut n = String::new();
                while let Some(&d) = chars.peek() {
                    if !d.is_ascii_digit() { break; }
                    n.push(d);
                    chars.next(); col += 1;
                }
                tokens.push(Token { t_type: TokenType::Number, value: n, pos });
            }
            c if c.is_alphabetic() => {
                let mut id = String::new();
                while let Some(&d) = chars.peek() {
                    if !d.is_alphanumeric() { break; }
                    id.push(d);
                    chars.next(); col += 1;
                }
                let t_type = match id.as_str() {
                    "let" => TokenType::Let,
                    "write" => TokenType::Print,
                    "if" => TokenType::If,
                    "else" => TokenType::Else,
                    "while" => TokenType::While,
                    "return" => TokenType::Return,
                    _ => TokenType::Identifier,
                };
                tokens.push(Token { t_type, value: id, pos });
            }
            _ => panic!("Unexpected character '{}' at line {}, column {}", c, line, col),
        }
    }
    tokens
}

// --- Parser -------------------------------------------------------------

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self { Parser { tokens, pos: 0 } }
    fn peek(&self) -> Option<&Token> { self.tokens.get(self.pos) }
    fn next(&mut self) -> Option<Token> { 
        let t = self.peek().cloned(); 
        if t.is_some() { 
            self.pos += 1; 
        } 
        t 
    }
    fn expect(&mut self, ty: TokenType) -> Token {
        let tok = self.next().unwrap_or_else(|| panic!("Expected {:?}, got EOF", ty));
        if tok.t_type != ty { 
            panic!("Expected {:?}, got {:?} at line {}, column {}", 
                  ty, tok.t_type, tok.pos.line, tok.pos.column) 
        }
        tok
    }
    
    fn parse(&mut self) -> Vec<Statement> {
        let main_tok = self.next().expect("Expected start");
        if main_tok.value != "start" { 
            panic!("Source must start with 'start()' at line {}, column {}", 
                  main_tok.pos.line, main_tok.pos.column); 
        }
        self.expect(TokenType::OpenParen);
        self.expect(TokenType::CloseParen);
        self.expect(TokenType::OpenCurly);
        let mut stmts = Vec::new();
        while self.peek().map(|t| t.t_type.clone()) != Some(TokenType::CloseCurly) {
            stmts.push(self.parse_stmt());
        }
        self.expect(TokenType::CloseCurly);
        stmts
    }
    
    fn parse_stmt(&mut self) -> Statement {
        let tok = self.peek().unwrap().clone();
        match tok.t_type {
            TokenType::Let => {
                self.next();
                let name = self.expect(TokenType::Identifier).value;
                self.expect(TokenType::Equals);
                let expr = self.parse_expr();
                self.expect(TokenType::Semicolon);
                Statement::Let(name, expr)
            }
            TokenType::Identifier => {
                let name = self.next().unwrap().value;
                self.expect(TokenType::Equals);
                let expr = self.parse_expr();
                self.expect(TokenType::Semicolon);
                Statement::Assign(name, expr)
            }
            TokenType::Print => {
                self.next();
                self.expect(TokenType::OpenParen);
                let expr = self.parse_expr();
                self.expect(TokenType::CloseParen);
                self.expect(TokenType::Semicolon);
                Statement::Print(expr)
            }
            TokenType::If => {
                self.next();
                self.expect(TokenType::OpenParen);
                let cond = self.parse_expr();
                self.expect(TokenType::CloseParen);
                self.expect(TokenType::OpenCurly);
                let mut then_b = Vec::new();
                while self.peek().map(|t| t.t_type.clone()) != Some(TokenType::CloseCurly) {
                    then_b.push(self.parse_stmt());
                }
                self.expect(TokenType::CloseCurly);
                let mut else_b = None;
                if self.peek().map(|t| t.t_type.clone()) == Some(TokenType::Else) {
                    self.next();
                    self.expect(TokenType::OpenCurly);
                    let mut else_block = Vec::new();
                    while self.peek().map(|t| t.t_type.clone()) != Some(TokenType::CloseCurly) {
                        else_block.push(self.parse_stmt());
                    }
                    self.expect(TokenType::CloseCurly);
                    else_b = Some(else_block);
                }
                Statement::If(cond, then_b, else_b)
            }
            TokenType::While => {
                self.next();
                self.expect(TokenType::OpenParen);
                let cond = self.parse_expr();
                self.expect(TokenType::CloseParen);
                self.expect(TokenType::OpenCurly);
                let mut body = Vec::new();
                while self.peek().map(|t| t.t_type.clone()) != Some(TokenType::CloseCurly) {
                    body.push(self.parse_stmt());
                }
                self.expect(TokenType::CloseCurly);
                Statement::While(cond, body)
            }
            TokenType::Return => {
                self.next();
                let expr = if self.peek().map(|t| t.t_type.clone()) != Some(TokenType::Semicolon) {
                    Some(self.parse_expr())
                } else { None };
                self.expect(TokenType::Semicolon);
                Statement::Return(expr)
            }
            _ => panic!("Unexpected token in statement: {:?} at line {}, column {}", 
                       tok.t_type, tok.pos.line, tok.pos.column),
        }
    }
    
    fn parse_expr(&mut self) -> Expression {
        let mut left = match self.next().unwrap() {
            Token { t_type: TokenType::Number, value, .. } => Expression::Int(value.parse().unwrap()),
            Token { t_type: TokenType::Identifier, value, .. } => Expression::Var(value),
            Token { t_type: TokenType::StringLiteral, value, .. } => Expression::Str(value),
            t => panic!("Unexpected token in expr: {:?} at line {}, column {}", 
                       t.t_type, t.pos.line, t.pos.column),
        };
        
        if let Some(next) = self.peek() {
            let op = match next.t_type {
                TokenType::Plus | TokenType::Minus 
                | TokenType::Star | TokenType::Slash
                | TokenType::EqEq | TokenType::NotEq
                | TokenType::Less | TokenType::Greater
                | TokenType::LessEq | TokenType::GreaterEq => {
                    let o = next.value.clone();
                    self.next();
                    o
                }
                _ => return left,
            };
            let right = self.parse_expr();
            left = Expression::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }
}

// --- Codegen ------------------------------------------------------------

fn gen_expr(e: Expression) -> String {
    match e {
        Expression::Int(n) => n.to_string(),
        Expression::Var(v) => v,
        Expression::Str(s) => format!("\"{}\"", s),
        Expression::Binary(l, op, r) => format!("({} {} {})", gen_expr(*l), op, gen_expr(*r)),
    }
}

fn gen_stmt(s: Statement) -> String {
    match s {
        Statement::Let(n, e) => format!("    int {} = {};\n", n, gen_expr(e)),
        Statement::Assign(n, e) => format!("    {} = {};\n", n, gen_expr(e)),
        Statement::Print(e) => format!("    print_int({});\n", gen_expr(e)),
        Statement::If(c, t, e) => {
            let mut out = format!("    if ({}) {{\n", gen_expr(c));
            for st in t { out.push_str(&gen_stmt(st)); }
            out.push_str("    }");
            if let Some(else_block) = e {
                out.push_str(" else {\n");
                for st in else_block { out.push_str(&gen_stmt(st)); }
                out.push_str("    }");
            }
            out.push_str("\n");
            out
        }
        Statement::While(c, body) => {
            let mut out = format!("    while ({}) {{\n", gen_expr(c));
            for st in body { out.push_str(&gen_stmt(st)); }
            out.push_str("    }\n");
            out
        }
        Statement::Return(opt) => {
            if let Some(e) = opt {
                format!("    return {};\n", gen_expr(e))
            } else {
                "    return;\n".into()
            }
        }
    }
}

fn gen_c(stmts: Vec<Statement>) -> String {
    let mut out: String = String::from(
        "#include <stdio.h>\n\n\
         void print_int(int v) { printf(\"%d\\n\", v); }\n\n\
         int main() {\n"
    );
    for s in stmts {
        out.push_str(&gen_stmt(s));
    }
    out.push_str("    return 0;\n}\n");
    out
}

// --- Main ---------------------------------------------------------------

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: psyduck <file.psy>");
        std::process::exit(1);
    }
    let in_path = Path::new(&args[1]);
    let source = fs::read_to_string(in_path).expect("read failed");
    let tokens = tokenize(&source);
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    let c = gen_c(ast);

    let c_path = in_path.with_extension("c");
    fs::write(&c_path, c).expect("write .c failed");

    let exe = in_path.with_extension("");
    let status = Command::new("gcc")
        .args(&["-O2", c_path.to_str().unwrap(), "-o", exe.to_str().unwrap()])
        .status()
        .expect("gcc failed");
    
    if !status.success() {
        eprintln!("Compilation failed");
        std::process::exit(1);
    }

    fs::remove_file(c_path).ok();
    println!("-> {:?}", exe);
}
