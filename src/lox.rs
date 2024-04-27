use std::{process, fs};
use std::{io, io::Write};

use crate::GLOBALS;
use crate::error::RuntimeError;

use crate::object::Object;
use crate::token::{Token, TokenType, token};
use crate::expr::{Expr, AstPrinter};
use crate::expr::{number, grouping, binary, unary};

use crate::scanner::Scanner;
use crate::parser::Parser;
use crate::interpreter::Interpreter;
use crate::resolver::Resolver;

pub struct Lox;
impl Lox {
    pub fn print_expr() {
        let expr = binary!(
            unary!(token!(TokenType::Minus, "-", Object::None, 1), number!(123.4)),
            token!(TokenType::Star, "*", Object::None, 1),
            grouping!(number!(45.67))
        );
    
        let mut printer = AstPrinter{};
        printer.print(&expr);
    }
    
    /// Runs a Lox file.
    ///
    /// # Errors
    /// 
    /// If parse error, exits program with code 65
    /// If runtime error, exits program with code 70
    ///
    /// # Panics
    ///
    /// Panics if cannot read file.
    ///
    pub fn run_file(path: &str) {
        let contents = fs::read_to_string(path).expect("unable to read file");
        let mut interpreter = Interpreter::new();

        Self::run(&contents, &mut interpreter);
    
        if GLOBALS.lock().unwrap().had_error {
            process::exit(65);
        }
        if GLOBALS.lock().unwrap().had_runtime_error {
            process::exit(70);
        }
    }
    
    /// Runs a REPL.
    ///
    /// # Panics
    ///
    /// Panics if cannot read from standard in.
    ///
    pub fn run_prompt() {
        let mut interpreter = Interpreter::new();

        loop {
            print!("> ");
            io::stdout().flush().unwrap();
    
            let mut line = String::new();
            io::stdin()
                .read_line(&mut line)
                .expect("Failed to read line");
    
            Self::run(&line, &mut interpreter);
            println!();
    
            GLOBALS.lock().unwrap().had_error = false;
        }
    }
    
    fn run(source: &str, interpreter: &mut Interpreter) {
        let mut scanner = Scanner::new(String::from(source));
        scanner.scan_tokens();
    
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
    
        if GLOBALS.lock().unwrap().had_error {
            return;
        }

        let mut resolver = Resolver::new();
        resolver.resolve(&stmts);
        if GLOBALS.lock().unwrap().had_error {
            return;
        }
        
        interpreter.locals.clear();
        interpreter.locals.extend(resolver.locals.into_iter());

        interpreter.interpret(&stmts);
    }
    
    pub fn error(line: usize, message: &str) {
        Self::report(line, "", message);
    }
    
    /// Handles a runtime error.
    ///
    /// Prints an error message and marks global runtime error flag to true.
    ///
    pub fn runtime_error(error: RuntimeError) {
        eprintln!("{}\n[line {}]", error.message, error.token.line);
        let mut globals = GLOBALS.lock().unwrap();
        
        globals.had_runtime_error = true;
        globals.last_error = error.message;
    }
    
    /// Handles an error associated with a parsed token.
    ///
    /// Prints an error message and sets global error indicator to true.
    ///
    pub fn token_error(token: &Token, message: &str) {
        if token.token_type == TokenType::Eof {
            Self::report(token.line, "at end", message);
        }
        else {
            let where_at = format!("at '{}'", token.lexeme);
    
            Self::report(token.line, &where_at, message);
        }
    }
    
    fn report(line: usize, where_at: &str, message: &str) {
        let error_message = format!("[line {line}] Error {where_at}: {message}");
    
        eprintln!("{}", error_message);
        
        let mut globals = GLOBALS.lock().unwrap();
        
        globals.had_error = true;
        globals.last_error = error_message;
    }
}
