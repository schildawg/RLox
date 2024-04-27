
#[macro_use]
mod lox;

mod smart_pointer;
mod error;
mod environment;

mod object;
mod token;
mod expr;
mod stmt;

mod scanner;
mod parser;
mod visitor;
mod interpreter;
mod resolver;

mod lox_class;
mod lox_function;
mod lox_instance;

use std::{env, process};

use lazy_static::lazy_static;
use std::sync::Mutex;

use lox::Lox;

#[derive(Clone, PartialEq, Debug)]
struct Globals {
    had_error: bool,
    had_runtime_error: bool,

    last_error: String,
}

impl Globals {
    fn new() -> Globals {
        Globals { had_error: false, had_runtime_error: false, last_error: String::from("")}
    }
}

lazy_static! {
    static ref GLOBALS: Mutex<Globals> = Mutex::new(Globals::new());
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("usage: lox [script]");
        process::exit(64);
    }
    else if args.len() == 2 {
        Lox::run_file(&args[1]);
    }
    else {
        Lox::print_expr();
        Lox::run_prompt();
    }
}
