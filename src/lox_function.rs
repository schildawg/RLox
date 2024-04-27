use std::cell::RefCell;
use std::rc::Rc;
use crate::smart_pointer::{copy_ref, new_cell_ref};

use crate::token::Token;
use crate::object::Object;

use crate::stmt::Stmt;

use crate::interpreter::Interpreter;

use crate::environment::Environment;
use crate::lox_class::LoxClass;
use crate::lox_instance::LoxInstance;

pub trait LoxCallable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Object>, class: Option<Rc<LoxClass>>) -> Object;
    fn to_string(&self) -> String;
}

#[derive(PartialEq, Debug)]
pub struct LoxFunction {
    name: Token,
    parameters: Vec<Token>,
    body: Vec<Rc<Stmt>>,
    pub closure: Rc<RefCell<Environment>>,
    is_initializer: bool,
}

impl LoxFunction {
    /// Factory method to create a new LoxFunction!
    ///
    pub fn new(declaration: Rc<Stmt>, closure: Rc<RefCell<Environment>>, is_initializer: bool) -> LoxFunction {
        if let Stmt::Function(token, parameters, body) = declaration.as_ref() {
            return LoxFunction { name: token.copy(), parameters: parameters.to_vec(), body: body.to_vec(), closure, is_initializer };
        }
        panic!("only takes functions!");
    }

    pub fn bind(&self, instance: Rc<RefCell<LoxInstance>>) -> LoxFunction {
        let mut environment = Environment::new();
        environment.enclosing = Some(copy_ref!(&self.closure));

        environment.define("this", Object::Instance(copy_ref!(&instance)));
        
        let stmt = Stmt::Function(self.name.copy(), self.parameters.to_vec(), self.body.to_vec());
        let stmt = Rc::new(stmt);

        LoxFunction::new(stmt, new_cell_ref!(environment), self.is_initializer)
    }
}

impl LoxCallable for LoxFunction {
    // Returns the number of parameters.
    // 
    fn arity(&self) -> usize {
        self.parameters.len()
    }

    // Runs the function!!
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Object>, _class: Option<Rc<LoxClass>>) -> Object {
        let mut environment = Environment::new();
        environment.enclosing = Some(copy_ref!(&self.closure));

        for i in 0..self.parameters.len() {
            environment.define(&self.parameters[i].lexeme, arguments[i].copy());
        }

        match interpreter.execute_block(&self.body, environment) {
            Ok(value) => { 
                if self.is_initializer {
                    return Environment::get_at(copy_ref!(&self.closure), 0, "this");
                }
                return value; 
            }
            Err(err) => { 
                if self.is_initializer {
                    return Environment::get_at(copy_ref!(&self.closure), 0, "this");
                }
                return err.return_value; 
            }
        }
    } 

    // The function's to_string().
    //
    fn to_string(&self) -> String {
        format!("<fn {}>", self.name.lexeme)
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;
    use crate::lox_function::{copy_ref, new_cell_ref};

    use crate::environment::Environment;
    use crate::object::Object;
    use crate::scanner::Scanner;
    use crate::parser::Parser;
    use crate::interpreter::Interpreter;

    use crate::lox_function::{LoxCallable, LoxFunction};

    // Make a new LoxFunction!
    //
    fn make_function() -> LoxFunction {
        let code = "
            fun fib(n) {
               if (n < 2) return n;
    
               return fib(n - 1) + fib(n - 2);
            }
        ";

        let mut scanner = Scanner::new(code.to_owned());      
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();

        // TODO: fix
        LoxFunction::new(copy_ref!(stmts.get(0).expect("REASON")), new_cell_ref!(Environment::new()), false)
    }

    // Tests Lox function to_string().
    //
    #[test]
    fn lox_function_new() {
        let uut = make_function();

        assert_eq!("<fn fib>", uut.to_string());
    }

    // Arity should be number of parameters.
    //
    #[test]
    fn lox_function_arity() {
        let uut = make_function();

        assert_eq!(1, uut.arity());
    }

    // Test calling a recursive function.
    //
    #[ignore]
    #[test]
    fn lox_function_call() {
        let uut = make_function();
        let uut = Rc::new(uut);

        let mut interpreter = Interpreter::new();

        interpreter.globals.borrow_mut().define("fib", Object::Function(copy_ref!(&uut)));

        let result = uut.call(&mut interpreter, vec![Object::Number(7.0)], None);

        assert_eq!(Object::Number(13.0), result);
    }
}



