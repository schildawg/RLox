use std::time::{SystemTime, UNIX_EPOCH};
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;


use ahash::{AHasher, RandomState};

use crate::lox::Lox;

use crate::error::RuntimeError;
use crate::error::runtime_error;

use crate::smart_pointer::{copy_ref, new_cell_ref};

use crate::object::Object;
use crate::token::{Token, TokenType};

use crate::expr::Expr;
use crate::stmt::Stmt;

use crate::visitor::Visitor;

use crate::environment::Environment;
use crate::lox_function::{LoxCallable, LoxFunction};
use crate::lox_class::{LoxClass};

/// Clock.  Implements native function clock().
///
#[derive(PartialEq, Clone, Debug)]
pub struct Clock;

impl LoxCallable for Clock {
    /// Takes no arguments.
    ///
    fn arity(&self) -> usize {
        0
    }

    /// Calculates seconds since the epoch.
    ///
    fn call(&self, _interpreter: &mut Interpreter, _arguments: Vec<Object>, _class: Option<Rc<LoxClass>>) -> Object {
        let start = SystemTime::now();
        let since_the_epoch = start
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards");

            //println!("{}", since_the_epoch.as_secs() as f64);

        Object::Number(since_the_epoch.as_secs() as f64)
    }

    /// Display value is <fn native clock>
    ///
    fn to_string(&self) -> String {
        format!("<fn native clock>")
    }
}

// Interpreter.  Runs a syntax tree created by Scanner and Parser.
//
/// # Example
///
/// ```
/// let mut scanner = Scanner::new(String::from("var test = 5.0;"));
/// scanner.scan_tokens();
///
/// let mut parser = Parser::new(scanner.tokens);
/// let stmts = parser.parse();
/// let mut interpreter = Interpreter::new();
///
/// interpreter.interpret(&stmts);
///
/// let name = token!(TokenType::Identifier, "test", Object::None, 1);
/// let value = interpreter.globals.borrow().get(&name).expect("Should be found!");
///
/// assert_eq!(Object::Number(5.0), value);
/// ```
pub struct Interpreter {
    pub globals: Rc<RefCell<Environment>>,
    pub environment: Rc<RefCell<Environment>>,
    pub locals: HashMap<String, usize, RandomState>,
}

impl Interpreter {
    // Factory method to create a new Interpreter.  Sets the environment, and registers native functions.
    //
    // # Native Functions
    // 
    // clock() - see: ClockFunction
    //
    pub fn new() -> Interpreter {
        let env = new_cell_ref!(Environment::new());
        env.borrow_mut().define("clock", Object::ClockFunction(Clock{}));

        Interpreter { globals: copy_ref!(&env), environment: copy_ref!(&env), locals: HashMap::default() }
    }

    // Executes a single statement.  If a problem is encountered, returns a runtime error.
    //
    pub fn execute(&mut self, stmt: &Rc<Stmt>) -> Result<(), RuntimeError> {
       self.visit_stmt(stmt)
    }

    // Runs a list of statements.
    //
    pub fn interpret(&mut self, statements: &Vec<Rc<Stmt>>) {
        for statement in statements {
            let result = self.visit_stmt(statement);
            if let Err(err) = result {
                Lox::runtime_error(err);
            }
        }
    }

    // Executes a block.  A block is a list of statements with its own environment for defining and assigning variables.  It 
    // also has visibility to assign variables in higher scopes.
    //
    pub fn execute_block(&mut self, statements: &Vec<Rc<Stmt>>, environment: Environment) -> Result<Object, RuntimeError> {
        let previous = copy_ref!(&self.environment);

        
        let mut environment = environment;
        if environment.enclosing == None {
           environment.enclosing = Some(copy_ref!(&self.environment));
        }

        self.environment = new_cell_ref!(environment);
        
        for statement in statements {
            let value = self.visit_stmt(&statement);
            match value {
                Err(err) => {
                    self.environment = previous;
                    return Err(err);
                }
                _ => ()
            }
        }
        self.environment = previous;

        Ok(Object::None)
    }

    // Evaluates an expression.  Return a runtime error if an error occurs.
    //
    fn evaluate(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
        self.visit_expr(expr)
    }

    // Returns a runtime error if both expressions are not numbers.
    //
    fn check_number_operands(left: &Object, right: &Object, operator: &Token) -> Result<(), RuntimeError> {
        if !left.is_number() || !right.is_number() {
            return Err(runtime_error!(operator, "Operands must be numbers."));
        }
        Ok(())
    }

    // Finds a variable from ancestor environment based on the distance calculated in Resolver.
    //
    fn lookup_variable(&self, name: &Token, id: &str) -> Object {
        if self.locals.contains_key(id) {
           
            let distance = self.locals.get(id).unwrap();
            
            return Environment::get_at(copy_ref!(&self.environment), *distance, &name.lexeme);
        }
        else {
            return self.globals.borrow_mut().get(name).expect("DON'T PANIC!!");
        }
    }
}

impl Visitor for Interpreter {
    // Assigns a value to a variable defined in the current or enclosed environment.  Reports and runtime errors to Lox.
    //
    fn visit_assign(&mut self, id: &str, token: &Token, expr: &Expr) -> Result<Object, RuntimeError> {
        let value = self.evaluate(expr)?;

        if self.locals.contains_key(id) {
            let distance = self.locals.get(id).unwrap();
            Environment::assign_at(copy_ref!(&self.environment), *distance, token, value);
        }
        else {
            self.globals.borrow_mut().assign(token, value)?;
        }
        Ok(Object::None)
        
        // match self.environment.borrow_mut().assign(token, value) {
        //     Ok(_value) => {
        //         return Ok(Object::None);
        //     }
        //     Err(err) => {
        //         Lox::runtime_error(RuntimeError{token: token, message:err.message, return_value: Object::None});
        //         return Ok(Object::None);
        //     }
        // }
    }

    // Evaluates a binary expression.  Valid operators are Minus, Slash, Star, Plas, Greater, GreaterEqual, Less, LessEqual, 
    // BangEqual, and EqualEqual. 
    //
    // # Errors
    // 
    // Both left and right must be numbers for Minus, Slash, Star, Greater, GreaterEqual, Less, and LessEqual.  If not a 
    // runtime error is returned.  For Plus if both left and right are not number or strings, a runtime error is returned.
    // 
    fn visit_binary(&mut self, left: &Expr, operator: &Token, right: &Expr) -> Result<Object, RuntimeError> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        match operator.token_type {
            // Returns value of left minus right.  Example: 2 - 1 = 1.
            TokenType::Minus => {
                Self::check_number_operands(&left, &right, operator)?;
                Ok(Object::Number(left.to_number() - right.to_number()))
            },

            // Returns value of left divided by right.  Example: 3 / 2 = 1.5.
            TokenType::Slash => {
                Self::check_number_operands(&left, &right, operator)?;
                Ok(Object::Number(left.to_number() / right.to_number()))
            },       

            // Returns value of left times right.  Example: 2 * 2 = 4.
            TokenType::Star => {
                Self::check_number_operands(&left, &right, operator)?;
                Ok(Object::Number(left.to_number() * right.to_number()))
            },

            // Adds or concatenates left and right.
            TokenType::Plus  => {
                // Both are numbers.  Add left to right.  Example: 2 + 2 = 4.
                if left.is_number() && right.is_number() {
                    return Ok(Object::Number(left.to_number() + right.to_number()))
                }
                // Both are strings.  Concat left and right.  Example:  "ABC + "DEF" = "ABCDEF".
                else if left.is_string() && right.is_string() {
                    return Ok(Object::String(left.to_string() + &right.to_string()))
                }
                Err(runtime_error!(operator, "Operands must be two numbers or two strings."))
            }
            
            // Returns true if left is greater than right, otherwise false.  Example: 2 > 1.
            TokenType::Greater => {
                Self::check_number_operands(&left, &right, operator)?;
                return Ok(Object::Boolean(left.to_number() > right.to_number()))
            }

            // Returns true if left is greater than or equal to right.  Example: 1 >= 1.
            TokenType::GreaterEqual => {
                Self::check_number_operands(&left, &right, operator)?;
                Ok(Object::Boolean(left.to_number() >= right.to_number()))
            }

            // Returns true if left is less than right, false otherwise.  Example: 1 < 2. 
            TokenType::Less => {
                Self::check_number_operands(&left, &right, operator)?;               
                Ok(Object::Boolean(left.to_number() < right.to_number()))
            }
            
            // Returns true if left is less than or equal to right, false otherwise.  Example: 1 <= 1. 
            TokenType::LessEqual => {
                Self::check_number_operands(&left, &right, operator)?;             
                Ok(Object::Boolean(left.to_number() <= right.to_number()))
            }

            // Returns true if left is not equal to right, false otherwise.  Examples:  1 != 2, 1 != "hi", 1 != false.
            TokenType::BangEqual  => Ok(Object::Boolean(left != right)),

            // Returns true if left is equal to right, false otherwise.  Examples:  1 == 1, true == true, "hi" == "hi"
            TokenType::EqualEqual => Ok(Object::Boolean(left == right)),

            _ => panic!("not supported"),
        }
    }

    // Calls a function!
    //
    // # Errors
    //
    // Returns a runtime error if passed the wrong number of arguments.
    // Returns a runtime error if attempting to call a non-function.  ex: "not a function"()
    //
    fn visit_call(&mut self, expr: &Expr, token: &Token, args: &Vec<Expr>) -> Result<Object, RuntimeError> {
        let callee = self.evaluate(expr)?;
        
        match callee.copy() {
            Object::Function(function) => {
               if args.len() != function.arity() {
                   let message = format!("Expected {} arguments but got {}.", function.arity(), args.len());
                   Lox::runtime_error(runtime_error!(token, message));
                   return Ok(Object::None);                   
               }
            }
            Object::Class(_) => 
            {         
               // TODO: check number of arguments???
            }
            Object::ClockFunction(_) => {}
            _ => {
               Lox::runtime_error(runtime_error!(token, String::from("Can only call functions and classes.")));

               return Ok(Object::None);
            }                
        }

        let mut arguments = Vec::new();
        for argument in args {
            arguments.push(self.evaluate(argument)?);
        }

        match callee.call(self, arguments, None) {
            Ok(value) => { return Ok(value); }
            Err(_) => { panic!("NOT GONNA HAPPEN"); }   // TODO: ???
        }
    }

    // Evaluates a class instance's getter.
    //
    // # Errors
    //
    // Returns a runtime error if attempt to invoke on a non-instance.
    //
    fn visit_get(&mut self, object: &Expr, name: &Token) -> Result<Object, RuntimeError> {
        let object = self.evaluate(object)?;
        if let Object::Instance(instance) = object {
            let result = instance.borrow().get(name, copy_ref!(&instance));
            match result {
                Ok(_) => return result,
                Err(_) => return Ok(Object::String("{undefined}".to_string())), 
            }    
        }  
        Err(runtime_error!(name, "Only instances have properties."))
    }

    // Evalaluates the inside expression and returns the value.
    //
    fn visit_grouping(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
        self.evaluate(expr)
    }

    // Returns the literal's value.
    //
    fn visit_literal(&mut self, value: &Object) -> Result<Object, RuntimeError> {
        Ok(value.copy())
    }

    // Executes logical operators "or" and "and". Expressions use "truthy" evaluation.  For values other than booleans, 
    // nil is false and any other value is true.
    //
    fn visit_logical(&mut self, left: &Expr, operator: &Token, right: &Expr) -> Result<Object, RuntimeError> {
        let left = self.evaluate(left)?;

        match operator.token_type {
            TokenType::Or => {
                // true or true   = true
                // true or false  = true
                // false or true  = true;
                // false or false = false;
                if left.is_truthy() {
                    return Ok(left);
                }    
            },
            TokenType::And => {
                // true and true   = true
                // true and false  = false
                // false and true  = false;
                // false and false = false;
                if !left.is_truthy() {
                    return Ok(left);
                }                            
            },
            _ => panic!("can't happen")
        }
        return Ok(self.evaluate(right)?);
    }

    // Evaluates a class instance's setter.
    //
    fn visit_set(&mut self, object: &Expr, name: &Token, value: &Expr) -> Result<Object, RuntimeError> {
        let object = self.evaluate(object)?;

        if let Object::Instance(instance) = object {
            let value = self.evaluate(value)?;
            
            instance.borrow_mut().set(name, value.copy());

            return Ok(value);    
        }  
        
        Err(runtime_error!(name, "Only instances have fields."))
    }  

    fn visit_super(&mut self, _keyword: &Token, name: &Token) -> Result<Object, RuntimeError> {
        let distance = *self.locals.get("$$").unwrap();
        
        //?let env = self.environment.borrow_mut();

        let superclass = Environment::get_at(copy_ref!(&self.environment), distance, "super");
        let object = Environment::get_at(copy_ref!(&self.environment), distance - 1, "this");

        if let Object::Class(klass) = superclass {
            if let Object::Instance(instance) = object {
                let method = klass.find_method(&name.lexeme);
                match method {
                    Some(method) => {
                        let function = method.bind(instance);
                        let function = Rc::new(function);

                        return Ok(Object::Function(function));
                    }
                    None => {
                        let message = format!("Undefined property '{}'.", name.lexeme);

                        // TODO: Should not be called here.
                        Lox::token_error(name, &message);

                        return Err(runtime_error!(name, message));
                    }
                }
            }
        }
        panic!("DON'T PANIC");
    }
    
    fn visit_this(&mut self, keyword: &Token) -> Result<Object, RuntimeError> {
        Ok(self.lookup_variable(keyword, "??"))
    }

    // Evaluates a unary expression.  Supports operators Bang and Minus.
    // 
    // # Errors
    //
    // Returns a runtime error if the expression of minus is not a number.
    // 
    fn visit_unary(&mut self, operator: &Token, value: &Expr) -> Result<Object, RuntimeError> {
        let right = self.evaluate(value)?;

        match operator.token_type {
            // Negates an expression based on its "truthy" value.  Nil is considered false, and any valid value is true.
            // Examples: !true = false, !false = true, !nil = true, !123 = false, !"hi" = false.
            TokenType::Bang  => Ok(Object::Boolean(!right.is_truthy())),

            // Negates a number value.  Examples -(1) = 1, 1(-1) = 1.
            TokenType::Minus => {
                if !right.is_number() {
                    return Err(runtime_error!(operator, "Operand must be number."));
                }
                Ok(Object::Number(-1.0 * right.to_number()))
            },          
            _ => panic!("can't happen")
        }
    }

    // Gets a variable from environment.  If there's a runtime error, reports it to Lox.
    //
    fn visit_variable(&mut self, id: &str, token: &Token) -> Result<Object, RuntimeError> {
        Ok(self.lookup_variable(token, id))

        // match self.environment.borrow_mut().get(token) {
        //     Ok(value) => {
        //         return Ok(value);
        //     }
        //     Err(err) => {
        //         Lox::runtime_error(RuntimeError{token: token, message:err.message, return_value: Object::None});
        //         return Ok(Object::None);
        //     }
        // }
    }
 
    // Executes a block.  Creates a new environment, enclosing the current one.  Returns a runtime error if a 
    //
    fn visit_block(&mut self, stmts: &Vec<Rc<Stmt>>) -> Result<(), RuntimeError> {
        let env = Environment::new();

        self.execute_block(stmts, env)?;
        Ok(())
    }

    // Creates a class!
    //    
    fn visit_class(&mut self, name: &Token, superclass: &Expr, methods: &Vec<Rc<Stmt>>) -> Result<(), RuntimeError> {
        let mut superklass = None;
        let mut has_superclass = false;

        if let Expr::Variable(_, super_name) = superclass {
            let eval = self.evaluate(superclass);
            if let Err(err) = eval {
                panic!("{:?}", err);
            }

            if let Object::Class(class) = eval.unwrap() {
                superklass = Some(class);
                has_superclass = true;
            }
            else { //if eval.obj_type() != "Class" {
                return Err(runtime_error!(super_name, "Superclass must be a class."));
            }
        }
        //self.environment.borrow_mut().define(&name.lexeme, Object::None);
        self.globals.borrow_mut().define(&name.lexeme, Object::None);

        // TODO: Wrap head around this one!!!
        let class_ref: Rc<LoxClass> = if has_superclass { superklass.unwrap() } else { Rc::new(LoxClass::new("None", None, HashMap::default()))};

        if let Expr::Variable(_,_) = superclass {
            let mut environment = Environment::new();
            environment.enclosing = Some(copy_ref!(&self.environment));

            self.environment = new_cell_ref!(environment);

            let obj = Object::Class(copy_ref!(&class_ref));
            self.environment.borrow_mut().define("super", obj);
        }

        let mut class_methods = HashMap::default();
        for method in methods {
            if let Stmt::Function(name, _, _) = method.as_ref() {
                let function = LoxFunction::new(copy_ref!(method), copy_ref!(&self.environment), name.lexeme == "init");
                class_methods.insert(name.to_lexeme(), function);
            }
        }
        
        let class = LoxClass::new(&name.lexeme, Some(class_ref), class_methods);

        if has_superclass {
            let abc = copy_ref!(&self.environment);
            let env = abc.borrow();
            if let Some(enclosing) = &env.enclosing {
                self.environment = copy_ref!(enclosing); 
            }
        }

        //self.environment.borrow_mut().assign(name, Object::Class(Box::new(klass)));
        self.globals.borrow_mut().assign(name, Object::Class(Rc::new(class)));
        Ok(())
    }

    // Evaluates an expression.  If an error occurs, returns a runtime error.
    //
    fn visit_expression(&mut self, expr: &Expr) -> Result<(), RuntimeError> {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_function(&mut self, stmt: &Rc<Stmt>, name: &Token, _args: &Vec<Token>, _stmts: &Vec<Rc<Stmt>>) -> Result<(), RuntimeError> {
        let function = LoxFunction::new(copy_ref!(stmt), copy_ref!(&self.environment), false);
        let function = Rc::new(function);

        self.environment.borrow_mut().define(&name.lexeme, Object::Function(function));
        Ok(())
    }

    // Executes an if statement.  If expression evaluates to true, it executes the main statement.  If the expression evaliuates
    // to false, the else statement is executed, if there is one.  If there is an error, returns a runtime error.
    //
    fn visit_if(&mut self, expr: &Expr, if_stmt: &Rc<Stmt>, else_stmt: &Rc<Stmt>) -> Result<(), RuntimeError> {
        let value = self.evaluate(expr)?;
        if value.is_truthy() {
            self.execute(&if_stmt)?;
        }
        else if **else_stmt != Stmt::None {
            self.execute(&else_stmt)?;             
        }
        Ok(())
    }

    // Prints a value to the console.  If there is an error, returns a runtime error.
    //
    fn visit_print(&mut self, expr: &Expr) -> Result<(), RuntimeError> {
        let object = self.evaluate(expr)?;
        println!("{}", object.to_string());  
        Ok(())
    }

    fn visit_return(&mut self, keyword: &Token, expr: &Expr) -> Result<(), RuntimeError> {
        let mut value = Object::None;
        if *expr != Expr::None {
            value = self.evaluate(expr)?;
        }
        return Err(RuntimeError { token: keyword.copy(), message: String::from(""), return_value: value});
    }

    /// Runs a whiles loop as long as the expression evaluates to true.  If an error occurs, returns a runtime error.
    ///
    fn visit_while(&mut self, expr: &Expr, body: &Rc<Stmt>) -> Result<(), RuntimeError> {
        while self.evaluate(expr)?.is_truthy() {
            self.execute(&body)?;  
        }
        Ok(())
    }

    // Defines a variable in the current environment.
    //
    fn visit_var(&mut self, name: &Token, expr: &Expr) -> Result<(), RuntimeError> {
        let value = self.evaluate(expr)?;
                
        self.environment.borrow_mut().define(&name.lexeme, value);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::error::*;
    use crate::token::*;

    use crate::object::Object;
    use crate::expr::Expr;
    use crate::stmt::Stmt;

    use crate::expr::*;

    use crate::lox_function::LoxCallable;

    use crate::scanner::Scanner;
    use crate::parser::Parser;
    use crate::resolver::Resolver;

    use crate::interpreter::Interpreter;
    use crate::interpreter::Clock;

    use crate::visitor::Visitor;

    use crate::GLOBALS;
    
    // Convenience method to parse tokens and return a 
    fn parse(value: &str) -> Result<Stmt, ParseError> {
        let mut scanner = Scanner::new(value.to_owned());      
        scanner.scan_tokens();
    
        let mut parser = Parser::new(scanner.tokens);
        
        parser.statement()
    }

    // Convenience method to assert failure of an expression with a given message.
    //
    fn assert_fail(result: Result<Object, RuntimeError>, message: &str) {
        match result {
            Ok(_) => { 
                panic!("should fail")
            }
            Err(err) => { 
                assert_eq!(message, err.message);
            }
        }
    }

    // Evaluating a literal should return the value.
    //
    #[test]
    fn evaluate_literal() {
        let expr = literal!(Object::Number(3.16));
        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Number(3.16), value);
    }

    // Evaluating a grouping should return the value of the inside expression.
    //
    #[test]
    fn evaluate_grouping() {
        let expr = literal!(Object::Number(3.16));
        let expr = grouping!(expr);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Number(3.16), value);
    }

    // Evaluating an unary number with a minus operator should negate the number.
    //
    #[test]
    fn evaluate_unary_minus() {
        let minus = token!(TokenType::Minus, "-", Object::None, 1);
        let expr = literal!(Object::Number(3.16));
        let expr = unary!(minus, expr);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Number(-3.16), value);
    }

    // Should return a runtime error if expression of minus unary is not a number.
    //
    #[test]
    fn evaluate_unary_minus_not_number() {
        let minus = token!(TokenType::Minus, "-", Object::None, 1);
        let expr = literal!(Object::Boolean(true));
        let expr = unary!(minus, expr);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operand must be number.");
    }

    // Unary expression with bang operator of boolean true should return false.
    //
    #[test]
    fn evaluate_unary_bang_true() {
        let minus = token!(TokenType::Bang, "!", Object::None, 1);
        let expr = literal!(Object::Boolean(true));
        let expr = unary!(minus, expr);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(false), value);
    }

    // Unary expression with bang operator of boolean false should return true.
    //
    #[test]
    fn evaluate_unary_bang_false() {
        let minus = token!(TokenType::Bang, "!", Object::None, 1);
        let expr = literal!(Object::Boolean(false));
        let expr = unary!(minus, expr);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(true), value);
    }

    // Unary expression with bang operand with nil value should return true.  (Nil has truthy value of false)
    //
    #[test]
    fn evaluate_unary_bang_nil() {
        let minus = token!(TokenType::Bang, "!", Object::None, 1);
        let expr = literal!(Object::None);
        let expr = unary!(minus, expr);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(true), value);
    }

    // Unary expression with bang operator with any other value should return false.  (Non-nil has truthy value of true)
    //
    #[test]
    fn evaluate_unary_bang_non_nil() {
        let minus = token!(TokenType::Bang, "!", Object::None, 1);
        let expr = literal!(Object::String("HI".to_owned()));
        let expr = unary!(minus, expr);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(false), value);
    }

    // Minus binary expression should return the left minus right.
    //
    #[test]
    fn evaluate_binary_minus() {
        let minus = token!(TokenType::Minus, "-", Object::None, 1);
        let left = literal!(Object::Number(2.0));
        let right = literal!(Object::Number(1.0));

        let expr = binary!(left, minus, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Number(1.0), value);
    }

    // Minus should return a runtime error if left expression is not a number.
    //
    #[test]
    fn evaluate_binary_minus_left_not_number() {
        let operator = token!(TokenType::Minus, "-", Object::None, 1);
        let left = literal!(Object::Boolean(true));
        let right = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // Minus should return a runtime error if right expression is not a number.
    //
    #[test]
    fn evaluate_binary_minus_right_not_number() {
        let operator = token!(TokenType::Minus, "-", Object::None, 1);
        let right = literal!(Object::Boolean(true));
        let left = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // Slash binary expression should return the left divided by right.
    //   
    #[test]
    fn evaluate_binary_slash() {
        let slash = token!(TokenType::Slash, "/", Object::None, 1);
        let left = literal!(Object::Number(4.0));
        let right = literal!(Object::Number(2.0));

        let expr = binary!(left, slash, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Number(2.0), value);
    }

    // Slash should return a runtime error if left expression is not a number.
    //
    #[test]
    fn evaluate_binary_slash_left_not_number() {
        let operator = token!(TokenType::Slash, "/", Object::None, 1);
        let left = literal!(Object::Boolean(true));
        let right = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // Slash should return a runtime error if right expression is not a number.
    //
    #[test]
    fn evaluate_binary_slash_right_not_number() {
        let operator = token!(TokenType::Slash, "/", Object::None, 1);
        let right = literal!(Object::Boolean(true));
        let left = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // Star binary expression should return the left multiplied by the right.
    //  
    #[test]
    fn evaluate_binary_star() {
        let star = token!(TokenType::Star, "*", Object::None, 1);
        let left = literal!(Object::Number(2.0));
        let right = literal!(Object::Number(2.0));

        let expr = binary!(left, star, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Number(4.0), value);
    }

    // Star should return a runtime error if left expression is not a number.
    //
    #[test]
    fn evaluate_binary_star_left_not_number() {
        let operator = token!(TokenType::Star, "*", Object::None, 1);
        let left = literal!(Object::Boolean(true));
        let right = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // Star should return a runtime error if right expression is not a number.
    //
    #[test]
    fn evaluate_binary_star_right_not_number() {
        let operator = token!(TokenType::Star, "*", Object::None, 1);
        let right = literal!(Object::Boolean(true));
        let left = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // Plus should add two numbers.
    //
    #[test]
    fn evaluate_binary_plus_double() {
        let plus = token!(TokenType::Plus, "+", Object::None, 1);
        let left = literal!(Object::Number(3.0));
        let right = literal!(Object::Number(3.0));

        let expr = binary!(left, plus, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Number(6.0), value);
    }

    // Plus should concatenate two strings.
    //
    #[test]
    fn evaluate_binary_plus_string() {
        let star = token!(TokenType::Plus, "+", Object::None, 1);
        let left = literal!(Object::String("ABC".to_string()));
        let right = literal!(Object::String("DEF".to_string()));

        let expr = binary!(left, star, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::String("ABCDEF".to_string()), value);
    }

    // Plus only supports two numbers and two strings.  Any other combination should return a runtime error.
    //
    #[test]
    fn evaluate_binary_plus_mixed() {
        let operator = token!(TokenType::Plus, "+", Object::None, 1);
        let right = literal!(Object::Boolean(true));
        let left = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be two numbers or two strings.");
    }

    // Test that Greater returns true if left is greater than right.
    //
    #[test]
    fn evaluate_binary_greater() {
        let greater = token!(TokenType::Greater, ">", Object::None, 1);
        let left = literal!(Object::Number(4.0));
        let right = literal!(Object::Number(3.0));
        let expr = binary!(left, greater, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(true), value);
    }

    // For Greater, if left is not a number a runtime error should be returned.
    //
    #[test]
    fn evaluate_binary_greater_left_not_number() {
        let greater = token!(TokenType::Greater, ">", Object::None, 1);
        let left = literal!(Object::Boolean(true));
        let right = literal!(Object::Number(3.0));
        let expr = binary!(left, greater, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // For Greater, if right is not a number a runtime error should be returned.
    //
    #[test]
    fn evaluate_binary_greater_right_not_number() {
        let greater = token!(TokenType::Greater, ">", Object::None, 1);
        let right = literal!(Object::Boolean(true));
        let left = literal!(Object::Number(3.0));
        let expr = binary!(left, greater, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }
   
    // Test that GreaterEqual returns true if left is greater than or equal to right.
    //
    #[test]
    fn evaluate_binary_greater_equal() {
        let greater_equal = token!(TokenType::GreaterEqual, ">=", Object::None, 1);
        let left = literal!(Object::Number(3.0));
        let right = literal!(Object::Number(3.0));

        let expr = binary!(left, greater_equal, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(true), value);
    }

    // For GreaterThan, if left is not a number, a runtime error should be returned.
    //
    #[test]
    fn evaluate_binary_greater_equal_left_not_number() {
        let operator = token!(TokenType::GreaterEqual, ">=", Object::None, 1);
        let left = literal!(Object::Boolean(true));
        let right = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // For GreaterThan, if right is not a number, a runtime error should be returned.
    //
    #[test]
    fn evaluate_binary_greater_equal_right_not_number() {
        let operator = token!(TokenType::GreaterEqual, ">=", Object::None, 1);
        let right = literal!(Object::Boolean(true));
        let left = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // Tests that Less returns true if left is less than right.
    //
    #[test]
    fn evaluate_binary_less() {
        let less = token!(TokenType::Less, "<", Object::None, 1);
        let left = literal!(Object::Number(3.0));
        let right = literal!(Object::Number(4.0));

        let expr = binary!(left, less, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(true), value);
    }

    // For Less, if left is not a number, a runtime error should be returned.
    //
    #[test]
    fn evaluate_binary_less_left_not_number() {
        let operator = token!(TokenType::Less, "<", Object::None, 1);
        let left = literal!(Object::Boolean(true));
        let right = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // For Less, if right is not a number, a runtime error should be returned.
    //
    #[test]
    fn evaluate_binary_less_right_not_number() {
        let operator = token!(TokenType::Less, "<", Object::None, 1);
        let right = literal!(Object::Boolean(true));
        let left = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // Tests that LessEqual returns true if left is less than or equal to right.
    //
    #[test]
    fn evaluate_binary_less_equal() {
        let less = token!(TokenType::LessEqual, "<=", Object::None, 1);
        let left = literal!(Object::Number(3.0));
        let right = literal!(Object::Number(3.0));

        let expr = binary!(left, less, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(true), value);
    }

    // For LessEqual, if less is not a number, a runtime error should be returned.
    //
    #[test]
    fn evaluate_binary_less_equal_left_not_number() {
        let operator = token!(TokenType::LessEqual, "<=", Object::None, 1);
        let left = literal!(Object::Boolean(true));
        let right = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // For LessEqual, if less is not a number, a runtime error should be returned.
    //
    #[test]
    fn evaluate_binary_less_eqaul_right_not_number() {
        let operator = token!(TokenType::LessEqual, "<=", Object::None, 1);
        let right = literal!(Object::Boolean(true));
        let left = literal!(Object::Number(3.0));
        let expr = binary!(left, operator, right);

        let mut interpreter = Interpreter::new();
        let value = interpreter.evaluate(&expr);

        assert_fail(value, "Operands must be numbers.");
    }

    // For BangEqual, the values None and None should return false.
    //
    #[test]
    fn evaluate_binary_bang_equal_none() {
        let bang_equal = token!(TokenType::BangEqual, "!=", Object::None, 1);
        let left = literal!(Object::None);
        let right = literal!(Object::None);

        let expr = binary!(left, bang_equal, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(false), value);
    }

    // For BangEqual the values None and some value should return true.
    //
    #[test]
    fn evaluate_binary_bang_equal_none_some() {
        let bang_equal = token!(TokenType::BangEqual, "!=", Object::None, 1);
        let left = literal!(Object::None);
        let right = literal!(Object::Number(3.0));

        let expr = binary!(left, bang_equal, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(true), value);
    }

    // For BangEqual unequal values should return true.
    //
    #[test]
    fn evaluate_binary_bang_equal_some_other() {
        let bang_equal = token!(TokenType::BangEqual, "!=", Object::None, 1);
        let left = literal!(Object::Boolean(true));
        let right = literal!(Object::Number(3.0));

        let expr = binary!(left, bang_equal, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(true), value);
    }

    // For EqualEqual equal values should return true.
    //
    #[test]
    fn evaluate_binary_equal_equal() {
        let equal_equal = token!(TokenType::EqualEqual, "==", Object::None, 1);
        let left = literal!(Object::Number(3.0));
        let right = literal!(Object::Number(3.0));

        let expr = binary!(left, equal_equal, right);

        let mut interpreter = Interpreter::new();

        let value = interpreter.evaluate(&expr).expect("REASON");

        assert_eq!(Object::Boolean(true), value);
    }

    // Print statements should display the expression to the console.  For now there are no side effects to test. 
    #[test]
    fn execute_print_statement() {
        let stmt = parse("print 123;").expect("SHOULD PARSE");

        let mut interpreter = Interpreter::new();

        interpreter.interpret(&vec![stmt.into()]);

        // TODO: Modify console output to be struct which can be asserted.  (And swapped for that matter).
    }
   
    // Not sure what to say about this one... assertions are tested in execute var statement.
    //
    #[test]
    fn execute_expression_statemnt() {
        let stmt = parse("a = 1;").expect("SHOULD PARSE");

        let mut interpreter = Interpreter::new();

        interpreter.interpret(&vec![stmt.into()]);

        // no side effects to assert atm.
    }

    // A variable declaration should define a variable in environment and allow a value to be assigned to it.
    //
    #[test]
    fn execute_var_statement() {
        let mut scanner = Scanner::new("var test = 1; test = 2;".to_owned());      
        scanner.scan_tokens();
    
        let mut parser = Parser::new(scanner.tokens);
        
        let stmts = parser.parse();

        let mut interpreter = Interpreter::new();

        interpreter.interpret(&stmts);

        let name = token!(TokenType::Identifier, "test", Object::None, 1);
        
        let value = interpreter.globals.borrow().get(&name).expect("Should be found!");

        assert_eq!(Object::Number(2.0), value);
    }

    // Verifies that a block can be executed.
    //
    #[test]
    fn execute_block_statement() {
        let mut scanner = Scanner::new("var test = 1; {test = 5;}".to_owned());      
        scanner.scan_tokens();
    
        let mut parser = Parser::new(scanner.tokens);
        
        let stmts = parser.parse();

        let mut interpreter = Interpreter::new();

        interpreter.interpret(&stmts);

        let name = token!(TokenType::Identifier, "test", Object::None, 1);
        
        let value = interpreter.globals.borrow().get(&name).expect("Should be found!");

        assert_eq!(Object::Number(5.0), value);
    }

    // Tests that if statement is executed when the expression evaluates to true.
    //
    #[test]
    fn execute_if_statement() {
        let code = "          
            var test = true;  
            var a = 0; 
            if (test) {
               a = 5;
            }
            ";

        let mut scanner = Scanner::new(code.to_owned());      
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();

        interpreter.interpret(&stmts);

        let name = token!(TokenType::Identifier, "a", Object::None, 1);
        let value = interpreter.globals.borrow().get(&name).expect("Should be found!");

        assert_eq!(Object::Number(5.0), value);
    }

    // If an else clause exists, it should be executed when the expression evaluates to false.
    //
    #[test]
    fn execute_if_statement_else() {
        let code = "          
            var test = false;  
            var a = 0;

            if (test) {
                a = 5;
            }
            else {
                a = 6;
            }
            ";

        let mut scanner = Scanner::new(code.to_owned());      
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();

        interpreter.interpret(&stmts);

        let name = token!(TokenType::Identifier, "a", Object::None, 1);
        let value = interpreter.globals.borrow().get(&name).expect("Should be found!");

        assert_eq!(Object::Number(6.0), value);
    }

    // Tests executing the logical or operator.
    //
    #[test]
    fn execute_logical_or() {
        let mut scanner = Scanner::new("var test = true or false;".to_owned());          
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();

        interpreter.interpret(&stmts);

        let name = token!(TokenType::Identifier, "test", Object::None, 1);
        let value = interpreter.globals.borrow().get(&name).expect("Should be found!");

        assert_eq!(Object::Boolean(true), value);
    }

    // Tests executing the logical or operator.
    //
    #[test]
    fn execute_logical_and() {
        let mut scanner = Scanner::new("var test = true and false;".to_owned());          
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();

        interpreter.interpret(&stmts);

        let name = token!(TokenType::Identifier, "test", Object::None, 1);
        let value = interpreter.globals.borrow().get(&name).expect("Should be found!");

        assert_eq!(Object::Boolean(false), value);
    }

    // When evaluating logical operators values nil is treated as "truthy" false, and all other values as true.
    //
    #[test]
    fn execute_logical_truthy() {
        let mut scanner = Scanner::new("var test = \"hi\" or 0 or false and nil;".to_owned());          
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();

        interpreter.interpret(&stmts);

        let name = token!(TokenType::Identifier, "test", Object::None, 1);
        let value = interpreter.globals.borrow().get(&name).expect("Should be found!");

        assert_eq!(Object::String("hi".to_owned()), value);
    }

    // Tests executing a while loop.
    //
    #[test]
    fn execute_while_loop() {
        let code = "
            var a = 0;
            var b = true;
            
            while (b) {
                b = false;
                a = 42;
            }
            ";

        let mut scanner = Scanner::new(code.to_owned());      
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();

        interpreter.interpret(&stmts);

        let name = token!(TokenType::Identifier, "a", Object::None, 1);
        let value = interpreter.globals.borrow().get(&name).expect("Should be found!");

        assert_eq!(Object::Number(42.0), value);
    }

    // Tests executing a for loop.  For loops are not directly supported in the Interpreter, but are syntactic sugar
    // in the parser creating a while loop to be run.  
    //
    #[test]
    fn execute_for_statment() {
        let code = "
            var a = 0;
            var temp;
            
            for (var b = 1; a < 5; b = temp + b) {
                temp = a;
                a = b;
            }
            ";

        let mut scanner = Scanner::new(code.to_owned());      
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();
        
        let mut resolver = Resolver::new();
        resolver.resolve(&stmts);
        interpreter.locals.clear();
        interpreter.locals.extend(resolver.locals.into_iter());

        interpreter.interpret(&stmts);

        let name = token!(TokenType::Identifier, "a", Object::None, 1);
        let value = interpreter.globals.borrow().get(&name).expect("Should be found!");

        assert_eq!(Object::Number(5.0), value);
    }

    // Test calling clock native function.
    //
    #[test]
    fn native_function_clock() {
        let mut interpreter = Interpreter::new();
        let clock = Clock {};
        
        let number = clock.call(&mut interpreter, vec![], None);

        assert!(number.to_number() > 0.0);
    }

    // Test clock's to_string().
    //
    #[test]
    fn native_function_clock_to_string() {
        let clock = Clock {};

        assert_eq!("<fn native clock>", clock.to_string());
    }

    // Try to call a non-function should should report runtime error to Lox.
    //
    #[test]
    fn interpret_call_non_function() {
        let code = "\"totally not a function\"();";

        let mut scanner = Scanner::new(code.to_owned());      
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();

        interpreter.interpret(&stmts);

        let globals = GLOBALS.lock().unwrap();

        assert_eq!("Can only call functions and classes.", globals.last_error);
    }

    // Calling a function with the wrong number of arguments should report runtime error to Lox.
    //
    #[test]
    fn interpret_wrong_number_of_arguments() {
        let code = "
            fun fib(n) {
               if (n < 2) return n;
        
               return fib(n - 1) + fib(n - 2);
            }

            var test = fib(1, 1);
            print(fib(1));
        ";

        let mut scanner = Scanner::new(code.to_owned());      
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();
        
        let mut resolver = Resolver::new();
        resolver.resolve(&stmts);
        interpreter.locals.clear();
        interpreter.locals.extend(resolver.locals.into_iter());

        interpreter.interpret(&stmts);

        let globals = GLOBALS.lock().unwrap();

        assert_eq!("Expected 1 arguments but got 2.", globals.last_error);
    }

    // Tests calling a recursive function.
    //
    #[test]
    fn interpret_function() {
        let code = "
            fun fib(n) {
               if (n < 2) return n;
        
               return fib(n - 1) + fib(n - 2);
            }
            var test = fib(7);
        ";

        let mut scanner = Scanner::new(code.to_owned());      
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();
        
        let mut resolver = Resolver::new();
        resolver.resolve(&stmts);
        interpreter.locals.clear();
        interpreter.locals.extend(resolver.locals.into_iter());

        interpreter.interpret(&stmts);

        let name = token!(TokenType::Identifier, "test", Object::None, 1);
        let value = interpreter.globals.borrow().get(&name).expect("Should be found!");

        assert_eq!(Object::Number(13.0), value);
    }

    // Tests local functions!!!
    //
    #[test]
    fn interpret_local_function() {
        let code = "
            fun makeCounter() {
                var i = 0;
                fun count() {
                    i = i + 1;
                    print i;
                }
                return count;
            }
            
            var counter = makeCounter();
            counter();
            counter();
        ";

        interpret(code);
    }

    fn interpret(code: &str) {
        let mut scanner = Scanner::new(code.to_owned());      
        scanner.scan_tokens();
        let mut parser = Parser::new(scanner.tokens);
        let stmts = parser.parse();
        let mut interpreter = Interpreter::new();
        
        let mut resolver = Resolver::new();
        resolver.resolve(&stmts);
        interpreter.locals.clear();
        interpreter.locals.extend(resolver.locals.into_iter());

        interpreter.interpret(&stmts);
    }

    // Tests invalid getter
    //
    #[test]
    fn interpret_invalid_getter() {
        let code = "
            var test = false;

            print test.len;
        ";

        interpret(code);

        // let globals = GLOBALS.lock().unwrap();
        // assert_eq!(true, globals.had_runtime_error);
        // assert_eq!("Only instances have properties.", globals.last_error);
    }

    // Tests undefined getter
    //
    #[test]
    fn interpret_undefined_getter() {
        let code = "
            class Bagel {}
            var bagel = Bagel();

            print bagel.flavor;
        ";

        interpret(code);

        // let globals = GLOBALS.lock().unwrap();
        // assert_eq!(true, globals.had_runtime_error);
        // assert_eq!("Undefined property 'flavor'.", globals.last_error);
    }

    // Tests setters and getters!!
    //
    #[test]
    fn interpret_setter_and_getter() {
        GLOBALS.lock().unwrap().had_runtime_error = false;

        let code = "
            class Bagel {}
            var bagel = Bagel();
            bagel.flavor = \"Yummy\";

            print bagel.flavor;
        ";
        interpret(code);        
    }

    // Tests trying to inherit from a non-class.
    //
    #[test]
    fn interpret_class_inherit_not_a_class() {
        GLOBALS.lock().unwrap().had_runtime_error = false;

        let code = "
            var NotAClass = \"Totally not a class!!!\";

            class Subclass < NotAClass {}
        ";
        interpret(code);    
        
        let globals = GLOBALS.lock().unwrap();
        assert_eq!(true, globals.had_runtime_error);
        assert_eq!("Superclass must be a class.", globals.last_error);
    }
}