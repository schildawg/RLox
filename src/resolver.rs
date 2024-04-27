use std::collections::HashMap;
use std::rc::Rc;

use ahash::{AHasher, RandomState};

use crate::lox::Lox;

use crate::error::{runtime_error, RuntimeError};

use crate::object::Object;
use crate::token::{Token};

use crate::expr::Expr;
use crate::stmt::Stmt;

use crate::visitor::Visitor;

#[derive(Debug, Clone, PartialEq)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Debug, Clone, PartialEq)]
enum ClassType {
    None,
    Class,
    Subclass,
}

/// Resolver.  Semantic analysis pass to resolve and bind variables for use in the Interpreter.
///
pub struct Resolver {
    pub locals: HashMap<String, usize, RandomState>,
    scopes: Vec<HashMap<String, bool, RandomState>>,
    
    current_function: FunctionType,
    current_class: ClassType,

    debug: bool,
}

impl Resolver {
    /// Factory method to create a new Resolver.
    ///
    pub fn new() -> Resolver {
        Resolver { scopes: vec![], debug: false, locals: HashMap::default(), current_function: FunctionType::None, current_class: ClassType::None}
    }

    /// Resolves a list of statements.
    ///
    pub fn resolve(&mut self, stmts: &Vec<Rc<Stmt>>) -> Result<(), RuntimeError> {
        for stmt in stmts {
            let _ = self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    // Resolves a statement.
    //
    fn resolve_stmt(&mut self, stmt: &Rc<Stmt>) -> Result<(), RuntimeError> {
        let _ = self.visit_stmt(stmt)?;
        Ok(())
    }

    // A function declaration introduces a new scope for its body, and binds its parameters to that scope.
    //
    fn resolve_function(&mut self, stmt: &Stmt, function_type: FunctionType) -> Result<(), RuntimeError> {
        if let Stmt::Function(_name, params, body) = stmt {
            let enclosing_function = self.current_function.clone();
            self.current_function = function_type;

            self.begin_scope();

            for param in params {
               self.declare(param);
               self.define(param); 
            }

            let _ = self.resolve(body);
            self.end_scope();
            
            self.current_function = enclosing_function;

            return Ok(())
        }
        panic!("invalid statement");
    }

    // Resolves an expression.
    //
    fn resolve_expr(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
        let _ = self.visit_expr(expr)?;
        Ok(Object::None)
    }

    fn resolve_local(&mut self, id: &str, token: &Token) -> Result<Object, RuntimeError> {    
        let last = self.scopes.len();

        for scope_number in (1..=last).rev() {
            if let Some(map) = self.scopes.get(scope_number - 1) {
                if map.contains_key(&token.lexeme) {
                    self.locals.insert(id.to_string(), last - scope_number);
                }
            }
        }
        Ok(Object::None)
    }
    
    // Begins a new scope.
    //
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    // Ends the current scope.
    //
    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    // Declares a variable.
    //
    fn declare(&mut self, name: &Token) {
        if self.scopes.is_empty() { return; };

        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&name.lexeme) {
            Lox::token_error(name, "Already a variable with this name in this scope.");
        }

        scope.insert(name.to_lexeme(), false);
    }

    // Defines a variable.
    //
    fn define(&mut self, token: &Token) {
        if self.scopes.is_empty() { return; };

        let scope = self.scopes.last_mut().unwrap();
        scope.insert(token.to_lexeme(), true);
    }

    // Prints if debug is enabled.
    //
    fn print(&self, value: &str) {
        if self.debug == true {
            println!("{}", value);
        }
    }
}

impl Visitor for Resolver {
    // Assignment expressions need to have their variables resolved.
    //
    fn visit_assign(&mut self, id: &str, name: &Token, value: &Expr) -> Result<Object, RuntimeError> {
       self.print("visit_assign()");
       
       let _ = self.resolve_expr(value)?;
       let _ = self.resolve_local(id, name)?;

       Ok(Object::None)
    }

    // Traverses tree.
    //
    fn visit_binary(&mut self, left: &Expr, _operator: &Token, right: &Expr) -> Result<Object, RuntimeError> {
        self.print("visit_binary()");  

        let _ = self.resolve_expr(left)?;
        let _ = self.resolve_expr(right)?;

        Ok(Object::None)
    }

    // Traverses tree.
    //
    fn visit_call(&mut self, callee: &Expr, _token: &Token, args: &Vec<Expr>) -> Result<Object, RuntimeError> {
        self.print("visit_call()");  

        let _ = self.resolve_expr(callee)?;

        for arg in args {
            let _ = self.resolve_expr(arg)?;
        }

        Ok(Object::None)
    }

    fn visit_get(&mut self, object: &Expr, _token: &Token) -> Result<Object, RuntimeError> {
        self.print("visit_get()");  
   
        let _ = self.resolve_expr(object)?;

        Ok(Object::None)
    }

    // Traverses tree.
    //
    fn visit_grouping(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
        self.print("visit_grouping()");  

        let _ = self.resolve_expr(expr)?;

        Ok(Object::None)
    }

    // Traverses tree.
    //
    fn visit_logical(&mut self, left: &Expr, _operator: &Token, right: &Expr) -> Result<Object, RuntimeError> {
        self.print("visit_logical()");  

        let _ = self.resolve_expr(left)?;
        let _ = self.resolve_expr(right)?;

        Ok(Object::None)
    }

    // Traverses tree.
    fn visit_set(&mut self, object: &Expr, _token: &Token, value: &Expr) -> Result<Object, RuntimeError> {
        self.print("visit_set()");  

        let _ = self.resolve_expr(object)?;
        let _ = self.resolve_expr(value)?;

        Ok(Object::None)
    }  

    fn visit_super(&mut self, keyword: &Token, _name: &Token) -> Result<Object, RuntimeError> {
        self.print("visit_super()");  

        if self.current_class == ClassType::None {
            Lox::token_error(keyword, "Can't use 'super' outside a class.")
        }
        else if self.current_class != ClassType::Subclass {
            Lox::token_error(keyword, "Can't use 'super' in a class with no superclass.")
        }

        // TODO: ADD ID
        self.resolve_local("$$", keyword);
        Ok(Object::None)
    }
    
    fn visit_this(&mut self, keyword: &Token) -> Result<Object, RuntimeError> {
        self.print("visit_this()");
        
        if let ClassType::None = self.current_class {
            Lox::token_error(keyword, "Can't use 'this' outside of a class.");
            // TODO: ^^^^^
            return Err(runtime_error!(keyword, "Can't use 'this' outside of a class."));
        }

        // TODO: ADD ID
        self.resolve_local("??", keyword);
        Ok(Object::None)
    }

    // Traverses tree.
    //
    fn visit_unary(&mut self, _operator: &Token, right: &Expr) -> Result<Object, RuntimeError> {
        self.print("visit_unary()");  

        let _ = self.resolve_expr(right)?;
        Ok(Object::None)
    }


    // Variable expressions need to have their variables resolved.
    //
    fn visit_variable(&mut self, id: &str, name: &Token) -> Result<Object, RuntimeError> {   
        self.print("visit_variable()");  

        if !self.scopes.is_empty() {
            let map = self.scopes.last().unwrap();
            if let Some(value) = map.get(&name.lexeme) {
                if *value == false {
                    Lox::token_error(name, "Can't read local variable in its own initializer.");
                }
            }
        }
        let _ = self.resolve_local(id, name)?;
        Ok(Object::None)
    }

    // A block statement introduces a new scope for the statements it contains.
    //
    fn visit_block(&mut self, stmts: &Vec<Rc<Stmt>>) -> Result<(), RuntimeError> {
        self.print("visit_block()");  

        self.begin_scope();
        let _ = self.resolve(stmts)?;
        self.end_scope();

        Ok(())
    }

    fn visit_class(&mut self, name: &Token, superclass: &Expr, methods: &Vec<Rc<Stmt>>) -> Result<(), RuntimeError> {
        self.print("visit_class()"); 

        let enclosing_class = self.current_class.clone();
        self.current_class = ClassType::Class;

        self.declare(name);
        self.define(name);
        if let Expr::Variable(_, super_name) = superclass {
            if name.lexeme == super_name.lexeme {
                Lox::token_error(super_name, "A class can't inherit from itself.");
            }

            self.current_class = ClassType::Subclass;
            self.resolve_expr(superclass);

            self.begin_scope();
            
            let scope = self.scopes.last_mut().unwrap();
            scope.insert("super".to_string(), true);          
        }

        self.begin_scope();
        let scope = self.scopes.last_mut().unwrap();
        scope.insert("this".to_string(), true);

        for method in methods {
            if let Stmt::Function(name, _, _) = method.as_ref() {
                let declaration = if name.lexeme == "init" {FunctionType::Initializer} else {FunctionType::Method};

                self.resolve_function(method, declaration);
            }
            else {
                panic!("shouldn't happen!");
            }

        }
        self.end_scope();
        if let Expr::Variable(_, _) = superclass {
            self.end_scope();        
        }

        self.current_class = enclosing_class;

        Ok(())
    }

    // Expression statements simply traverses the tree.
    //
    fn visit_expression(&mut self, expr: &Expr) -> Result<(), RuntimeError> {
        self.print("visit_expression()");  

        let _ = self.resolve_expr(expr)?;

        Ok(())
    }

    // A function declaration introduces a new scope for its body, and binds its parameters to that scope.  Binds its name early
    // to enable recursion.
    //   
    fn visit_function(&mut self, stmt: &Rc<Stmt>, name: &Token, _args: &Vec<Token>, _stmts: &Vec<Rc<Stmt>>) -> Result<(), RuntimeError> {
        self.print("visit_function()");  

        self.declare(name);
        self.define(name);

        let _ = self.resolve_function(stmt, FunctionType::Function)?;
        Ok(())
    }

    // Traverses tree.
    //
    fn visit_if(&mut self, condition: &Expr, then_branch: &Rc<Stmt>, else_branch: &Rc<Stmt>) -> Result<(), RuntimeError> {
        self.print("visit_if()");  

        let _ = self.resolve_expr(condition)?;
        let _ = self.resolve_stmt(then_branch)?;
        let _ = self.resolve_stmt(else_branch)?;

        Ok(())
    }

    // Traverses tree.
    //
    fn visit_print(&mut self, expr: &Expr) -> Result<(), RuntimeError> { 
        self.print("visit_print()");  

        let _ = self.resolve_expr(expr)?;
        Ok(())
    }

    // Traverses tree.
    //
    fn visit_return(&mut self, keyword: &Token, expr: &Expr) -> Result<(), RuntimeError> {
        self.print("visit_return()");  

        if self.current_function == FunctionType::None {
            Lox::token_error(keyword, "Can't return from top-level code.");
        }

        if *expr != Expr::None && self.current_function == FunctionType::Initializer {
            Lox::token_error(keyword, "Can't return a value from an initializer.");
        }

        self.resolve_expr(expr)?;
        Ok(())
    }

    // Traverses tree.
    //
    fn visit_while(&mut self, condition: &Expr, body: &Rc<Stmt>) -> Result<(), RuntimeError> {
        self.print("visit_while()");  

        let _ = self.resolve_expr(condition)?;
        let _ = self.resolve_stmt(body)?;
        Ok(())
    }

    // A variable declaration adds a new variable to the current scope.
    //
    fn visit_var(&mut self, name: &Token, initializer: &Expr) -> Result<(), RuntimeError> {
        self.print("visit_var()");  

        self.declare(name);
        let _ = self.resolve_expr(initializer)?;
        self.define(name);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::resolver::Resolver;
    
    use crate::visitor::Visitor;

    use crate::token::Token;
    use crate::scanner::Scanner;
    use crate::parser::Parser;

    use crate::GLOBALS;

    // Convenience method to scan tokens from a string, to make code more readable than building by hand.
    //
    fn scan(value: &str) -> Vec<Token> {
        let mut scanner = Scanner::new(value.to_owned());      
        scanner.scan_tokens();

        scanner.tokens
    }

    // Resolver should have zero hops if defined and used in same scope.
    //
    #[test]
    fn resolve_same_level() {
        let mut parser = Parser::new(scan("var test = true; print test;"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(0, *resolver.locals.get("1").unwrap());
    }

    // Resolver should have zero hops if defined and used in same scope.  Trying it two levels deep.
    //
    #[test]
    fn resolve_same_level_two_deep() {
        let mut parser = Parser::new(scan("{var test = true; print test;}"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(0, *resolver.locals.get("1").unwrap());
    }

    // Resolver should have zero hops if defined and used in same scope.  Trying it three levels deep.
    //
    #[test]
    fn resolve_same_level_three_deep() {
        let mut parser = Parser::new(scan("{var test = true; print test;}"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(0, *resolver.locals.get("1").unwrap());
    }

    // Usage of variable in nested scope should have one hop.
    //
    #[test]
    fn resolve_one_hop() {
        let mut parser = Parser::new(scan("var test = true; {print test;}"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(1, *resolver.locals.get("1").unwrap());
    }

    // Testing two hops.
    //
    #[test]
    fn resolve_two_hop() {
        let mut parser = Parser::new(scan("var test = true; {{print test;}}"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(2, *resolver.locals.get("1").unwrap());
    }

    // Testing assignment resolves locals.
    //
    #[test]
    fn resolve_assignment() {
        let mut parser = Parser::new(scan("var test = true; {test = false;}"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(1, *resolver.locals.get("2").unwrap());
    }

    // Testing logical resolves locals.
    //
    #[test]
    fn resolve_logical() {
        let mut parser = Parser::new(scan("var test = true; {test = false or true;}"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(1, *resolver.locals.get("2").unwrap());
    }

    // Testing unary resolves locals.
    //
    #[test]
    fn resolve_unary() {
        let mut parser = Parser::new(scan("var test = true; var test2 = !test;"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(0, *resolver.locals.get("1").unwrap());
    }

    // Testing binary resolves locals.
    //
    #[test]
    fn resolve_binary() {
        let mut parser = Parser::new(scan("var test = true; {test = 1 > 2;}"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(1, *resolver.locals.get("2").unwrap());
    }

    // Testing grouping resolves locals.
    //
    #[test]
    fn resolve_grouping() {
        let mut parser = Parser::new(scan("var test = true; var test2 = (test);"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(0, *resolver.locals.get("1").unwrap());
    }

    // Tests resolving function and call.
    //
    #[test]
    fn resolve_function_and_call() {
        let mut parser = Parser::new(scan("
            fun abc(a, b, c) {
                test = 1 > 2;
            }
            abc(1, 2, 3);
            "));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(0, *resolver.locals.get("3").unwrap());
    }

    // Tests resolving if statements
    //
    #[test]
    fn resolve_if() {
        let mut parser = Parser::new(scan("
            var test = 1;
            if (test == 1) {
                test = 2;
            }
            else {
                test = 3;
            }
            "));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(0, *resolver.locals.get("1").unwrap());
        assert_eq!(1, *resolver.locals.get("3").unwrap());
        assert_eq!(1, *resolver.locals.get("5").unwrap());
    }

    // Tests resolving while statements
    //
    #[test]
    fn resolve_while() {
        let mut parser = Parser::new(scan("
            var test = 1;

            while (test < 10) {
                test = test + 1;
            }
            "));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        assert_eq!(0, *resolver.locals.get("1").unwrap());
        assert_eq!(1, *resolver.locals.get("3").unwrap());
        assert_eq!(1, *resolver.locals.get("4").unwrap());
    }

    // The resolver should report an error to Lox if a local variable is used in its own initializer.
    //
    #[test]
    fn resolver_local_variable_own_initializer() {
        let mut parser = Parser::new(scan("{{ var test = test; }}"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        let globals = GLOBALS.lock().unwrap();
        assert_eq!(true, globals.had_error);
        assert_eq!("[line 1] Error at 'test': Can't read local variable in its own initializer.", globals.last_error);
    }

    // The resolver should report an error to Lox if a local variable is declared twice in same scope.
    //
    #[test]
    fn resolver_duplicate_variable() {
        let mut parser = Parser::new(scan("
            fun bad() {
                var a = \"first\";
                var a = \"second\";
            } 
            "));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        let globals = GLOBALS.lock().unwrap();
        assert_eq!(true, globals.had_error);
        assert_eq!("[line 4] Error at 'a': Already a variable with this name in this scope.", globals.last_error);
    }    

    // The resolver should report an error to Lox if a return value is not in a function block.
    //
    #[test]
    fn resolver_invalid_return() {
        let mut parser = Parser::new(scan("return \"not at top level\";"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        let globals = GLOBALS.lock().unwrap();
        assert_eq!(true, globals.had_error);
        assert_eq!("[line 1] Error at 'return': Can't return from top-level code.", globals.last_error);
    }
    
    // The resolver should report an error to Lox if a class tries to inherit from itself.
    //
    #[test]
    fn resolver_class_inherit_from_self() {
        let mut parser = Parser::new(scan("class Pie < Pie {}"));
        
        let mut resolver = Resolver::new();

        let _ = resolver.visit_block(&parser.parse());

        let globals = GLOBALS.lock().unwrap();
        assert_eq!(true, globals.had_error);
        assert_eq!("[line 1] Error at 'Pie': A class can't inherit from itself.", globals.last_error);
    }
}