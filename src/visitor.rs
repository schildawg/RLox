use std::rc::Rc;

use crate::error::RuntimeError;

use crate::object::Object;
use crate::token::Token;

use crate::expr::Expr;
use crate::stmt::Stmt;

/// Visitor.
///
/// Translates Rust's match structure into a more standard Visitor style for code clarity.  Not the Rust way,
/// but, hey, this is my code :)
///
pub trait Visitor {
        /// Matches expression and dispatches to appropriate method.
    ///
    fn visit_expr(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
                match expr {
            Expr::Assign(id, token, expr) => self.visit_assign(id, &token, expr),
            Expr::Binary(left, operator, right) => self.visit_binary(left, &operator, right), 
            Expr::Call(expr, token, args) => self.visit_call(expr, &token, &args), 
            Expr::Get(expr, token) => self.visit_get(expr, &token),
            Expr::Grouping(value) => self.visit_grouping(value), 
            Expr::Literal(object) => self.visit_literal(&object), 
            Expr::Logical(left, operator, right) => self.visit_logical(left, &operator, right),
            Expr::Set(expr, token, exprs) => self.visit_set(expr, &token, &exprs),
            Expr::Super(token, token2) => self.visit_super(&token, &token2),
            Expr::This(token) => self.visit_this(&token), 
            Expr::Unary(operator, value) => self.visit_unary(&operator, value), 
            Expr::Variable(id, token) => self.visit_variable(id, &token), 
            Expr::None => Ok(Object::None),
        }
    }

    /// Matches statement and dispatches to appropriate method.
    ///
    fn visit_stmt(&mut self, stmt: &Rc<Stmt>) -> Result<(), RuntimeError> {
        match stmt.as_ref() {
            Stmt::Block(stmts) => self.visit_block(stmts),
            Stmt::Class(token, superclass, stmts) => self.visit_class(&token, superclass, stmts),
            Stmt::Expression(expr) => self.visit_expression(expr),
            Stmt::Function(name, params, body) => self.visit_function(stmt, &name, &params, body),
            Stmt::If(expr, if_stmt, else_stmt) => self.visit_if(expr, if_stmt, else_stmt),
            Stmt::Print(expr) => self.visit_print(expr),
            Stmt::Return(keyword, expr) => self.visit_return(&keyword, expr),
            Stmt::While(expr, body) => self.visit_while(expr, body),
            Stmt::Var(name, expr) => self.visit_var(&name, expr), 
            Stmt::None => Ok(()),      
        }
    }

    /// Handles Assign expression.
    //
    fn visit_assign(&mut self, _id: &str, _token: &Token, _expr: &Expr) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }

    /// Handles Binary expression.
    ///
    fn visit_binary(&mut self, _left: &Expr, _token: &Token, _right: &Expr) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }

    /// Handles Call expression.
    ///
    fn visit_call(&mut self, _expr: &Expr, _token: &Token, _args: &Vec<Expr>) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }

    /// Handles Get expression.
    ///
    fn visit_get(&mut self, _expr: &Expr, _token: &Token) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }

    /// Handles Grouping expression.
    ///
    fn visit_grouping(&mut self, _expr: &Expr) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }

    /// Handles Literal expression.
    ///
    fn visit_literal(&mut self, _object: &Object) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }

    /// Handles Logical expression.
    fn visit_logical(&mut self, _left: &Expr, _token: &Token, _expr: &Expr) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }

    /// Handles Set expression.
    ///
    fn visit_set(&mut self, _expr: &Expr, _token: &Token, _exprs: &Expr) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }  

    /// Handles Super expression.
    ///
    fn visit_super(&mut self, _token: &Token, _token2: &Token) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }
    
    /// Handles This expression.
    ///
    fn visit_this(&mut self, _token: &Token) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }

    /// Handles Unary expression.
    ///
    fn visit_unary(&mut self, _token: &Token, _right: &Expr) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }

    /// Handles Variable expression.
    ///
    fn visit_variable(&mut self, _id: &str, _token: &Token) -> Result<Object, RuntimeError> {
        Ok(Object::None)
    }

    /// Handles Block statement.
    ///
    fn visit_block(&mut self, _stmts: &Vec<Rc<Stmt>>) -> Result<(), RuntimeError> {
        Ok(())
    }

    /// Handles Class statement.
    ///
    fn visit_class(&mut self, _token: &Token, _superclass: &Expr, _stmts: &Vec<Rc<Stmt>>) -> Result<(), RuntimeError> {
        Ok(())
    }

    /// Handles Expression statement.
    ///
    fn visit_expression(&mut self, _expr: &Expr) -> Result<(), RuntimeError> {
        Ok(())
    }

    /// Handles function statement.
    ///
    fn visit_function(&mut self, _stmt: &Rc<Stmt>, _name: &Token, _args: &Vec<Token>, _stmts: &Vec<Rc<Stmt>>) -> Result<(), RuntimeError> {
        Ok(())
    }

    /// Handles If statement.
    ///
    fn visit_if(&mut self, _expr: &Expr, _if_stmt: &Rc<Stmt>, _else_stmt: &Rc<Stmt>) -> Result<(), RuntimeError> {
        Ok(())
    }

    /// Handles Print statement.
    ///
    fn visit_print(&mut self, _expr: &Expr) -> Result<(), RuntimeError> {
        Ok(())
    }

    /// Handles Return statement.
    ///
    fn visit_return(&mut self, _token: &Token, _expr: &Expr) -> Result<(), RuntimeError> {
        Ok(())
    }

    /// Handles While statement.
    ///
    fn visit_while(&mut self, _expr: &Expr, _stmt: &Rc<Stmt>) -> Result<(), RuntimeError> {
        Ok(())
    }

    /// Handles Var statement.
    ///
    fn visit_var(&mut self, _token: &Token, _expr: &Expr) -> Result<(), RuntimeError> {
        Ok(())
    }
}

