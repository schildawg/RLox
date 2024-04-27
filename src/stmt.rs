use std::rc::Rc;

use crate::expr::Expr;
use crate::token::Token;

#[derive(PartialEq, Debug)]
pub enum Stmt {
    Block(Vec<Rc<Stmt>>),
    Class(Token, Expr, Vec<Rc<Stmt>>),
    Expression(Expr),
    Function(Token, Vec<Token>, Vec<Rc<Stmt>>),
    If(Expr, Rc<Stmt>, Rc<Stmt>),
    Print(Expr),
    Return(Token, Box<Expr>),
    While(Expr, Rc<Stmt>),
    Var(Token, Expr),
    None,
}
