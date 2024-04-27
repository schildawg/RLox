
use crate::token::Token;
use crate::object::Object;

pub type ExprRef = Box<Expr>;

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Assign(String, Token, ExprRef),
    Binary(ExprRef, Token, ExprRef),
    Call(ExprRef, Token, Vec<Expr>),
    Get(ExprRef, Token),
    Grouping(ExprRef),
    Literal(Object),
    Logical(ExprRef, Token, ExprRef),
    Set(ExprRef, Token, ExprRef),    
    Super(Token, Token),
    This(Token),
    Unary(Token, ExprRef),
    Variable(String, Token),

    None,
}


macro_rules! number {
    ($a:expr) => {
        Box::new(Expr::Literal(Object::Number($a)))
    };
}

macro_rules! boolean {
    ($a:expr) => {
        Box::new(Expr::Literal(Object::Boolean($a)))
    };
}

macro_rules! literal {
    ($a:expr) => {
        Box::new(Expr::Literal($a))
    };
}

macro_rules! this {
    ($a:expr) => {
        Box::new(Expr::This($a))
    };
}

macro_rules! superman {
    ($a:expr,$b:expr) => {
        Box::new(Expr::Super($a,$b))
    };
}


macro_rules! grouping {
    ($a:expr) => {
        Box::new(Expr::Grouping($a))
    };
}

macro_rules! variable {
    ($a:expr,$b:expr) => {
        Box::new(Expr::Variable($a,$b))
    };
}

macro_rules! assign {
    ($a:expr,$b:expr,$c:expr) => {
        Box::new(Expr::Assign($a,$b,$c))
    };
}

macro_rules! binary {
    ($a:expr,$b:expr,$c:expr) => {
        Box::new(Expr::Binary($a,$b,$c))
    };
}

macro_rules! logical {
    ($a:expr,$b:expr,$c:expr) => {
        Box::new(Expr::Logical($a,$b,$c))
    };
}

macro_rules! call {
    ($a:expr,$b:expr,$c:expr) => {
        Box::new(Expr::Call(Box::new($a),$b,$c))
    };
}

macro_rules! unary {
    ($a:expr,$b:expr) => {
        Box::new(Expr::Unary($a,$b))
    };
}

macro_rules! get {
    ($a:expr,$b:expr) => {
        Box::new(Expr::Get($a,$b.copy()))
    };
}

macro_rules! set {
    ($a:expr,$b:expr,$c:expr) => {
        Box::new(Expr::Set(Box::new($a),$b,$c))
    };
}

pub (crate) use {number, boolean, literal, assign, grouping, binary, logical, call, unary, variable, get, set, this, superman};

pub trait ExprVisitor<T> {
    fn visit_expr(&mut self, e: &Expr) -> T;
}

pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, expr: &Expr) {
        println!("{}", self.visit_expr(expr));
    } 

    pub fn parenthesize(&mut self, name: &str, exprs: Vec<&Expr>) -> String {
        let mut builder = String::from("(");
        builder.push_str(name);

        for expr in exprs {
            builder.push_str(" ");
            builder.push_str(&(self.visit_expr(&expr)));
        }
        builder.push_str(")");

        return builder;
    }
}

impl ExprVisitor<String> for AstPrinter {
    fn visit_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Binary(left, operator, right) => 
                self.parenthesize(&operator.lexeme, vec![left, right]),

            Expr::Grouping(expr) => 
                self.parenthesize("group", vec![expr]),

            Expr::Literal(object) => 
                object.to_string(),

            Expr::Unary(operator, right) => 
                self.parenthesize(&operator.lexeme, vec![right]),

            _ => panic!("not supported"),
        }
    }
}
