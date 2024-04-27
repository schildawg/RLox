use std::rc::Rc;

use crate::lox::Lox;

use crate::error::ParseError;

use crate::object::Object;
use crate::token::{Token, TokenType};

use crate::expr::Expr;
use crate::stmt::Stmt;

use crate::expr::*;

type Expression = Box<Expr>;

/// Parses a list of tokens, creating a list of statements.
///
/// # Example
///
/// ```
/// let mut scanner = Scanner::new(String::from("3.14"));
/// scanner.scan_tokens();
///
/// let mut parser = Parser::new(scanner.tokens); 
/// let result = parser.primary().unwrap();
///
/// assert_eq!(literal!(Object::Number(3.14)), result);
/// ```
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    id_counter: u32,
}

impl Parser {
    // Factory method to create a new Parser.
    //
    pub fn new(tokens: Vec<Token>) -> Parser {     
        Parser { tokens, current: 0, id_counter: 0 }
    }

    /// Parses tokens, and returns a list of statements.
    ///
    pub fn parse(&mut self) -> Vec<Rc<Stmt>> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            let stmt = self.declaration();
            statements.push(stmt.into());
        }

        statements
    }

    // Parses a declaration.  This can be a function, variable declaration, or a statement.  If a parse error is returned,
    // synchronizes to the next statement to avoid unhelpful error messages.
    //
    fn declaration(&mut self) -> Stmt {
        if self.match_token(&[TokenType::Class]) {
            return self.class_declaration_sync();      
        }
        if self.match_token(&[TokenType::Fun]) {
            return self.function_sync();      
        }
        if self.match_token(&[TokenType::Var]) {
            return self.var_declaration_sync();    
        }
        return self.statement_sync();
    }

    // Parses a class declaration. 
    //
    // #Errors
    //
    // Returns a parse error if no class name.
    // Returns a parse error if no opening brace before body.
    // Returns a parse error if no closing brace after body.
    // Returns a parse error if no superclass name (when matches <)
    //
    fn class_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(&TokenType::Identifier, "Expect class name.")?;


        let mut superclass = Expr::None;
        if self.match_token(&[TokenType::Less]) {
            let _ = self.consume(&TokenType::Identifier, "Expect superclass name.")?;
            superclass = Expr::Variable("???".to_owned(), self.previous());        
        }

        self.consume(&TokenType::LeftBrace, "Expect '{{' before class body.")?;

        let mut methods: Vec<Rc<Stmt>> = Vec::new();
        if !self.check(&TokenType::RightBrace) {    
            loop {
                methods.push(self.function("method")?.into());
                if self.check(&TokenType::RightBrace) || self.is_at_end() {
                    break;
                }
            }
        } 
        self.consume(&TokenType::RightBrace, "Expect '}}' after class body.")?;
        Ok(Stmt::Class(name, superclass, methods))
    }


    // Parses a class declaration, synchronizing if error.
    //
    fn class_declaration_sync(&mut self) -> Stmt {
        match self.class_declaration() {
            Ok(value) => return value,
            Err(_err) => {
                self.synchronize();
                return Stmt::None;
            }
        }   
    }

    // Parses a function, synchronizing if error.
    //
    fn function_sync(&mut self) -> Stmt {
        match self.function("function") {
            Ok(value) => return value,
            Err(_err) => {
                self.synchronize();
                return Stmt::None;
            }
        }   
    }

    // Parses a variable declaration, synchronizing if error.
    //
    fn var_declaration_sync(&mut self) -> Stmt {
        match self.var_decaration() {
            Ok(value) => return value,
            Err(_err) => {
                self.synchronize();
                return Stmt::None;
            }
        }   
    }

    // Parses statement, synchronizing if error.
    //
    fn statement_sync(&mut self) -> Stmt {
        match self.statement() {
            Ok(value) => return value,
            Err(_err) => {
                self.synchronize();
                return Stmt::None;
            }
        }
    }


    // Parses a function.
    //
    // # Errors
    //
    // Returns a parse error if not correct function kind.
    // Returns a parse error if no open parenthesis after name.
    // Returns a parse error if more than 255 parameters.
    // Returns a parse error if parameter is not an identifier.
    // Returns a parse error if no close parenthesis after parameters.
    // Returns a parse error if no opening brace before body. 
    //
    fn function(&mut self, kind: &str) -> Result<Stmt, ParseError> {
        let name = self.consume(&TokenType::Identifier, &format!("Expect {} name.", kind))?;
        self.consume(&TokenType::LeftParen, &format!("Expect '(' after {} name.", kind))?;
        
        let mut parameters = Vec::new();

        if !self.check(&TokenType::RightParen) {    
            loop {
                if parameters.len() >= 255 {
                    return Err(Self::error(&self.peek(), "Can't have more than 255 parameters."));  
                }
                let param = self.consume(&TokenType::Identifier, "Expect parameter name.")?;
                parameters.push(param);
            
                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        self.consume(&TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(&TokenType::LeftBrace, &format!("Expect '{{' before {} body.", kind))?;

        let body = self.block()?;

        Ok(Stmt::Function(name, parameters, body))
    }

    // Continues parsing a var statement.  
    //
    // # Errors
    //
    // Returns parse error if left expression is not an identifier.
    // Returns parse error if statement doesn't end with semicolon.
    //
    fn var_decaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(&TokenType::Identifier, "Expect variable name.")?;
    
        let mut initializer = Expr::None;
        if self.match_token(&[TokenType::Equal]) {
            initializer = *self.expression()?;
        }
        self.consume(&TokenType::Semicolon, "Expect ';' after variable declaration.")?;

        Ok(Stmt::Var(name, initializer))
    }

    // Parses a statement.  Valid statements are For, If, Print, Return, While, Block, and Expression.
    //
    pub fn statement(&mut self) -> Result<Stmt, ParseError> {
        if  self.match_token(&[TokenType::For]) {
            return self.for_statement();
        }

        if  self.match_token(&[TokenType::If]) {
            return self.if_statement();
        }

        if  self.match_token(&[TokenType::Print]) {
            return self.print_statement();
        }

        if  self.match_token(&[TokenType::Return]) {
            return self.return_statement();
        }

        if self.match_token(&[TokenType::While]) {
            return self.while_statement();
        }
        
        if  self.match_token(&[TokenType::LeftBrace]) {
            return Ok(Stmt::Block(self.block()?));
        }
        return self.expression_statement();
    }

    // Parses an if statement.
    //
    // # Errors
    //
    // Returns parse error if expression doesn't start with left paren.
    // Returns parse error if expression doesn't end with right paren.
    //
    fn if_statement(&mut self) -> Result<Stmt, ParseError> {    
        self.consume(&TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after condition.")?;

        let then_branch = self.statement()?;
        let mut else_branch = Stmt::None;
        if self.match_token(&[TokenType::Else]) {
            else_branch = self.statement()?;
        }

        return Ok(Stmt::If(*condition, then_branch.into(), else_branch.into()));   
    }

    // Parses a for statement.  The for loop is syntactic sugar in Lox.  Builds and returns a while statement with initializers
    // and conditions to simulate the for loop. 
    //
    // # Errors
    //
    // Returns parse error if expression doesn't start with left paren.
    // Returns parse error if there is no semicolon after loop expression.
    // Returns parse error if expression doesn't end with right paren.
    //
    fn for_statement(&mut self) -> Result<Stmt, ParseError> {    
        self.consume(&TokenType::LeftParen, "Expect '(' after 'for'.")?;
        
        let initializer;
        if  self.match_token(&[TokenType::Semicolon]) {
            initializer = Stmt::None;
        } 
        else if self.match_token(&[TokenType::Var]) {
            initializer = self.var_decaration()?;
        } 
        else {
            initializer = self.expression_statement()?;
        } 

        let mut condition = Expr::None;
        if !self.check(&TokenType::Semicolon) {
            condition = *self.expression()?;
        }
        self.consume(&TokenType::Semicolon, "Expect ';' after loop condition")?;

        let mut increment = Expr::None;
        if !self.check(&TokenType::RightParen) {
            increment = *self.expression()?;
        }
        self.consume(&TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if increment != Expr::None {   
            body = Stmt::Block(vec![body.into(), Stmt::Expression(increment).into()]);
        }

        if condition == Expr::None {       
            condition = Expr::Literal(Object::Boolean(true));
        }
        body = Stmt::While(condition, body.into());

        if initializer != Stmt::None {
            body = Stmt::Block(vec![initializer.into(), body.into()]);
        }
        Ok(body)
    }

    // Parses a while statement.
    //
    // # Errors
    //
    // Returns a parse error if no open parenthesis after while.
    // Returns a parse error if no close parenthesis after condition.
    //
    fn while_statement(&mut self) -> Result<Stmt, ParseError> {    
        self.consume(&TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after condition.")?;

        let body = self.statement()?;

        return Ok(Stmt::While(*condition, body.into()));   
    }

    // Parses a Print statement.  Returns a parse error if not ended with semicolon.
    //
    fn print_statement(&mut self) -> Result<Stmt, ParseError> {    
        match self.expression() {
            Ok(value) => {
                self.consume(&TokenType::Semicolon, "Expect ; after value.")?;

                Ok(Stmt::Print(*value))
            },
            Err(err) => Err(err)
        }
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {    
        let keyword = self.previous();

        let mut value = Box::new(Expr::None);
        if !self.check(&TokenType::Semicolon) {
            value = self.expression()?;
        }
        self.consume(&TokenType::Semicolon, "Expect ; after return value.")?;

        return Ok(Stmt::Return(keyword, value));
    }

    // Parses an Expression statement.  Returns a parse error if not ended with a semicolon.
    // Calls expression().
    //
    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.expression() {
            Ok(value) => {
                self.consume(&TokenType::Semicolon, "Expect ; after value.")?;

                Ok(Stmt::Expression(*value))
            },
            Err(err) => Err(err)
        }
    }

    // Parses a block statement.  Returns a parse error if block is not ended with a }.
    //
    fn block(&mut self) -> Result<Vec<Rc<Stmt>>, ParseError> {
        let mut statements = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration().into());
        }

        self.consume(&TokenType::RightBrace, "Expect '}' after a block.")?;

        return Ok(statements);
    }

    // Parses an Expression.  Calls assignment().
    //
    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.assignment()
    }

    // Calls or() to continue evaluating expression.  If next token is Equal, returns an assignment, otherwise returns the 
    // expression.  For assignments, returns a parse error if the left expression isn't a variable.
    //
    fn assignment(&mut self) -> Result<Expression, ParseError> {
        let expr = self.or()?;

        if self.match_token(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;
            
            if let Expr::Variable(_, token) = *expr {
                self.id_counter += 1;
                return Ok(assign!(self.id_counter.to_string(), token, value));
            }
            if let Expr::Get(object, name) = *expr {
                return Ok(set!(*object, name, value));
            }

            Self::error(&equals, "Invalid assignment target.");
        }
        Ok(expr)
    }

    // Logical operator or has precedence after and.
    //
    fn or(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.and()?;
        
        while self.match_token(&[TokenType::Or]) {
            let operator = self.previous();
            let right = self.and()?;

            expr = logical!(expr, operator, right);
        }
        Ok(expr)  
    }

    // Logical operator and has precendence before or, and after equality.
    //
    fn and(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.equality()?;
        
        while self.match_token(&[TokenType::And]) {
            let operator = self.previous();
            let right = self.equality()?;

            expr = logical!(expr, operator, right);
        }
        Ok(expr)  
    }

    // Equality has precendence between comparison and logical operators.  Value equality types are EqualEqual
    // and BangEqual.  This ensures that == and != happen after and and or, and before >, >=, <, <=.
    //
    fn equality(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.comparison()?;

        while self.match_token(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();

            let right = self.comparison()?;
            
            expr = binary!(expr, operator, right);
        }
        Ok(expr)  
    }

    // Comparison has precendence between term and equality.  Valid comparison types are Greater, GreaterEqual,
    // Less and Less equal.  This makes comparisons happen after addition and subtraction (hence also multiplication
    // and division) and before equals and not equals.
    //
    fn comparison(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.term()?;

        while self.match_token(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let operator = self.previous();
 
            let right = self.term()?;
            
            expr = binary!(expr, operator, right);
        }
        Ok(expr)     
    }

    // Term has precendence between factor and comparison.  Valid term types are Minus and Plus.  This ensures 
    // addition and subtraction happen after division and multiplication, and after comparisions such as >, >=, 
    // < and <=.
    //
    fn term(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.factor()?;

        while self.match_token(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
 
            let right = self.factor()?;
            
            expr = binary!(expr, operator, right);
        }
        Ok(expr)  
    }

    // Factor has precendence between unary and term.  Valid factor types are Slash and Star.  This makes division
    // and multiplication happen after negation, and be addition and subtraction.
    //
    fn factor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.unary()?;

        while self.match_token(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
 
            let right = self.unary()?;
            
            expr = binary!(expr, operator, right);
        }
        Ok(expr)  
    }

    // Unary in next in precendence after function calls.  Valid unary types are Bang and Minus.
    //
    fn unary(&mut self) -> Result<Expression, ParseError> {    
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();     
            let right = self.unary()?;
            
            return Ok(unary!(operator, right));
        }
        self.call()
    }

    // If expression is followed by parenthesis, treat as function.
    // If expression is followed by dot, treat as getter.
    //
    // # Errors
    //
    // Return parse error if getter does not have property name.
    //
    fn call(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if self.match_token(&[TokenType::LeftParen]) {
                expr = self.finish_call(*expr)?;
            }
            else if self.match_token(&[TokenType::Dot]) {
                let name = self.consume(&TokenType::Identifier, "Expect property name after '.'.")?;
                expr = get!(expr, name);
            }
            else {
                break;
            }
        }
        Ok(expr)
    }

    // When expression is followed by a paren, it is a function call.  This method parses the arguments.
    //
    // # Errors
    // Returns a parse error if more than 255 arguments.
    // Returns a parse error if arguments are not followed by a closing paren.
    //
    fn finish_call(&mut self, callee: Expr) -> Result<Expression, ParseError> {
        let mut arguments = Vec::new();
        
        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(Self::error(&self.peek(), "Can't have more than 255 arguments."));
                }
                arguments.push(*self.expression()?);
                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        let paren = self.consume(&TokenType::RightParen, "Expect ')' after arguments.")?;

        Ok(call!(callee, paren, arguments))
    }

    // Primary has the highest precedence in parsing.  Consists of boolean, nil, number, string, identifier, and 
    // grouping.  
    // 
    // When encountering a parenthesis, returns a grouping of expression, raising its precedence.  If there is no
    // closing parenthesis, it returns a ParseError.
    //
    // Returns a ParseError if none of the above is matched.
    //
    fn primary(&mut self) -> Result<Expression, ParseError> {
        if self.match_token(&[TokenType::False]) {
            return Ok(boolean!(false));
        }
        
        if self.match_token(&[TokenType::True]) {
            return Ok(boolean!(true));
        }

        if self.match_token(&[TokenType::Nil]) {
            return Ok(literal!(Object::None));
        }

        if self.match_token(&[TokenType::Number, TokenType::String]) {
            return Ok(literal!(self.previous().to_literal()));
        }

        if self.match_token(&[TokenType::Super]) {
            let keyword = self.previous();
            self.consume(&TokenType::Dot, "Expect '.' after super.")?;
            
            let method = self.consume(&TokenType::Identifier, "Expect superclass method name.")?;

            return Ok(superman!(keyword, method));
        }

        if self.match_token(&[TokenType::This]) {
            return Ok(this!(self.previous()));
        }

        if self.match_token(&[TokenType::Identifier]) {
            self.id_counter += 1;

            return Ok(variable!(self.id_counter.to_string(), self.previous()));
        }

        if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;

            return Ok(grouping!(expr));
        }  
        Err(Self::error(&self.peek(), "Expect expression!"))
    }

    // Checks if the current token matches a given list of tokens.  If it does, advances and returns true.
    // Otherwise it returns false and does not change the token pointer.
    //
    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type) {
                self.advance();

                return true;
            }
        }
        false
    }

    // If the token matches, then advances and returns the previous token.  Otherwise returns a ParseError.
    //
    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<Token, ParseError> {
        if self.check(token_type) {
            return Ok(self.advance());
        }
        Err(Self::error(&self.peek(), message))
    }

    // Checks if token matches.  Always returns false if at end.
    //
    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false
        }
        &self.peek().token_type == token_type
    }

    // Advances the current token pointer if not at the end, and returns the previous token.
    //
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    // Checks to see if current token is Eof.
    //
    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    // Returns the current token.
    //
    fn peek(&self) -> Token {
        self.tokens[self.current].copy()
    }
    
    // Returns the previous token.
    //
    fn previous(&self) -> Token {
        self.tokens[self.current - 1].copy()
    }

    // Reports an error to Lox and returns it.
    //
    fn error(token: &Token, message: &str) -> ParseError {
        Lox::token_error(token, message);

        ParseError { message: String::from(message) }
    }

    // When a error occurs, we don't want to flood the user with cascading errors.  But we also don't want
    // to feed them one error at a time.  This method provides a synchronization mechanism to skip errors until
    // the next statement, and then resume normal parsing.
    //
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class|TokenType::For|TokenType::Fun|TokenType::If|TokenType::Print|
                TokenType::Return|TokenType::Var|TokenType::While => return,
                _ => (),
            }
            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::ParseError;

    use crate::scanner::Scanner;
    use crate::parser::Parser;

    use crate::object::Object;
    use crate::stmt::Stmt;

    use crate::token::*;
    use crate::expr::*;
    use crate::GLOBALS;

    // Convenience method to scan tokens from a string, to make code more readable than building by hand.
    //
    fn scan(value: &str) -> Vec<Token> {
        let mut scanner = Scanner::new(value.to_owned());      
        scanner.scan_tokens();

        scanner.tokens
    }

    // Convenience method to assert failure of an expression with a given message.
    //
    fn assert_fail(result: Result<Box<Expr>, ParseError>, message: &str) {
        match result {
            Ok(_) => { 
                panic!("should fail")
            }
            Err(err) => { 
                assert_eq!(message, err.message);
            }
        }
    }

    // Convienience method to assert failure of a statement with a given message.
    //
    fn assert_stmt_fail(result: Result<Stmt, ParseError>, message: &str) {
        match result {
            Ok(_) => { 
                panic!("should fail")
            }
            Err(err) => { 
                assert_eq!(message, err.message);
            }
        }
    }

    // Parsing a false token should return a false literal.
    //
    #[test]
    fn parse_false() {
        let mut parser = Parser::new(scan("false"));
        let result = parser.primary().unwrap();

        assert_eq!(boolean!(false), result);
    }    

    // Parsing a true token should return a true literal.
    //
    #[test]
    fn parse_true() {
        let mut parser = Parser::new(scan("true"));
        let result = parser.primary().unwrap();

        assert_eq!(boolean!(true), result);
    } 
    
    // Parsing a nil token should return a literal of Object::None.
    //
    #[test]
    fn parse_nil() {
        let mut parser = Parser::new(scan("nil"));
        let result = parser.primary().unwrap();

        assert_eq!(literal!(Object::None), result);
    } 

    // Parsing a number token should return a literal of Object::Number.
    //
    #[test]
    fn parse_number() {
        let mut parser = Parser::new(scan("3.14"));
        let result = parser.primary().unwrap();

        assert_eq!(literal!(Object::Number(3.14)), result);
    } 

    // Parsing this should return a This expression.
    //
    #[test]
    fn parse_this() {
        let mut parser = Parser::new(scan("this"));
        let result = parser.primary().unwrap();

        assert_eq!(this!(token!(TokenType::This, "this", Object::None, 1)), result);
    } 

    // Parsing a string token should return a literal of Object::String.
    //
    #[test]
    fn parse_string() {
        let mut parser = Parser::new(scan("\"ABC\""));
        let result = parser.primary().unwrap();

        assert_eq!(literal!(Object::String("ABC".to_owned())), result);
    } 

    // Parsing a non-keyword identifier token should return a variable expression of Identifier with the 
    // same name and literal value of None.
    //
    #[test]
    fn parse_identifier() {
        let mut parser = Parser::new(scan("test"));
        let result = parser.primary().unwrap();

        assert_eq!(variable!("1".to_string(), token!(TokenType::Identifier, "test", Object::None, 1)), result);
    } 

    // Parsing a token of paranthesis should return a Grouping expression.
    //
    #[test]
    fn parse_paren() {
        let mut parser = Parser::new(scan("(1)"));
        let result = parser.primary().unwrap();

        let expr = literal!(Object::Number(1.0));
        let expr = grouping!(expr);

        assert_eq!(expr, result);
    } 

    // When parsing parenthesis, primary should return an error when there is an unmatched closing parenthesis.
    // 
    #[test]
    fn parse_paren_error() {
        let mut parser = Parser::new(scan("(1"));
        let result = parser.primary();

        assert_fail(result, "Expect ')' after expression.");
    } 

    // Should fail if reaches the end and a valid primary expression is not matched.
    // 
    #[test]
    fn parse_primary_error() {
        let mut parser = Parser::new(scan("-1"));   // <-- this is valid unary :)
        let result = parser.primary();

        assert_fail(result, "Expect expression!");
    } 

    // Negative numbers should return a unary with token type of Minus.
    //
    #[test]
    fn parse_unary_minus() {
        let mut parser = Parser::new(scan("-1"));
        let result = parser.unary().unwrap();

        let minus = token!(TokenType::Minus, "-", Object::None, 1);

        let expr = literal!(Object::Number(1.0));
        let expr = unary!(minus, expr);

        assert_eq!(expr, result);
    } 

    // Parsing bang (!) should return a unary of token type Bang.
    //
    #[test]
    fn parse_unary_bang() {
        let mut parser = Parser::new(scan("!true"));
        let result = parser.unary().unwrap();

        let bang = token!(TokenType::Bang, "!", Object::None, 1);

        let expr = literal!(Object::Boolean(true));
        let expr = unary!(bang, expr);

        assert_eq!(expr, result);
    }
    
    // Parsing a Slash (/) should return a Binary expression with left and right expressions.
    //
    #[test]
    fn parse_factor_slash() {
        let mut parser = Parser::new(scan("1/2"));
        let result = parser.factor().unwrap();

        let slash = token!(TokenType::Slash, "/", Object::None, 1);

        let left = literal!(Object::Number(1.0));
        let right = literal!(Object::Number(2.0));
        let expr = binary!(left, slash, right);

        assert_eq!(expr, result);
    }

    // Parsing a Star (*) should return a Binary expression with left and right expressions.
    #[test]
    fn parse_factor_star() {
        let mut parser = Parser::new(scan("1*2"));
        let result = parser.factor().unwrap();

        let star = token!(TokenType::Star, "*", Object::None, 1);

        let left = literal!(Object::Number(1.0));
        let right = literal!(Object::Number(2.0));
        let expr = binary!(left, star, right);

        assert_eq!(expr, result);
    }

    // Parsing a Plus (+) should return a Binary expression with left and right expressions.
    //
    #[test]
    fn parse_term_plus() {
        let mut parser = Parser::new(scan("1+2"));
        let result = parser.term().unwrap();

        let operator = token!(TokenType::Plus, "+", Object::None, 1);

        let left = literal!(Object::Number(1.0));
        let right = literal!(Object::Number(2.0));
        let expr = binary!(left, operator, right);

        assert_eq!(expr, result);
    }

    // Parsing a Minus (-) should return a Binary expression with left and right expressions.
    //
    #[test]
    fn parse_term_minus() {
        let mut parser = Parser::new(scan("1-2"));
        let result = parser.term().unwrap();

        let operator = token!(TokenType::Minus, "-", Object::None, 1);

        let left = literal!(Object::Number(1.0));
        let right = literal!(Object::Number(2.0));
        let expr = binary!(left, operator, right);

        assert_eq!(expr, result);
    }

    // Parsing a Greater (>) should return a Binary expression with left and right expressions.
    //
    #[test]
    fn parse_comparison_greater() {
        let mut parser = Parser::new(scan("1 > 2"));
        let result = parser.comparison().unwrap();

        let operator = token!(TokenType::Greater, ">", Object::None, 1);

        let left = literal!(Object::Number(1.0));
        let right = literal!(Object::Number(2.0));
        let expr = binary!(left, operator, right);

        assert_eq!(expr, result);
    }

    // Parsing a GreaterEqual (>=) should return a Binary expression with left and right expressions.
    //
    #[test]
    fn parse_comparison_greater_equal() {
        let mut parser = Parser::new(scan("1 >= 2"));
        let result = parser.comparison().unwrap();

        let operator = token!(TokenType::GreaterEqual, ">=", Object::None, 1);

        let left = literal!(Object::Number(1.0));
        let right = literal!(Object::Number(2.0));
        let expr = binary!(left, operator, right);

        assert_eq!(expr, result);
    }

    // Parsing a Less (<) should return a Binary expression with left and right expressions.
    //
    #[test]
    fn parse_comparison_less() {
        let mut parser = Parser::new(scan("1 < 2"));
        let result = parser.comparison().unwrap();

        let operator = token!(TokenType::Less, "<", Object::None, 1);

        let left = literal!(Object::Number(1.0));
        let right = literal!(Object::Number(2.0));
        let expr = binary!(left, operator, right);

        assert_eq!(expr, result);
    }

    // Parsing a LessEqual (<=) should return a Binary expression with left and right expressions.
    //
    #[test]
    fn parse_comparison_less_equal() {
        let mut parser = Parser::new(scan("1 <= 2"));
        let result = parser.comparison().unwrap();

        let operator = token!(TokenType::LessEqual, "<=", Object::None, 1);

        let left = literal!(Object::Number(1.0));
        let right = literal!(Object::Number(2.0));
        let expr = binary!(left, operator, right);

        assert_eq!(expr, result);
    }

    // Parsing a BangEqual (!=) should return a Binary expression with left and right expressions.
    //
    #[test]
    fn parse_comparison_bang_equal() {
        let mut parser = Parser::new(scan("1 != 2"));
        let result = parser.equality().unwrap();

        let operator = token!(TokenType::BangEqual, "!=", Object::None, 1);

        let left = literal!(Object::Number(1.0));
        let right = literal!(Object::Number(2.0));
        let expr = binary!(left, operator, right);

        assert_eq!(expr, result);
    }

    // Parsing a EqualEqual (==) should return a Binary expression with left and right expressions.
    //
    #[test]
    fn parse_comparison_equal_equal() {
        let mut parser = Parser::new(scan("1 == 2"));
        let result = parser.equality().unwrap();

        let operator = token!(TokenType::EqualEqual, "==", Object::None, 1);

        let left = literal!(Object::Number(1.0));
        let right = literal!(Object::Number(2.0));
        let expr = binary!(left, operator, right);

        assert_eq!(expr, result);
    }

    // Test parsing an or expression.
    //
    #[test]
    fn parse_or() {
        let mut parser = Parser::new(scan("true or false"));
        let result = parser.or().unwrap();

        let operator = token!(TokenType::Or, "or", Object::None, 1);

        let left = literal!(Object::Boolean(true));
        let right = literal!(Object::Boolean(false));
        let expr = logical!(left, operator, right);

        assert_eq!(expr, result);
    }

    // Test parsing an and expression.
    //
    #[test]
    fn parse_and() {
        let mut parser = Parser::new(scan("true and false"));
        let result = parser.or().unwrap();

        let operator = token!(TokenType::And, "and", Object::None, 1);

        let left = literal!(Object::Boolean(true));
        let right = literal!(Object::Boolean(false));
        let expr = logical!(left, operator, right);

        assert_eq!(expr, result);
    }

    // Tests parsing a print statement.
    //
    #[test]
    fn parse_print_statement() {
        let mut parser = Parser::new(scan("print 123;"));
        let result = parser.statement().expect("REASON");

        let expr = *literal!(Object::Number(123.0));
        let stmt = Stmt::Print(expr);

        assert_eq!(stmt, result);
    }

    // Print statement should end with a semicolon.
    //
    #[test]
    fn parse_print_expect_semicolon() {
        let mut parser = Parser::new(scan("print 123"));
        let result = parser.statement();

        assert_stmt_fail(result, "Expect ; after value.");
    }

    // Test parsing an expression statement.
    //
    #[test]
    fn parse_expression_statement() {
        let mut parser = Parser::new(scan("a = 1;"));
        let result = parser.statement().expect("REASON");

        let token = token!(TokenType::Identifier, "a", Object::None, 1);

        let expr = literal!(Object::Number(1.0));
        
        let expr = *assign!("2".to_string(), token, expr);

        let stmt = Stmt::Expression(expr);

        assert_eq!(stmt, result);
    }

    // Should not be able to assign to a rvalue.
    //
    #[test]
    fn parse_expression_invalid_assignment() {
        let mut parser = Parser::new(scan("1 = 1;"));
        parser.expression();

        let globals = GLOBALS.lock().unwrap();

        assert_eq!("[line 1] Error at '=': Invalid assignment target.", globals.last_error);
    }

    // Expression statement should end with a semicolon.
    //
    #[test]
    fn parse_expression_expect_semicolon() {
        let mut parser = Parser::new(scan("a = 1"));
        let result = parser.statement();

        assert_stmt_fail(result, "Expect ; after value.");
    }

    // Tests parsing a declaration (var statement).
    #[test]
    fn parse_var_statement() {
        let mut parser = Parser::new(scan("var a = 1;"));

        let result = parser.declaration();
        
        let token = token!(TokenType::Identifier, "a", Object::None, 1);

        let expr = *literal!(Object::Number(1.0));

        let stmt = Stmt::Var(token, expr);

        assert_eq!(stmt, result);
    }  
    
    // The left expression of a declaration should be a variable.
    //
    #[test]
    fn parse_var_expect_variable_name() {
        let mut parser = Parser::new(scan("true = 1;"));  // <- method starts after "var"

        let result = parser.var_decaration();
        
        assert_stmt_fail(result, "Expect variable name.");
    }  

    // A declaration should end with a semicolon.
    //
    #[test]
    fn parse_var_expect_semicolon() {
        let mut parser = Parser::new(scan("a = 1"));  // <- method starts after "var"

        let result = parser.var_decaration();
        
        assert_stmt_fail(result, "Expect ';' after variable declaration.");
    }  

    // Verify that block statements are parsed successfully.
    //
    #[test]
    fn parse_block_statement() {
        let mut parser = Parser::new(scan("{ var a = 1; }"));

        let result = parser.parse();
        let result = result.get(0).unwrap();
        
        let token = token!(TokenType::Identifier, "a", Object::None, 1);

        let expr = *literal!(Object::Number(1.0));

        let stmt = Stmt::Var(token, expr);
        let stmt = Stmt::Block(vec![stmt.into()]);

        assert_eq!(stmt, **result);
    }  

    // Parsing a block should return a parse error if it is does not have a closing brace.
    //
    #[test]
    fn parse_block_expect_close() {
        let mut parser = Parser::new(scan("{ var a = 1;")); 

        let result = parser.statement();
        
        assert_stmt_fail(result, "Expect '}' after a block.");
    }  

    // Tests parsing an if statement.
    //
    #[test]
    fn parse_if_statement() {
        let mut parser = Parser::new(scan("if (true) print true; else print false;"));

        let result = parser.parse();
        let result = result.get(0).unwrap();

        let literal_true = *literal!(Object::Boolean(true));

        let print_true = Stmt::Print(*literal!(Object::Boolean(true)));
        let print_false = Stmt::Print(*literal!(Object::Boolean(false)));

        let stmt = Stmt::If(literal_true, print_true.into(), print_false.into());

        assert_eq!(stmt, **result);
    }  

    // Parsing an if statement without an opening paren should return a parse error.
    // 
    #[test]
    fn parse_if_expect_opening_paren() {
        let mut parser = Parser::new(scan("if true) print true; else print false;")); 

        let result = parser.statement();
        
        assert_stmt_fail(result, "Expect '(' after 'if'.");
    }  

    // Parsing an if statement without a closing paren should return a parse error.
    //
    #[test]
    fn parse_if_expect_closing_paren() {
        let mut parser = Parser::new(scan("if (true print true; else print false;")); 

        let result = parser.statement();
        
        assert_stmt_fail(result, "Expect ')' after condition.");
    }  

    // Tests parsing a while statement.
    //
    #[test]
    fn parse_while_statement() {
        let mut parser = Parser::new(scan("while (true) print true;"));

        let result = parser.parse();
        let result = result.get(0).unwrap();

        let literal_true = *literal!(Object::Boolean(true));

        let print_true = Stmt::Print(*literal!(Object::Boolean(true)));

        let stmt = Stmt::While(literal_true, print_true.into());

        assert_eq!(stmt, **result);
    }  

    // A while statement missing a left paren should return a parse error.
    //
    #[test]
    fn parse_while_expect_opening_paren() {
        let mut parser = Parser::new(scan("while true) print true;")); 

        let result = parser.statement();
        
        assert_stmt_fail(result, "Expect '(' after 'while'.");
    }  

    // A while statement missing the right paren should return a parse error.
    //
    #[test]
    fn parse_while_expect_closing_paren() {
        let mut parser = Parser::new(scan("while (true print true;")); 

        let result = parser.statement();
        
        assert_stmt_fail(result, "Expect ')' after condition.");
    }
    
    // A for statement should return an elaborate while statement.
    //
    #[test]
    fn parse_for_statement() {
        let mut parser = Parser::new(scan("for (var i = 0; i < 10; i = i + 1) print true;"));

        let result = parser.parse();
        let result = result.get(0).unwrap();

        // whew...
        let expected = "Block([Var(Token { token_type: Identifier, lexeme: \"i\", literal: None, line: 1 }, Literal(Number(0.0))), While(Binary(Variable(\"1\", Token { token_type: Identifier, lexeme: \"i\", literal: None, line: 1 }), Token { token_type: Less, lexeme: \"<\", literal: None, line: 1 }, Literal(Number(10.0))), Block([Print(Literal(Boolean(true))), Expression(Assign(\"4\", Token { token_type: Identifier, lexeme: \"i\", literal: None, line: 1 }, Binary(Variable(\"3\", Token { token_type: Identifier, lexeme: \"i\", literal: None, line: 1 }), Token { token_type: Plus, lexeme: \"+\", literal: None, line: 1 }, Literal(Number(1.0)))))]))])";
        assert_eq!(expected, format!("{:?}", result));
    }  

    // For statement missing opening paren should return a parse error.
    //
    #[test]
    fn parse_for_expect_opening_paren() {
        let mut parser = Parser::new(scan("for var i = 0; i < 10; i = i + 1) print true;")); 

        let result = parser.statement();
        
        assert_stmt_fail(result, "Expect '(' after 'for'.");
    }  

    // For statement missing closing paren should return a parse error.
    //
    #[test]
    fn parse_for_expect_closing_paren() {
        let mut parser = Parser::new(scan("for (var i = 0; i < 10; i = i + 1 print true;")); 

        let result = parser.statement();
        
        assert_stmt_fail(result, "Expect ')' after for clauses.");
    }

    // For statement missing a semicolon after the initializer should return a parse error.
    //
    #[test]
    fn parse_for_expect_semicolon() {
        let mut parser = Parser::new(scan("for (var i = 0) print true;")); 

        let result = parser.statement();
        
        assert_stmt_fail(result, "Expect ';' after variable declaration.");
    }

    // Tests parsing a function call
    //
    #[test]
    fn parse_function_call() {
        let mut parser = Parser::new(scan("test(1, 2);")); 

        let result = parser.parse();
        let result = result.get(0).unwrap();

        let expected = "Expression(Call(Variable(\"1\", Token { token_type: Identifier, lexeme: \"test\", literal: None, line: 1 }), Token { token_type: RightParen, lexeme: \")\", literal: None, line: 1 }, [Literal(Number(1.0)), Literal(Number(2.0))]))";
        assert_eq!(expected, format!("{:?}", result));
    }
    
    // Parsing should accept multiple parenthesis.
    //
    #[test]
    fn parse_function_call_multiple_paren() {
        let mut parser = Parser::new(scan("test(1, 2)();")); 

        let result = parser.parse();
        let result = result.get(0).unwrap();

        let expected = "Expression(Call(Call(Variable(\"1\", Token { token_type: Identifier, lexeme: \"test\", literal: None, line: 1 }), Token { token_type: RightParen, lexeme: \")\", literal: None, line: 1 }, [Literal(Number(1.0)), Literal(Number(2.0))]), Token { token_type: RightParen, lexeme: \")\", literal: None, line: 1 }, []))";
        assert_eq!(expected, format!("{:?}", result));
    }

    // Should return a parse error if arguments are not followed by a close parent.
    //
    #[test]
    fn parse_call_expect_close_paren() {
        let mut parser = Parser::new(scan("test(1, 2;")); 

        let result = parser.statement();
        
        assert_stmt_fail(result, "Expect ')' after arguments.");
    }

    // Should return a parse error if more than 255 arguments.
    //
    #[test]
    fn parse_call_less_than_255() {
        let mut arguments = String::new();
        for i in 1..299 {
            arguments.push_str(&i.to_string());
            arguments.push_str(", ");
        }
        arguments.push_str("300");
        
        let value = format!("test({});", arguments);
        let mut parser = Parser::new(scan(&value)); 

        let result = parser.statement();
        
        assert_stmt_fail(result, "Can't have more than 255 arguments.");
    }


    // Tests parsing a function.
    //
    #[test]
    fn parse_function() {
        let mut parser = Parser::new(scan("fun test(a, b) {}"));

        let result = parser.parse();
        let result = result.get(0).unwrap();

        let identifier = token!(TokenType::Identifier, "test", Object::None, 1);
        let param1 = token!(TokenType::Identifier, "a", Object::None, 1);
        let param2 = token!(TokenType::Identifier, "b", Object::None, 1);
        let stmt = Stmt::Function(identifier, vec![param1, param2], vec![]);

        assert_eq!(stmt, **result);
    }   

    // Should return a parse error if no opening parenthesis
    //
    #[test]
    fn parse_function_no_open_paren() {
        let mut parser = Parser::new(scan("test"));  // <-- starts after fun

        let result = parser.function("function");
        
        assert_stmt_fail(result, "Expect '(' after function name.");
    }  

    // Should return a parse error if more than 255 parameters
    //
    #[test]
    fn parse_function_more_than_255_parameters() {
        let mut signature = String::new();
        signature.push_str("test(");
        for i in 1..299 {
            signature.push_str("param");
            signature.push_str(&i.to_string());
            signature.push_str(", ");
        }
        signature.push_str("param300) {{}}");
        let mut parser = Parser::new(scan(&signature));  // <-- starts after fun

        let result = parser.function("function");
        
        assert_stmt_fail(result, "Can't have more than 255 parameters.");
    }  

    // Should return a parse error if parameter is not an identifier.
    //
    #[test]
    fn parse_function_parameter_not_identifier() {
        let mut parser = Parser::new(scan("test(1, 2, 3)"));  // <-- starts after fun

        let result = parser.function("function");
        
        assert_stmt_fail(result, "Expect parameter name.");
    }  

    // Should return a parse error if no closing parenthesis.
    //
    #[test]
    fn parse_function_no_close_paren() {
        let mut parser = Parser::new(scan("test(a, b"));  // <-- starts after fun

        let result = parser.function("function");
        
        assert_stmt_fail(result, "Expect ')' after parameters.");
    }  

    // Should return a parse error if no closing parenthesis.
    //
    #[test]
    fn parse_function_no_open_brace() {
        let mut parser = Parser::new(scan("test(a, b)"));  // <-- starts after fun

        let result = parser.function("function");
        
        assert_stmt_fail(result, "Expect '{' before function body.");
    }  

    // Tests parsing a class!!
    //
    #[test]
    fn parse_class_declaration() {
        let mut parser = Parser::new(scan("
            class Breakfast {
                cook() {
                    print \"Egg a-frying!\";
                } 

                serve(who) {
                    print \"Enjoy your breakfast, \" + who + \".\";
                }
            }
         }
        ")); 

        let result = parser.declaration();
        
        let expected = "Class(Token { token_type: Identifier, lexeme: \"Breakfast\", literal: None, line: 2 }, None, [Function(Token { token_type: Identifier, lexeme: \"cook\", literal: None, line: 3 }, [], [Print(Literal(String(\"Egg a-frying!\")))]), Function(Token { token_type: Identifier, lexeme: \"serve\", literal: None, line: 7 }, [Token { token_type: Identifier, lexeme: \"who\", literal: None, line: 7 }], [Print(Binary(Binary(Literal(String(\"Enjoy your breakfast, \")), Token { token_type: Plus, lexeme: \"+\", literal: None, line: 8 }, Variable(\"1\", Token { token_type: Identifier, lexeme: \"who\", literal: None, line: 8 })), Token { token_type: Plus, lexeme: \"+\", literal: None, line: 8 }, Literal(String(\".\"))))])])";
        assert_eq!(expected, format!("{:?}", result));

    }  

    // Should return a parse error when there's no identifier.
    //
    #[test]
    fn parse_class_no_identifier() {
        let mut parser = Parser::new(scan("123"));  // <-- starts after class

        let result = parser.class_declaration();
        
        assert_stmt_fail(result, "Expect class name.");
    }  

    // Should return a parse error when there's no opening brace.
    //
    #[test]
    fn parse_class_no_opening_brace() {
        let mut parser = Parser::new(scan("Breakfast"));  // <-- starts after class

        let result = parser.class_declaration();
        
        assert_stmt_fail(result, "Expect '{{' before class body.");
    }  

    // Should return a parse error when there's no closing brace.
    //
    #[test]
    fn parse_class_no_closing_brace() {
        let mut parser = Parser::new(scan("
        Breakfast {
            cook() {
               print \"Egg a-frying!\";
            } 
        "));  // <-- starts after class

        let result = parser.class_declaration();
        
        assert_stmt_fail(result, "Expect '}}' after class body.");
    }  

    // Should return a parse error when parses < but no superclass name.
    //
    #[test]
    fn parse_class_no_superclass() {
        let mut parser = Parser::new(scan("
        Breakfast < 123 {
            cook() {
               print \"Egg a-frying!\";
            } 
        "));  // <-- starts after class

        let result = parser.class_declaration();
        
        assert_stmt_fail(result, "Expect superclass name.");
    }  

    // Class getter should have a valid property name.
    //
    #[test]
    fn parse_class_getter_valid_property_name() {
        let mut parser = Parser::new(scan("
            print bagel.123;
        "));

        let result = parser.statement();
        
        assert_stmt_fail(result, "Expect property name after '.'.");
    }  

    // Tests getters!!
    //
    #[test]
    fn parse_class_getter() {
        let mut parser = Parser::new(scan("
            print egg.scramble(3).with(cheddar);
        "));

        let result = parser.statement().unwrap();
        
        let expected = "Print(Call(Get(Call(Get(Variable(\"1\", Token { token_type: Identifier, lexeme: \"egg\", literal: None, line: 2 }), Token { token_type: Identifier, lexeme: \"scramble\", literal: None, line: 2 }), Token { token_type: RightParen, lexeme: \")\", literal: None, line: 2 }, [Literal(Number(3.0))]), Token { token_type: Identifier, lexeme: \"with\", literal: None, line: 2 }), Token { token_type: RightParen, lexeme: \")\", literal: None, line: 2 }, [Variable(\"2\", Token { token_type: Identifier, lexeme: \"cheddar\", literal: None, line: 2 })]))";
        assert_eq!(expected, format!("{:?}", result));
    }  

    // Tests setter!!
    //
    #[test]
    fn parse_class_setter() {
        let mut parser = Parser::new(scan("
            print eggs.count = 42;
        "));

        let result = parser.statement().unwrap();
        
        let expected = "Print(Set(Variable(\"1\", Token { token_type: Identifier, lexeme: \"eggs\", literal: None, line: 2 }), Token { token_type: Identifier, lexeme: \"count\", literal: None, line: 2 }, Literal(Number(42.0))))";
        assert_eq!(expected, format!("{:?}", result));
    }  
}