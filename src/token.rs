use std::fmt;

use crate::object::Object;

/// Defines all token types used by Lox.
/// 
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star, 
    
    Bang, BangEqual, Equal, EqualEqual,
    Greater, GreaterEqual, Less, LessEqual,
    
    Identifier, String, Number,
    
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
    
    Eof,
}

/// Token.  
///
/// Contains information about lexemes and the mapped type, literal value, and line location.
///
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
   pub token_type: TokenType,
   pub lexeme: String,
   pub literal: Object,
   pub line: usize,
}

impl Token {
    /// Factory method to create a new Token.
    ///
    pub fn new(token_type: TokenType, lexeme: String, literal: Object, line: usize) -> Token {
        Token { token_type, lexeme, literal, line}
    }

    /// Formats as a string. 
    ///
    pub fn to_string(&self) -> String {
        format!("{:?} {} {}", self.token_type, self.lexeme, self.literal.to_string())
    }

    /// Returns a copy of lexeme.
    ///
    pub fn to_lexeme(&self) -> String {
        self.lexeme.to_string()
    }

    /// Returns a copy of literal.
    ///
    pub fn to_literal(&self) -> Object {
        self.literal.copy()
    }

    /// Returns a copy of Token.   
    ///
    pub fn copy(&self) -> Token {
        self.clone()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

/// macro_rules!
///
/// Convenience macro for creating token.  Converts string reference to String.
///
macro_rules! token {
    ($a:expr,$b:expr,$c:expr,$d:expr) => {
        Token::new($a,String::from($b),$c,$d)
    };
}
pub (crate) use token;

#[cfg(test)]
mod tests {
    use crate::token::{Token, TokenType};
    use crate::object::Object;

    // Tests creating a new Token.
    //
    #[test]
    fn new_token() {
        let token = Token::new(TokenType::String, String::from("ABC"), Object::None, 1);

        assert_eq!(TokenType::String, token.token_type);
        assert_eq!("ABC", token.lexeme);
        assert_eq!(Object::None, token.literal);
        assert_eq!(1, token.line);
    }

    // Tests creating a new Token using convenience macro.
    //
    #[test]
    fn token_macro() {
        let token = token!(TokenType::String, "ABC", Object::None, 1);

        assert_eq!(TokenType::String, token.token_type);
        assert_eq!("ABC", token.lexeme);
        assert_eq!(Object::None, token.literal);
        assert_eq!(1, token.line);
    }

    // Tests converting a Token to a String.
    //
    #[test]
    fn token_to_string() {
        let token = Token::new(TokenType::String, String::from("ABC"), Object::None, 1);

        // TODO: Do we want nil here??
        assert_eq!("String ABC nil", token.to_string());
    }

    // Tests returning a copy of lexeme.
    //
    #[test]
    fn token_to_lexeme() {
        let token = Token::new(TokenType::String, String::from("ABC"), Object::None, 1);

        assert_eq!("ABC", token.to_lexeme());
    }

    // Tests returning a copy of literal.
    //
    #[test]
    fn token_to_literal() {
        let token = Token::new(TokenType::String, String::from("ABC"), Object::String("ABC".to_string()), 1);

        assert_eq!(Object::String("ABC".to_string()), token.to_literal());
    }
}