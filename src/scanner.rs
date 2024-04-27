use std::collections::HashMap;

use ahash::{AHasher, RandomState};

use crate::lox::Lox;

use crate::token::{Token, TokenType};
use crate::token::{token};

use crate::object::Object;

/// Scans a source string, converting it into a list of Tokens to be used by Parser.
///
/// # Example
///
/// ```
/// let mut scanner = Scanner::new(String::from("fun"));
///        
/// scanner.scan_tokens();
///
/// assert_eq!(TokenType::Fun, scanner.tokens[0].token_type);
/// ```
pub struct Scanner {
    pub source: String,
    pub tokens: Vec<Token>,

    pub line: usize,

    chars: Vec<char>,
    start: usize,
    current: usize,

    keywords: HashMap<String, TokenType, RandomState>,
}

impl Scanner {
    /// Factory method to create Scanner.
    ///
    /// Sets the source, and starts the scanner at the beginning, with empty tokens, and line at 1.
    ///
    pub fn new(source: String) -> Scanner {
        let chars = source.chars().collect(); 
        
        let mut map = HashMap::default();
        map.insert(String::from("and"),    TokenType::And);
        map.insert(String::from("class"),  TokenType::Class);
        map.insert(String::from("else"),   TokenType::Else);
        map.insert(String::from("false"),  TokenType::False);
        map.insert(String::from("for"),    TokenType::For);
        map.insert(String::from("fun"),    TokenType::Fun);
        map.insert(String::from("if"),     TokenType::If);
        map.insert(String::from("nil"),    TokenType::Nil);
        map.insert(String::from("or"),     TokenType::Or);
        map.insert(String::from("print"),  TokenType::Print);
        map.insert(String::from("return"), TokenType::Return);
        map.insert(String::from("super"),  TokenType::Super);
        map.insert(String::from("this"),   TokenType::This);
        map.insert(String::from("true"),   TokenType::True);
        map.insert(String::from("var"),    TokenType::Var);
        map.insert(String::from("while"),  TokenType::While);
        
        Scanner { source, chars, tokens: Vec::new(), line: 1, start: 0, current: 0, keywords: map }
    }


    /// Scan Tokens.
    ///
    /// Scans until reaches the end of the source and sets the list of tokens.  This method can only be called once
    /// until the source is exhausted, and a new Scanner must be created.
    ///
    /// # Errors
    /// 
    /// Sends Lox an error if an unrecognized character is scanned, or if it reaches the enf of source with an 
    /// unterminated String.
    ///
    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;

            self.scan_token();
        }
        self.tokens.push(token!(TokenType::Eof, "", Object::None, self.line));
    }

    // Scans a single token.
    //
    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen,  Object::None),
            ')' => self.add_token(TokenType::RightParen, Object::None),
            '{' => self.add_token(TokenType::LeftBrace,  Object::None),
            '}' => self.add_token(TokenType::RightBrace, Object::None),
            ',' => self.add_token(TokenType::Comma,      Object::None),
            '.' => self.add_token(TokenType::Dot,        Object::None),
            '-' => self.add_token(TokenType::Minus,      Object::None),
            '+' => self.add_token(TokenType::Plus,       Object::None),
            ';' => self.add_token(TokenType::Semicolon,  Object::None),
            '*' => self.add_token(TokenType::Star,       Object::None),
            
            '!' => self.add_token_cmp('=', TokenType::BangEqual,    TokenType::Bang),
            '=' => self.add_token_cmp('=', TokenType::EqualEqual,   TokenType::Equal),
            '<' => self.add_token_cmp('=', TokenType::LessEqual,    TokenType::Less),
            '>' => self.add_token_cmp('=', TokenType::GreaterEqual, TokenType::Greater),

            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
                else {
                    self.add_token(TokenType::Slash, Object::None)
                }
            }
            
            ' ' | '\r' | '\t' => (),
            
            '\n' => self.line += 1,

            '"' => self.string(),

            _ => {
                if Self::is_digit(c) {
                    self.number();
                }
                else if Self::is_alpha(c) {
                    self.identifier();
                }
                else {
                    Lox::error(self.line, &format!("Unexpected character: {}.", c));
                }
            } 
        }
    }

    // Scans an identifier.  Identifiers must start with an alphabetic character and can contain alphanumeric 
    // values.  If the identifier is contained in the map of keywords, the specific keyword token is returned 
    // instead.
    //
    fn identifier(&mut self) {
        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        } 

        let text: String = self.chars[self.start..self.current].iter().collect();
        let token_type = self.keywords.get(&text).unwrap_or(&TokenType::Identifier);

        self.add_token(*token_type, Object::None);
    }

    // Scans for a Number.  In the source, numbers can look like "123" or "3.14".  In the literal value, however,
    // Lox treats all numbers as floating point values.
    //
    fn number(&mut self) {
        while Self::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && Self::is_digit(self.peek_next()) {
            self.advance();
        }
        
        while Self::is_digit(self.peek()) {
            self.advance();
        }
        
        let text: String = self.chars[self.start..self.current].iter().collect();
        let number: f64 = text.parse().expect("number should parse");

        self.add_token(TokenType::Number, Object::Number(number));
    }

    // Scans for a String until a terminating quotation mark or the end of source.  If end of source, reports
    // an error.
    //
    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        
        if self.is_at_end() {
            Lox::error(self.line, "Unterminated string.");
            return
        }
        self.advance();
        let text: String = self.chars[self.start+1..self.current-1].iter().collect();
        
        self.add_token(TokenType::String, Object::String(text));
    }

    // Return the current character in the source.  If at end, returns 0 instead.
    //
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }  
        self.chars[self.current]
    }

    // Lookahead to next character.  If it is past the end, returns 0 instead.
    //
    fn peek_next(&self) -> char {
        if self.current + 1 > self.chars.len() {
            return '\0'
        }
        self.chars[self.current + 1]
    }
    
    // Determines if character is alphabetic.
    // 
    fn is_alpha(c: char) -> bool {
        (c >= 'a' && c <= 'z') || 
        (c >= 'A' && c <= 'Z') ||
        c  == '_'
    }

    // Determines if character is alphabetic or numeric.
    //
    fn is_alpha_numeric(&self, c: char) -> bool {
        Self::is_alpha(c) || Self::is_digit(c)
    }

    // Determines if character is a digit (numeric).
    //
    fn is_digit(c: char) -> bool {
        c >= '0' && c <= '9'
    }

    // Conditionally adds a token based on the character argument.
    //
    fn add_token_cmp(&mut self, c: char, first: TokenType, second: TokenType) {
        let token_type = if self.match_char(c) { first } else { second };       
        self.add_token(token_type, Object::None);
    }
    
    // Matches a character.  If at end or not matched, simply returns, otherwise the current character is
    // advanced.
    //
    fn match_char(&mut self, expected : char) -> bool {
        if self.is_at_end() { 
            return false
        }
        
        if self.chars[self.current] != expected {
            return false;
        }

        self.current += 1;
        return true;
    }

    // Unconditionally advances current character, returning previous value.
    //
    fn advance(&mut self) -> char {
        let c = self.chars[self.current];
        self.current += 1;
        
        return c;
    }

    // Uses the start and current characters to add a Token to list.
    //
    fn add_token(&mut self, token_type: TokenType, literal: Object) {
       let text: String = self.chars[self.start..self.current].iter().collect();
       
       self.tokens.push(Token::new(token_type, text, literal, self.line));
    }

    // Are we at the end of the source?
    //
    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }
}


#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;
    use crate::token::TokenType;
    use crate::object::Object;
    use crate::GLOBALS;

    // Scans each of the single character tokens, verifying the correct token types are returned, and ends
    // with an EOF.
    //
    #[test]
    fn scan_tokens() {
        let mut scanner = Scanner::new(String::from("(){},.-+;*"));
        
        scanner.scan_tokens();

        assert_eq!(TokenType::LeftParen, scanner.tokens[0].token_type);
        assert_eq!(TokenType::RightParen, scanner.tokens[1].token_type);

        assert_eq!(TokenType::LeftBrace,  scanner.tokens[2].token_type);
        assert_eq!(TokenType::RightBrace, scanner.tokens[3].token_type);

        assert_eq!(TokenType::Comma,  scanner.tokens[4].token_type);
        assert_eq!(TokenType::Dot,    scanner.tokens[5].token_type);
        assert_eq!(TokenType::Minus,  scanner.tokens[6].token_type);
        assert_eq!(TokenType::Plus,   scanner.tokens[7].token_type);

        assert_eq!(TokenType::Semicolon, scanner.tokens[8].token_type);
        assert_eq!(TokenType::Star,      scanner.tokens[9].token_type);

        assert_eq!(TokenType::Eof, scanner.tokens[10].token_type);
    }

    // Scans the two character operators, verifying the correct token types are returned.  Also checks that 
    // the individual characters continue to be scanned properly.  Implicitly checks that whitespace is ignored.
    //
    #[test]
    fn scan_operators() {
        let mut scanner = Scanner::new(String::from("! = < > != == <= >="));
        
        scanner.scan_tokens();

        assert_eq!(TokenType::Bang,   scanner.tokens[0].token_type);
        assert_eq!(TokenType::Equal,  scanner.tokens[1].token_type);
        
        assert_eq!(TokenType::Less,    scanner.tokens[2].token_type);
        assert_eq!(TokenType::Greater, scanner.tokens[3].token_type);
        
        assert_eq!(TokenType::BangEqual,    scanner.tokens[4].token_type);
        assert_eq!(TokenType::EqualEqual,   scanner.tokens[5].token_type);
        assert_eq!(TokenType::LessEqual,    scanner.tokens[6].token_type);
        assert_eq!(TokenType::GreaterEqual, scanner.tokens[7].token_type);

        assert_eq!(TokenType::Eof, scanner.tokens[8].token_type);
    }

    // Tests that a comment is ignored until the end of a line.
    //
    #[test]
    fn scan_comment() {
        let mut scanner = Scanner::new(String::from("// this is a comment"));
        
        scanner.scan_tokens();

        assert_eq!(TokenType::Eof, scanner.tokens[0].token_type);
    }

    // Test that the line counter is increased when scanning an end-of-line character (\n).
    //
    #[test]
    fn scan_newline() {
        let mut scanner = Scanner::new(String::from("test\ntest2"));
        
        scanner.scan_tokens();

        assert_eq!(1, scanner.tokens[0].line);
        assert_eq!(2, scanner.tokens[2].line);
    }

    // Test that quotation mark returns a String token type, with a lexeme including the quotation marks, and 
    // the literal value a String object without them.
    //
    #[test]
    fn scan_string() {
        let mut scanner = Scanner::new(String::from("\"ABC\""));
        
        scanner.scan_tokens();

        let token = &scanner.tokens[0];

        assert_eq!(TokenType::String, token.token_type);
        assert_eq!("\"ABC\"", token.lexeme);
        assert_eq!(Object::String(String::from("ABC")), token.literal);
    }

    // If the end of the file is reached without a terminating quotation mark, an error should be sent to 
    // Lox.
    // 
    #[test]
    fn scan_unterminated_string() {
        let mut scanner = Scanner::new(String::from("\"ABC"));
        
        scanner.scan_tokens();

        let globals = GLOBALS.lock().unwrap();
        assert_eq!(true, globals.had_error);
        assert_eq!("[line 1] Error : Unterminated string.", globals.last_error);
    }

    // Tests that scanning a series of numbers returns a Number object, with the String value in lexeme.
    //
    #[test]
    fn scan_number() {
        let mut scanner = Scanner::new(String::from("123"));
        
        scanner.scan_tokens();

        let token = &scanner.tokens[0];

        assert_eq!(TokenType::Number, token.token_type);
        assert_eq!("123", token.lexeme);
        assert_eq!(Object::Number(123.0), token.literal);
    }

    // If a period is encountered while scanning numbers, it should scan for additional numbers for a decimal
    // value.  Returns a Number with the String value in lexeme.
    //
    #[test]
    fn scan_number_decimal() {
        var mut scanner = Scanner::new(String::from("3.14"));
        
        scanner.scan_tokens();

        let token = &scanner.tokens[0];

        assert_eq!(TokenType::Number, token.token_type);
        assert_eq!("3.14", token.lexeme);
        assert_eq!(Object::Number(3.14), token.literal);
    }

    // Test scanning an identifier.  The lexeme should contain the name, and the literal value should be None.
    //
    #[test]
    fn scan_identifier() {
        let mut scanner = Scanner::new(String::from("test"));
        
        scanner.scan_tokens();

        let token = &scanner.tokens[0];

        assert_eq!(TokenType::Identifier, token.token_type);
        assert_eq!("test", token.lexeme);
        assert_eq!(Object::None, token.literal);
    }

    // Tests that all of Lox's keywords are properly distinguished from identifiers.
    //
    #[test]
    fn scan_keywords() {
        let mut scanner = Scanner::new(String::from("and class else false for fun if nil or print return super this true var while"));

        scanner.scan_tokens();

        assert_eq!(TokenType::And,    scanner.tokens[0].token_type);
        assert_eq!(TokenType::Class,  scanner.tokens[1].token_type);
        assert_eq!(TokenType::Else,   scanner.tokens[2].token_type);
        assert_eq!(TokenType::False,  scanner.tokens[3].token_type);
        assert_eq!(TokenType::For,    scanner.tokens[4].token_type);
        assert_eq!(TokenType::Fun,    scanner.tokens[5].token_type);
        assert_eq!(TokenType::If,     scanner.tokens[6].token_type);
        assert_eq!(TokenType::Nil,    scanner.tokens[7].token_type);
        assert_eq!(TokenType::Or,     scanner.tokens[8].token_type);
        assert_eq!(TokenType::Print,  scanner.tokens[9].token_type);
        assert_eq!(TokenType::Return, scanner.tokens[10].token_type);
        assert_eq!(TokenType::Super,  scanner.tokens[11].token_type);
        assert_eq!(TokenType::This,   scanner.tokens[12].token_type);
        assert_eq!(TokenType::True,   scanner.tokens[13].token_type);
        assert_eq!(TokenType::Var,    scanner.tokens[14].token_type);
        assert_eq!(TokenType::While,  scanner.tokens[15].token_type);
        assert_eq!(TokenType::Eof,    scanner.tokens[16].token_type);
    }

    // Should report an error to Lox if an unexpected character is scanned.
    ///
    #[test]
    fn scan_unexpected_character() {
        let mut scanner = Scanner::new(String::from("%"));
        
        scanner.scan_tokens();

        let globals = GLOBALS.lock().unwrap();
        assert_eq!(true, globals.had_error);
        assert_eq!("[line 1] Error : Unexpected character: %.", globals.last_error);
    }


    #[test]
    fn test_unsafe() {
        let x = 5;
        let raw = &x as *const i32;

        let points_at = unsafe { *raw };

        println!("raw points at {}", points_at);
    }
}