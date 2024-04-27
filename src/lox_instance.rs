use std::collections::HashMap;

use ahash::{AHasher, RandomState};

use std::cell::RefCell;
use std::rc::Rc;

use crate::error::{RuntimeError, runtime_error};

use crate::token::Token;
use crate::object::Object;
use crate::lox_class::LoxClass;

#[derive(PartialEq, Debug)]
pub struct LoxInstance {
    class: Rc<LoxClass>,
    fields: HashMap<String, Object, RandomState>,
}

impl LoxInstance {
    /// Factory method to create a new LoxInstance!
    ///
    pub fn new(class: Rc<LoxClass>) -> LoxInstance {
        LoxInstance { class, fields: HashMap::default() }
    }

    /// The class's to_string().
    ///
    pub fn to_string(&self) -> String {
        format!("{} instance", self.class.name)
    }

    /// Gets a property value from the instance.
    ///
    /// # Errors
    /// 
    /// Returns a runtime error if property is undefined.
    ///
    pub fn get(&self, name: &Token, instance: Rc<RefCell<LoxInstance>>) -> Result<Object, RuntimeError> {
        if self.fields.contains_key(&name.lexeme) {
            return Ok(self.fields.get(&name.lexeme).expect("DON'T PANIC").copy());
        }

        let method = self.class.find_method(&name.lexeme);
        if let Some(function) = method {
            // TODO: Yeah, this isn't going to work :(
            let function = function.bind(instance); 
            let function = Rc::new(function);
            let function = Object::Function(function); 

            return Ok(function);
        }
        Err(runtime_error!(name, format!("Undefined property '{}'.", name.lexeme)))  
    }

    /// Sets a property value.
    ///
    pub fn set(&mut self, name: &Token, value: Object) {
        self.fields.insert(name.to_lexeme(), value);
    }
}

#[cfg(test)]
mod tests {
    use crate::smart_pointer::{copy_ref, new_cell_ref};
    
    use crate::object::Object;
    use crate::token::{Token, TokenType, token};

    use crate::lox_class::LoxClass;
    use crate::lox_instance::LoxInstance;
    
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::collections::HashMap;

    use ahash::{AHasher, RandomState};

    // Tests Lox instance to_string().
    //
    #[test]
    fn lox_instance_new() {
        // Arrange
        let class = LoxClass::new("Bagel", None, HashMap::default());
        let class = Rc::new(class);

        let uut = LoxInstance::new(class);

        // Assert
        assert_eq!("Bagel instance", uut.to_string());
    }

    // Test Lox instance get field.
    //
    #[test]
    fn lox_instance_get() {
        // Arrange
        let class = LoxClass::new("Bagel", None, HashMap::default());
        let class = Rc::new(class);

        let mut uut = LoxInstance::new(class);
        uut.fields.insert("ABC".to_owned(), Object::Number(123.0));
 
        let token = &token!(TokenType::String, "ABC", Object::None, 1);

        let uut = new_cell_ref!(uut);
        let instance = copy_ref!(&uut);

        // Act
        let result = uut.borrow().get(token, instance).expect("REASON");

        // Assert
        assert_eq!(Object::Number(123.0), result);
    }

    // Test Lox instance get field undefined.
    //
    #[test]
    fn lox_instance_get_undefined() {
        // Arrange
        let class = LoxClass::new("Bagel", None, HashMap::default());
        let class = Rc::new(class);

        let uut = LoxInstance::new(class);
        let uut = new_cell_ref!(uut);
    
        let token = &token!(TokenType::String, "ABC", Object::None, 1);

        let instance = copy_ref!(&uut);

        // Act
        let result = uut.borrow().get(token, instance);

        // Assert
        if let Err(err) = result {
            assert_eq!("Undefined property 'ABC'.", err.message);
        } 
        else {
            panic!("should have error");
        }
    }

    // Test Lox instance get field.
    //
    #[test]
    fn lox_instance_set() {
        let class = LoxClass::new("Bagel", None, HashMap::default());
        let class = Rc::new(class);

        let uut = LoxInstance::new(class);
        let uut = new_cell_ref!(uut);
    
        let token = &token!(TokenType::String, "ABC", Object::None, 1);
        
        uut.borrow_mut().set(token, Object::Number(123.0));

        let instance = Rc::clone(&uut);

        assert_eq!(Object::Number(123.0), uut.borrow().get(token, instance).expect("REASON"));
    }

    // Test Lox instance clones.
    //
    #[test]
    fn lox_instance_clones() {
        // Arrange
        let class = LoxClass::new("Bagel", None, HashMap::default());
        let class = Rc::new(class);

        let uut = LoxInstance::new(class);
        let uut = new_cell_ref!(uut);
    
        let token = &token!(TokenType::String, "ABC", Object::None, 1);
        
        let clone = uut.clone();
        clone.borrow_mut().set(token, Object::Number(123.0));

        let instance = copy_ref!(&uut);

        // Act
        let result = uut.borrow().get(token, instance).expect("REASON");

        // Assert
        assert_eq!(Object::Number(123.0), result);
    }
}



