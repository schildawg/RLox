use crate::smart_pointer::{copy_ref, new_cell_ref};
use crate::object::{ClassRef,Object};
use crate::lox_function::{LoxCallable, LoxFunction};

use crate::interpreter::Interpreter;
use crate::lox_instance::LoxInstance;

use std::cell::RefCell;
use std::rc::Rc;

use std::collections::HashMap;

use ahash::{AHasher, RandomState};

#[derive(PartialEq, Debug)]
pub struct LoxClass {
    pub name: String,
    methods: HashMap<String, LoxFunction, RandomState>,
    superclass: Option<Rc<LoxClass>>,
}

impl LoxClass {
    /// Factory method to create a new LoxClass!
    ///
    pub fn new(name: &str, superclass: Option<Rc<LoxClass>>, methods: HashMap<String, LoxFunction, RandomState>) -> LoxClass {
        LoxClass { name: name.to_owned(), superclass, methods }
    }

    /// The class's to_string().
    ///
    fn to_string(&self) -> String {
        format!("{}", self.name)
    }

    /// Finds a method!
    ///
    pub fn find_method(&self, name: &str) -> Option<&LoxFunction> {
        if self.methods.get(name) != None {
            return self.methods.get(name);
        }
        
        match &self.superclass {
            Some(superclass) => {
                return superclass.find_method(name);
            }
            None => {
                return None;
            }
        }
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> usize {
        let initializer = self.find_method("init");
        if let Some(function) = initializer {
            return function.arity();
        }
        return 0;
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Object>, class: Option<Rc<LoxClass>>) -> Object {
        let instance = new_cell_ref!(LoxInstance::new(copy_ref!(&class.unwrap())));

        let initializer = self.find_method("init");
        if let Some(function) = initializer {
            function.bind(copy_ref!(&instance)).call(interpreter, arguments, None);
        }

        Object::Instance(instance)
    } 

    // The class's to_string().
    //
    fn to_string(&self) -> String {
        format!("{}", self.name)
    }
}

#[cfg(test)]
mod tests {
    use crate::lox_class::LoxClass;
    use std::collections::HashMap;

    use ahash::{AHasher, RandomState};
    
    // Tests Lox class to_string().
    //
    #[test]
    fn lox_class_new() {
        let uut = LoxClass::new("Breakfast", None, HashMap::default());

        assert_eq!("Breakfast", uut.to_string());
    }
}