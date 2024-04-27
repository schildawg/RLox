use crate::smart_pointer::copy_ref;
use crate::error::RuntimeError;

use crate::interpreter::Interpreter;
use crate::interpreter::Clock;

use crate::lox_function::{LoxCallable,LoxFunction};
use crate::lox_class::LoxClass;
use crate::lox_instance::LoxInstance;

use std::cell::RefCell;
use std::rc::Rc;

pub type FunctionRef = Rc<LoxFunction>;
pub type ClassRef    = Rc<LoxClass>;
pub type InstanceRef = Rc<RefCell<LoxInstance>>;

/// Object.  
///
/// Enumeration of all the types of values that can be used in Lox.
///
#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    String(String),
    Number(f64),
    Boolean(bool),
    Function(FunctionRef),
    
    Class(ClassRef),
    Instance(InstanceRef),

    ClockFunction(Clock),
    None,
}

impl Object {
    /// Converts Object to a String.
    ///
    pub fn to_string(&self) -> String {
        match &self {
            Object::String(string) => String::from(string),
            Object::Number(number) => format!("{}", number),
            Object::Boolean(value) => if *value { String::from("true") } else { String::from("false") }
            Object::Class(klass) => format!("{}", klass.to_string()),
            Object::Instance(instance) => format!("{}", instance.borrow().to_string()),
            
            _ => String::from("nil"),
        }
    }

    pub fn obj_type(&self) -> String {
        match &self {
            Object::String(_) => String::from("String"),
            Object::Number(_) =>  String::from("Number"),
            Object::Boolean(_) =>  String::from("Boolean"),
            Object::Class(_) => String::from("Class"),
            Object::Instance(_) =>  String::from("Instance"),
            Object::Function(_) =>  String::from("Function"),
            Object::None =>  String::from("None"),
            _ => String::from("NONE"),
        }
    }

    /// Converts any value into a boolean.  
    ///
    /// Object::Boolean(true) is true.
    /// Object::Boolean(false) is false.
    /// Object::None is false.
    /// Everything else is true.
    ///
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::None => false,
            Object::Boolean(value) => *value,
            _ => true,
        }
    }

    /// Converts Object to 64 bit number.  Caller should check is_number() first.  If not a number, panics.
    ///
    pub fn to_number(&self) -> f64 {
        match self {
            Object::Number(number) => *number,
            _ => panic!("invalid cast!"),
        }
    }

    /// Returns true if a number, false otherwise.
    ///
    pub fn is_number(&self) -> bool {
        match self {
            Object::Number(_number) => true,
            _ => false,
        }
    }

    /// Returns true if a string, false otherwise.
    ///
    pub fn is_string(&self) -> bool {
        match self {
            Object::String(_value) => true,
            _ => false,
        }
    }

    /// Returns true if a function, false otherwise.
    ///
    pub fn is_function(&self) -> bool {
        match self {
            Object::ClockFunction(_callable) => true,
            Object::Function(_function) => true,
            _ => false,
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Object>, _class: Option<Rc<LoxClass>>) -> Result<Object, RuntimeError> {
        match self {
            Object::ClockFunction(callable) => {
                return Ok(callable.call(interpreter, arguments, None));
            }
            Object::Function(function) => {
                return Ok(function.call(interpreter, arguments, None));
            }
            Object::Class(class) => {
                return Ok(class.call(interpreter, arguments, Some(copy_ref!(class))));
            }
            _ => {
                panic!("not possible");
            }
        }
    }

    /// Returns a copy of Object.  All complex objects are behind references, so should not cause performance issue.s  
    ///
    pub fn copy(&self) -> Object {
        self.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::object::Object;

    // Yeah, this is weird, but Object::String still needs to be converted to String.
    //
    #[test]
   fn object_string_to_string() {
       let string = Object::String(String::from("ABC"));

       assert_eq!("ABC", string.to_string());
   }

    // Numbers without decimal should be displayed without decimal.
    //
    #[test]
   fn object_integer_to_string() {
       let number = Object::Number(1.0);

       assert_eq!("1", number.to_string());
   }

    // Numbers with decimal should be displayed with decimal.
    //
    #[test]
   fn object_integer_to_stringnumber_to_string() {
       let number = Object::Number(3.14);

       assert_eq!("3.14", number.to_string());
   }

    // Tests that false is converted to string properly.
    //
    #[test]
   fn object_false_to_string() {
       let object = Object::Boolean(false);

       assert_eq!("false", object.to_string());
   }

    // Tests that true is converted to string properly.
    //
    #[test]
   fn object_true_to_string() {
       let object = Object::Boolean(true);

       assert_eq!("true", object.to_string());
   }

    // Object::None should be displayed as "nil".
    //
    #[test]
   fn object_none_to_string() {
       let object = Object::None;

       assert_eq!("nil", object.to_string());
   }

    #[test]
   fn object_function_to_string() {
       // TODO: write me!
   }

   #[test]
   fn object_clock_to_string() {
       // TODO: write me!
   }
}


