use std::collections::HashMap;

use std::cell::RefCell;
use std::rc::Rc;
use crate::smart_pointer::{copy_ref, new_cell_ref};

use ahash::{AHasher, RandomState};

use crate::object::Object;
use crate::token::Token;

use crate::error::{runtime_error, RuntimeError};

pub type EnvironmentRef = Rc<RefCell<Environment>>;

/// Environment to hold map of variables and their values.  Each scope should have its own Environment, with an enclosing Environment
/// for the containing scopes.     
/// 
/// The enclosing environments creates a "cactus" structure with multiple owners needing mutable access, so we are using the
/// reference counting cell pattern to implement.
///
#[derive(PartialEq, Debug)]
pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>,
    pub values: HashMap<String, Object, RandomState>
}

impl Environment {
    /// Factory method to create a new Environment.  
    ///
    pub fn new() -> Environment {
        Environment { enclosing: None, values: HashMap::default()}
    }

    /// Defines a variable at the local scope.
    ///
    pub fn define(&mut self, name: &str, value: Object) {
        self.values.insert(String::from(name), value);
    }

    /// Assigns a value to an existing variable.  Walks up the enclosing Environments until it finds the variable defined.
    ///
    /// # Errors
    ///
    /// Returns a runtime error if reaches the top-level environment (enclosing is None) and does not find the variable.
    ///
    pub fn assign(&mut self, name: &Token, value: Object) -> Result<Object, RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.to_lexeme(), value);
            return Ok(Object::None);
        }

        if self.enclosing != None {
            let _ = self.enclosing.as_mut().expect("").borrow_mut().assign(name, value);
            return Ok(Object::None);
        }
        //panic!("NOT DEFINED!!!");
        Err(runtime_error!(name, format!("Undefined variable '{}'.", name.lexeme)))
    }

    /// Gets a variable from the local scope, or looks in the enclosing environment.
    ///
    /// # Errors
    ///
    /// Returns a runtime error if reaches the top-level environment and does not find the variable.
    ///
    pub fn get(&self, name: &Token) -> Result<Object, RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            return Ok(self.values[&name.lexeme].copy());
        }

        if let Some(env) = &self.enclosing {
            return env.borrow().get(name);
        }
        Err(runtime_error!(name, format!("Undefined variable '{}'.", name.lexeme)))
    }

    /// Gets a variable from the local scope, or looks in the enclosing environment.  The parameter "distance" is how many
    /// hops up ancestor environments to reach the correct scope.
    ///
    pub fn get_at(self_ref: EnvironmentRef, distance: usize, name: &str) -> Object {
        Environment::ancestor(self_ref, distance).borrow().values.get(name).unwrap_or(&Object::None).copy()
    }

    /// Assigns a value to an existing variable.  The parameter "distance" is how many hops up ancestor environments to reach
    ///  the correct scope.
    ///
    pub fn assign_at(self_ref: EnvironmentRef, distance: usize, name: &Token, value: Object) {
        Environment::ancestor(self_ref, distance).borrow_mut().values.insert(name.to_lexeme(), value);
    }

    // Hops up the ancestor chain by a "distance"
    //
    fn ancestor(self_ref: EnvironmentRef, distance: usize) -> Rc<RefCell<Environment>> {
        let mut environment = self_ref;

        for _ in 0..distance {
            let env = copy_ref!(&environment);
            let env = env.borrow_mut();
            let enclosing = env.enclosing.as_ref().expect("REASON");
            environment = copy_ref!(enclosing);
        }
        environment
    }

}

#[cfg(test)]
mod tests {
    use crate::environment::Environment;
    use crate::object::Object;
    use crate::token::{TokenType, Token, token};

    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::environment::{copy_ref, new_cell_ref};

    struct Holder {
        env: Rc<RefCell<Environment>>,
    }

    // Tests creating an environment, defining a variable and retrieving it.
    //
    #[test]
    fn environment_test() {
        let holder = Holder { env: new_cell_ref!(Environment::new())};
        
        holder.env.borrow_mut().define("test", Object::Number(1.0));
        
        let result = holder.env.borrow_mut().get(&token!(TokenType::Identifier, "test", Object::None, 0)).expect("REASON");
        assert_eq!(Object::Number(1.0), result);
    }

    // A variable defined in an environment should be able to be accessed from a lower scope.
    //
    #[test]
    fn environment_two_deep() { 
        let globals = new_cell_ref!(Environment::new());
        globals.borrow_mut().define("test", Object::Number(1.0));

        let env = copy_ref!(&globals);

        let mut function_env = Environment::new();
        function_env.enclosing = Some(env);

        let mut holder = Holder { env: new_cell_ref!(function_env) };
        
        holder.env.borrow_mut().define("test", Object::Number(2.0));

        let token = token!(TokenType::Identifier, "test", Object::None, 0);
        assert_eq!(Object::Number(1.0), globals.borrow_mut().get(&token).expect("REASON"));
        assert_eq!(Object::Number(2.0), holder.env.borrow_mut().get(&token).expect("REASON"));
    }

    // Too often something goes wrong at 3 :)  So we are checking that a variable can be accessed from a scope three
    // levels deep.
    #[test]
    fn environment_three_deep() {        
        let globals = new_cell_ref!(Environment::new());
        globals.borrow_mut().define("test", Object::Number(1.0));

        let env = copy_ref!(&globals);

        let mut function_env = Environment::new();
        function_env.enclosing = Some(env);

        let mut inner = Environment::new();
        inner.enclosing = Some(new_cell_ref!(function_env));

        let holder = Holder { env: new_cell_ref!(inner), };
        
        assert_eq!(Object::Number(1.0), holder.env.borrow_mut().get(&token!(TokenType::Identifier, "test", Object::None, 0)).expect("REASON"));
    }

    // #[test]
    // fn environment_hop_three_deep() {        
    //     let globals = new_cell_ref!(Environment::new());
    //     globals.borrow_mut().define("test", Object::Number(1.0));

    //     let env = copy_ref!(&globals);

    //     let mut function_env = Environment::new();
    //     function_env.enclosing = Some(env);

    //     let mut inner = Environment::new();
    //     inner.enclosing = Some(new_cell_ref!(function_env));

    //     let holder = Holder { env: new_cell_ref!(inner), };
        
    //     assert_eq!(Object::Number(1.0), holder.env.borrow_mut().get_at(2, "test"));
    // }


    // Assigning a value to a defined variable should change the value returned from get().
    //
    #[test]
    fn environment_assign() {
        let globals = new_cell_ref!(Environment::new());
        globals.borrow_mut().define("test", Object::Number(1.0));

        let env = copy_ref!(&globals);

        let holder = Holder { env };

        holder.env.borrow_mut().assign(&token!(TokenType::Identifier, "test", Object::None, 0), Object::Number(3.14));       

        let result = holder.env.borrow().get(&token!(TokenType::Identifier, "test", Object::None, 0)).expect("REASON");
        assert_eq!(Object::Number(3.14), result);
    }

    // Attempting to assign a value to an identifier with no variable defined should return a runtime error.
    //
    #[test]
    fn environment_assign_not_defined() {
        let globals = new_cell_ref!(Environment::new());

        let env = copy_ref!(&globals);
        let holder = Holder { env };

        let result = holder.env.borrow_mut().assign(&token!(TokenType::Identifier, "test", Object::None, 0), Object::Number(3.14));       

        match result {
            Err(err) => { 
                assert_eq!("Undefined variable 'test'.", err.message);
            }
            _ => { panic!("should fail"); }
        }
    }

    // Attempting to get a value that is not defined should return a runtime error.
    //
    #[test]
    fn environment_get_not_defined() {
        let globals = new_cell_ref!(Environment::new());

        let env = copy_ref!(&globals);
        let holder = Holder { env };      

        //let result = holder.env.borrow().get(&token!(TokenType::Identifier, "test", Object::None, 0));

        // TODO: FIX ME.
        // match result {
        //     Err(err) => { 
        //         assert_eq!("Undefined variable 'test'.", err.message);
        //     }
        //     _ => { panic!("should fail"); }
        // }
    }

    // Assigning a value to a variable defined in a higher scope should change that value.
    //
    #[test]
    fn environment_cascade_assign() {        
        let globals = new_cell_ref!(Environment::new());
        globals.borrow_mut().define("test", Object::Number(1.0));

        let env = copy_ref!(&globals);

        let mut function_env = Environment::new();
        function_env.enclosing = Some(env);

        let mut inner = Environment::new();
        inner.enclosing = Some(new_cell_ref!(function_env));

        let holder = Holder { env: new_cell_ref!(inner) };
      
        holder.env.borrow_mut().assign(&token!(TokenType::Identifier, "test", Object::None, 0), Object::Number(3.14));   

        assert_eq!(Object::Number(3.14), holder.env.borrow_mut().get(&token!(TokenType::Identifier, "test", Object::None, 0)).expect("REASON"));
    }
}
