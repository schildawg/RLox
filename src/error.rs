use crate::token::Token;
use crate::object::Object;

/// Parse Error.
///
/// Captures the a message for an error that occurs during the parsing pass.
///
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

/// Runtime Error.
///
/// Captures error that occurs during the interpret phase.  Contains message, and token information such as line number.
///
/// # Return Value
/// 
/// Runtime Error can also be used as a return value from a function.  If this value is not None, then the rest of the 
/// fields are ignored, and this struct is used to unwind the stack to easily return a value.
//
#[derive(Debug)]
pub struct RuntimeError {
    pub token: Token,
    pub message: String,
    pub return_value: Object,
}

/// runtime_error!
///
/// Convenience macro to create a Runtime Error.  Clones the token, creates a String, and sets return value to None.
///
macro_rules! runtime_error {
    ($a:expr,$b:expr) => {
        RuntimeError { token: $a.copy(), message: String::from($b), return_value: Object::None }
    };
}
pub (crate) use runtime_error;