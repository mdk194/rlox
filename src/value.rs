use crate::object::{IObject, NativeFn};
use crate::strings::IString;

#[derive(Clone, Copy)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(IString),
    Function(IObject),
    NativeFunction(NativeFn),
    Closure(IObject),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Nil | Value::Bool(false))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(s), Value::Bool(o)) => s == o,
            (Value::Nil, _) => true,
            (Value::Number(s), Value::Number(o)) => s == o,
            (Value::String(s), Value::String(o)) => s == o,
            (Value::Function(s), Value::Function(o)) => s == o,
            (Value::NativeFunction(s), Value::NativeFunction(o)) => std::ptr::eq(s, o),
            (Value::Closure(s), Value::Closure(o)) => s == o,
            _ => false,
        }
    }
}
