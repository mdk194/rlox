use crate::function::{Functions, IFunction};
use crate::strings::{IString, Interner};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(IString),
    Function(IFunction),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Nil | Value::Bool(false))
    }

    pub fn as_string(&self, strings: &Interner, functions: &Functions) -> String {
        match self {
            Value::Nil => "nil".to_owned(),
            Value::Bool(v) => format!("{}", v),
            Value::Number(v) => format!("{}", v),
            Value::String(i) => strings.lookup(*i).to_owned(),
            Value::Function(i) => {
                if let Some(fn_name) = functions.lookup(*i).name {
                    return format!("<fn {}>", strings.lookup(fn_name));
                }
                "<script>".to_owned()
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(s), Value::Bool(o)) => s == o,
            (Value::Nil, _) => true,
            (Value::Number(s), Value::Number(o)) => s == o,
            (Value::String(s), Value::String(o)) => s == o,
            _ => false,
        }
    }
}
