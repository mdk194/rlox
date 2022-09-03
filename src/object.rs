use crate::chunk::Chunk;
use crate::strings::IString;
use crate::value::Value;
use std::time::{self, SystemTime};

#[allow(dead_code)]
#[derive(Default, PartialEq, Eq)]
pub enum FunctionType {
    Function,
    #[default]
    Script,
}

#[allow(dead_code)]
#[derive(Default)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<IString>,
}

impl Function {
    pub fn new(name: Option<IString>) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::default(),
            name,
        }
    }
}

pub type IObject = usize;

pub struct Objects<O> {
    v: Vec<O>,
}

impl<O> Objects<O> {
    pub fn new() -> Self {
        Self { v: Vec::new() }
    }

    pub fn lookup(&self, id: IObject) -> &O {
        &self.v[id]
    }

    pub fn add(&mut self, o: O) -> IObject {
        self.v.push(o);
        self.v.len() - 1
    }
}

pub type NativeFn = fn(&[Value]) -> Value;

pub fn clock(_args: &[Value]) -> Value {
    let start = SystemTime::now();
    let time = start
        .duration_since(time::UNIX_EPOCH)
        .expect("duration since epoch");
    Value::Number(time.as_secs_f64())
}

pub struct Closure {
    pub ifunction: IObject,
}

impl Closure {
    pub fn new(ifunction: IObject) -> Self {
        Self { ifunction }
    }
}