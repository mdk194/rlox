use crate::chunk::Chunk;
use crate::memory::HeapId;
use crate::value::Value;
use std::time::{self, SystemTime};

#[allow(dead_code)]
pub struct Object {
    pub data: ObjectData,
    is_marked: bool,
}

impl Object {
    pub fn new(data: ObjectData) -> Self {
        Self {
            data,
            is_marked: false,
        }
    }
}

pub enum ObjectData {
    String(String),
    Function(Function),
    Closure(Closure),
    UpValue(UpValue),
}

#[allow(dead_code)]
#[derive(Default, PartialEq, Eq)]
pub enum FunctionType {
    Function,
    #[default]
    Script,
}

#[derive(Clone, Copy)]
pub struct FnUpValue {
    pub index: u8,
    pub is_local: bool,
}

impl FnUpValue {
    pub fn new(index: u8, is_local: bool) -> Self {
        Self { index, is_local }
    }
}

#[allow(dead_code)]
#[derive(Default)]
pub struct Function {
    pub arity: usize,
    pub upvalues: Vec<FnUpValue>,
    pub chunk: Chunk,
    pub name: Option<HeapId>,
}

impl Function {
    pub fn new(name: Option<HeapId>) -> Self {
        Self {
            arity: 0,
            upvalues: Vec::new(),
            chunk: Chunk::default(),
            name,
        }
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
    pub ifunction: HeapId,
    pub upvalues: Vec<HeapId>,
}

impl Closure {
    pub fn new(ifunction: HeapId) -> Self {
        Self {
            ifunction,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Clone)]
pub struct UpValue {
    pub location: usize,
    pub next: Option<HeapId>,
    pub closed: Option<Value>,
}

impl UpValue {
    pub fn new(location: usize) -> Self {
        Self {
            location,
            next: None,
            closed: None,
        }
    }
}

#[macro_export]
macro_rules! cast {
    ($target: expr, $type:ident) => {
        if let ObjectData::$type(a) = $target {
            a
        } else {
            panic!("unable to cast to {}", stringify!($type));
        }
    };
}
