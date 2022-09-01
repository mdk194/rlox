use crate::chunk::Chunk;
use crate::strings::IString;

#[allow(dead_code)]
#[derive(Default, PartialEq)]
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

pub type IFunction = usize;

pub struct Functions {
    fs: Vec<Function>,
}

impl Functions {
    pub fn new() -> Self {
        Self { fs: Vec::new() }
    }

    pub fn lookup(&self, id: IFunction) -> &Function {
        &self.fs[id]
    }

    pub fn add(&mut self, f: Function) -> IFunction {
        self.fs.push(f);
        self.fs.len() - 1
    }
}
