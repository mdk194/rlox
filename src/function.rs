use crate::chunk::Chunk;
use crate::strings::IString;

#[allow(dead_code)]
#[derive(Default)]
pub enum FunctionType {
    Function,
    #[default]
    Script,
}

#[allow(dead_code)]
#[derive(Default)]
pub struct Function {
    arity: u8,
    pub chunk: Chunk,
    name: IString,
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
