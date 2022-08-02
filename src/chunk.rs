#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Return,
    Constant(usize),
}

type Value = f64;

#[derive(Debug, Default, Clone)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write(&mut self, op: OpCode, line_number: usize) -> usize {
        self.code.push(op);
        self.lines.push(line_number);
        self.code.len() - 1
    }

    pub fn add_constant(&mut self, v: Value) -> usize {
        self.constants.push(v);
        self.constants.len() - 1
    }
}

pub struct Disassembler {
    chunk: Chunk,
}

impl Disassembler {
    pub fn new(chunk: Chunk) -> Self {
        Disassembler { chunk }
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        for (offset, op) in self.chunk.code.iter().enumerate() {
            self.instruction(op, offset);
        }
    }

    pub fn instruction(&self, op: &OpCode, offset: usize) {
        print!("{:04} ", offset);
        let line = self.chunk.lines[offset];
        if offset > 0 && line == self.chunk.lines[offset - 1] {
            print!("    | ");
        } else {
            print!("{:4} ", line);
        }
        match op {
            OpCode::Return => println!("OP_RETURN"),
            OpCode::Constant(index) => self.constant_instruction("OP_CONSTANT", *index),
        }
    }

    fn constant_instruction(&self, instruction: &str, index: usize) {
        let value = self.chunk.constants[index];
        println!("{:<16} {:4} {}", instruction, index, value);
    }
}
