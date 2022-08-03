mod chunk;
mod disassembler;
mod vm;

use chunk::{Chunk, OpCode};
use disassembler::Disassembler;
use vm::{InterpretResult, VM};

fn main() -> InterpretResult {
    let mut chunk = Chunk::new();

    let i = chunk.add_constant(1.2);
    chunk.write(OpCode::Constant as u8, 123);
    chunk.write(i as u8, 123);

    let i = chunk.add_constant(3.4);
    chunk.write(OpCode::Constant as u8, 123);
    chunk.write(i as u8, 123);

    chunk.write(OpCode::Add as u8, 123);

    let i = chunk.add_constant(5.6);
    chunk.write(OpCode::Constant as u8, 123);
    chunk.write(i as u8, 123);

    chunk.write(OpCode::Divide as u8, 123);
    chunk.write(OpCode::Negate as u8, 123);
    chunk.write(OpCode::Return as u8, 123);

    let disassembler = Disassembler::new(&chunk);
    let mut vm = VM::new(&chunk, Some(&disassembler));
    vm.run()
}
