mod chunk;
use chunk::{Chunk, OpCode, Disassembler};

fn main() {
    let mut chunk = Chunk::new();

    let i = chunk.add_constant(1.2);
    chunk.write(OpCode::Constant as u8, 123);
    chunk.write(i as u8, 123);

    chunk.write(OpCode::Return as u8, 123);

    let disassembler = Disassembler::new(chunk);
    disassembler.disassemble("test chunk");
}
