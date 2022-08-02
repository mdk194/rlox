mod chunk;

fn main() {
    let mut chunk = chunk::Chunk::new();

    let i = chunk.add_constant(1.2);
    chunk.write(chunk::OpCode::Constant(i), 123);

    chunk.write(chunk::OpCode::Return, 123);

    let disassembler = chunk::Disassembler::new(chunk);
    disassembler.disassemble("test chunk");
}
