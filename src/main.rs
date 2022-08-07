mod chunk;
mod disassembler;
mod vm;
mod scanner;
mod compiler;

use std::io::{BufRead, Write};

use chunk::{Chunk, OpCode};
use disassembler::Disassembler;
use vm::{VM, VMError};

fn main() {
    let chunk = Chunk::new();
    let disassembler = Disassembler::new(&chunk);
    let mut vm = VM::new(&chunk, Some(&disassembler));

    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => repl(&mut vm),
        2 => run_file(&mut vm, &args[1]),
        _ => {
            eprintln!("usage: rlox [path]");
            std::process::exit(64);
        },
    }
}

fn repl(vm: &mut VM) {
    for line in std::io::stdin().lock().lines() {
        print!("> ");
        std::io::stdout().flush().expect("Failed writing to stdout.");
        vm.interpret(&line.unwrap()).unwrap_or(());
    }
}

fn run_file(vm: &mut VM, path: &str) -> ! {
    let source = std::fs::read_to_string(path).unwrap_or_else(|_| {
        eprintln!("Could not read input file: {}", path);
        std::process::exit(74);
    });

    let exit = match vm.interpret(&source) {
        Ok(()) => 0,
        Err(VMError::CompileError) => 65,
        Err(VMError::RuntimeError) => 70,
    };

    std::process::exit(exit);
}
