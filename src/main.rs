mod chunk;
mod compiler;
mod disassembler;
mod function;
mod scanner;
mod strings;
mod value;
mod vm;

use std::io::{self, Write};

use chunk::{Chunk, OpCode};
use typed_arena::Arena;
use vm::{VMError, VM};

fn main() {
    let arena = Arena::new();
    let mut vm = VM::new(&arena);

    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => repl(&mut vm),
        2 => run_file(&mut vm, &args[1]),
        _ => {
            eprintln!("usage: rlox [path]");
            std::process::exit(64);
        }
    }
}

fn repl(vm: &mut VM) {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .expect("Failed to read line");
        if line.is_empty() {
            break;
        }
        vm.interpret(&line).ok();
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
