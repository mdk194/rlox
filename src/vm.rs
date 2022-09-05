use rustc_hash::FxHashMap;

#[allow(unused_imports)]
use crate::{
    cast,
    compiler::Parser,
    disassembler::Disassembler,
    memory::{Heap, HeapId},
    object::{clock, Closure, FnUpValue, Function, NativeFn, ObjectData, UpValue},
    value::Value,
    Chunk, OpCode,
};

struct CallFrame {
    iclosure: HeapId,
    ifunction: HeapId,
    ip: usize,
    slot: usize,
}

impl CallFrame {
    fn new(iclosure: HeapId, ifunction: HeapId, slot: usize) -> Self {
        Self {
            iclosure,
            ifunction,
            ip: 0,
            slot,
        }
    }
}

pub struct VM {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    heap: Heap,
    globals: FxHashMap<HeapId, Value>,
    open_upvalue: Option<HeapId>,
}

pub enum VMError {
    CompileError,
    RuntimeError,
}

pub type InterpretResult = Result<(), VMError>;

impl VM {
    const FRAME_MAX: usize = 64;
    const STACK_MAX: usize = VM::FRAME_MAX * (std::u8::MAX as usize + 1);

    pub fn new(heap: Heap) -> Self {
        let mut vm = VM {
            frames: Vec::with_capacity(VM::FRAME_MAX),
            stack: Vec::with_capacity(VM::STACK_MAX),
            heap,
            globals: FxHashMap::default(),
            open_upvalue: None,
        };
        vm.define_native("clock", clock);
        vm
    }

    fn define_native(&mut self, name: &str, f: NativeFn) {
        let istring = self.heap.intern(name);
        self.globals.insert(istring, Value::NativeFunction(f));
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn current_closure(&self) -> &Closure {
        let iclosure = self.current_frame().iclosure;
        cast!(self.heap.lookup(iclosure), Closure)
    }

    fn current_chunk(&self) -> &Chunk {
        let ifunction = self.current_frame().ifunction;
        let f = cast!(self.heap.lookup(ifunction), Function);
        &f.chunk
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let p = Parser::new(source, &mut self.heap);

        match p.compile() {
            None => Err(VMError::CompileError),
            Some(f) => {
                let ifunction = self.heap.alloc(ObjectData::Function(f));
                let closure = Closure::new(ifunction);
                let iclosure = self.heap.alloc(ObjectData::Closure(closure));
                self.stack.push(Value::Closure(iclosure));
                self.call(iclosure, 0);
                self.run()
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.current_frame();
        let b = self.current_chunk().code[frame.ip];
        self.current_frame_mut().ip += 1;
        b
    }

    fn read_short(&mut self) -> u16 {
        let frame = self.current_frame();
        let chunk = self.current_chunk();
        let f = chunk.code[frame.ip] as u16;
        let s = chunk.code[frame.ip + 1] as u16;
        self.current_frame_mut().ip += 2;
        f << 8 | s
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte();
        let chunk = self.current_chunk();
        chunk.read_constant(index)
    }

    fn read_string(&mut self) -> HeapId {
        let index = self.read_byte();
        let chunk = self.current_chunk();
        chunk.read_string(index)
    }

    pub fn peek(&self, distance: usize) -> Value {
        self.stack[self.stack.len() - 1 - distance]
    }

    pub fn runtime_error(&mut self, msg: &str) {
        eprintln!("{}", msg);
        self.frames.iter().rev().for_each(|frame| {
            let closure = cast!(self.heap.lookup(frame.iclosure), Closure);
            let f = cast!(self.heap.lookup(closure.ifunction), Function);
            let line = frame.ip - 1;

            match f.name {
                Some(istring) => {
                    let function_name = cast!(self.heap.lookup(istring), String);
                    eprintln!("[line {}] in {}()", f.chunk.lines[line], function_name);
                }
                None => eprintln!("[line {}] in script", f.chunk.lines[line]),
            };
        });
        self.stack.clear();
    }

    pub fn call_value(&mut self, callee: Value, arg_count: u8) -> bool {
        match callee {
            Value::Closure(iclosure) => self.call(iclosure, arg_count as usize),
            Value::NativeFunction(f) => {
                let left = self.stack.len() - arg_count as usize;
                let result = f(&self.stack[left..]);
                self.stack.push(result);
                true
            }
            _ => {
                self.runtime_error("Can only call funtions and classes.");
                false
            }
        }
    }

    fn call(&mut self, iclosure: HeapId, arg_count: usize) -> bool {
        let closure = cast!(self.heap.lookup(iclosure), Closure);
        let f = cast!(self.heap.lookup(closure.ifunction), Function);

        if arg_count != f.arity {
            self.runtime_error(
                format!("Expected {} arguments but got {}.", f.arity, arg_count).as_str(),
            );
            return false;
        } else if self.frames.len() == VM::FRAME_MAX {
            self.runtime_error("Stack overflow.");
            return false;
        }
        let frame = CallFrame::new(
            iclosure,
            closure.ifunction,
            self.stack.len() - arg_count - 1,
        );
        self.frames.push(frame);
        true
    }

    fn capture_upvalue(&mut self, location: usize) -> HeapId {
        let mut prev_upvalue = None;
        let mut upvalue = self.open_upvalue;

        while let Some(value) =
            upvalue.filter(|upvalue| cast!(self.heap.lookup(*upvalue), UpValue).location > location)
        {
            upvalue = cast!(self.heap.lookup(value), UpValue).next;
            prev_upvalue = Some(value);
        }

        if let Some(value) = upvalue
            .filter(|upvalue| cast!(self.heap.lookup(*upvalue), UpValue).location == location)
        {
            return value;
        }

        let mut created_upvalue = UpValue::new(location);
        created_upvalue.next = upvalue;
        let iupvalue = self.heap.alloc(ObjectData::UpValue(created_upvalue));

        if let Some(value) = prev_upvalue {
            cast!(self.heap.lookup_mut(value), UpValue).next = Some(iupvalue);
        } else {
            self.open_upvalue = Some(iupvalue);
        }

        iupvalue
    }

    fn close_upvalues(&mut self, last: usize) {
        while let Some(upvalue) = self
            .open_upvalue
            .filter(|upvalue| cast!(self.heap.lookup(*upvalue), UpValue).location >= last)
        {
            let mut upvalue = cast!(self.heap.lookup_mut(upvalue), UpValue);
            let value = self.stack[upvalue.location];
            upvalue.closed = Some(value);
            self.open_upvalue = upvalue.next.take();
        }
    }

    #[allow(dead_code)]
    pub fn upvalues(&self, ifunction: HeapId) -> &Vec<FnUpValue> {
        &cast!(self.heap.lookup(ifunction), Function).upvalues
    }

    pub fn print_value(&self, v: Value) -> String {
        match v {
            Value::Nil => "nil".to_owned(),
            Value::Bool(v) => format!("{}", v),
            Value::Number(v) => format!("{}", v),
            Value::String(i) => cast!(self.heap.lookup(i), String).to_owned(),
            Value::Function(i) => {
                if let Some(fn_name) = cast!(self.heap.lookup(i), Function).name {
                    return format!("<fn {}>", cast!(self.heap.lookup(fn_name), String));
                }
                "<script>".to_owned()
            }
            Value::NativeFunction(_) => "<native fn>".to_owned(),
            Value::Closure(i) => {
                let ifunction = cast!(self.heap.lookup(i), Closure).ifunction;
                if let Some(fn_name) = cast!(self.heap.lookup(ifunction), Function).name {
                    return format!("<fn {}>", cast!(self.heap.lookup(fn_name), String));
                }
                "<closure>".to_owned()
            }
        }
    }

    pub fn run(&mut self) -> InterpretResult {
        macro_rules! binary_op {
            ($type:ident, $op:tt) => {{
                let b: Value = self.stack.pop().unwrap();
                let a: Value = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Number(a), Value::Number(b)) => {
                        self.stack.push(Value::$type(a $op b));
                    },
                    (Value::String(a), Value::String(b)) => {
                        let a_str = cast!(self.heap.lookup(a), String);
                        let b_str = cast!(self.heap.lookup(b), String);
                        let r = format!("{}{}", a_str, b_str);
                        let r = self.heap.intern(&r);
                        self.stack.push(Value::String(r));
                    },
                    _ => {
                        self.runtime_error("Operands must be numbers or strings.");
                        return Err(VMError::RuntimeError);
                    },
                }
            }};
        }

        loop {
            #[cfg(debug_assertions)]
            {
                print!("          ");
                self.stack.iter().for_each(|v| {
                    print!("[ {} ]", self.print_value(*v));
                });
                println!();
                let d = Disassembler::new(self.current_chunk(), self);
                d.instruction(self.current_frame().ip);
            }

            let op = OpCode::try_from(self.read_byte()).unwrap();
            match op {
                OpCode::Return => {
                    let result = self.stack.pop().unwrap();
                    let frame = self.frames.pop().unwrap();
                    self.close_upvalues(frame.slot);

                    if self.frames.is_empty() {
                        return Ok(());
                    } else {
                        self.stack.truncate(frame.slot);
                        self.stack.push(result);
                    }
                }
                OpCode::Constant => {
                    let c = self.read_constant();
                    self.stack.push(c);
                }
                OpCode::Negate => {
                    if let Value::Number(v) = self.peek(0) {
                        self.stack.pop();
                        self.stack.push(Value::Number(-v));
                    } else {
                        self.runtime_error("Operand must be a number.");
                        return Err(VMError::RuntimeError);
                    }
                }
                OpCode::Add => binary_op!(Number, +),
                OpCode::Substract => binary_op!(Number, -),
                OpCode::Multiply => binary_op!(Number, *),
                OpCode::Divide => binary_op!(Number, /),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::Not => {
                    let v: Value = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(v.is_falsey()));
                }
                OpCode::Equal => {
                    let b: Value = self.stack.pop().unwrap();
                    let a: Value = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(a == b));
                }
                OpCode::Greater => binary_op!(Bool, >),
                OpCode::Less => binary_op!(Bool, <),
                OpCode::Print => {
                    if let Some(v) = self.stack.pop() {
                        println!("{}", self.print_value(v));
                    }
                }
                OpCode::Pop => {
                    self.stack.pop();
                }
                OpCode::DefineGlobal => {
                    let istring = self.read_string();
                    let name = self.stack.pop().unwrap();
                    self.globals.insert(istring, name);
                }
                OpCode::GetGlobal => {
                    let istring = self.read_string();
                    match self.globals.get(&istring) {
                        Some(&value) => self.stack.push(value),
                        None => {
                            let name = cast!(self.heap.lookup(istring), String);
                            let msg = format!("Undefined variable '{}'.", name);
                            self.runtime_error(&msg);
                            return Err(VMError::RuntimeError);
                        }
                    }
                }
                OpCode::SetGlobal => {
                    let istring = self.read_string();
                    let value = self.peek(0);
                    if self.globals.insert(istring, value).is_none() {
                        self.globals.remove(&istring);
                        let name = cast!(self.heap.lookup(istring), String);
                        let msg = format!("Undefined variable '{}'.", name);
                        self.runtime_error(&msg);
                        return Err(VMError::RuntimeError);
                    }
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte() as usize + self.current_frame().slot;
                    let value = self.stack[slot];
                    self.stack.push(value);
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte() as usize + self.current_frame().slot;
                    let value = self.peek(0);
                    self.stack[slot as usize] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short();
                    if self.peek(0).is_falsey() {
                        self.current_frame_mut().ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short();
                    self.current_frame_mut().ip += offset as usize;
                }
                OpCode::Loop => {
                    let offset = self.read_short();
                    self.current_frame_mut().ip -= offset as usize;
                }
                OpCode::Call => {
                    let arg_count = self.read_byte();
                    let callee = self.peek(arg_count as usize);
                    if !self.call_value(callee, arg_count) {
                        return Err(VMError::RuntimeError);
                    }
                }
                OpCode::Closure => {
                    let constant = self.read_constant();

                    if let Value::Function(ifunction) = constant {
                        let mut closure = Closure::new(ifunction);
                        let upvalues = cast!(self.heap.lookup(ifunction), Function).upvalues.len();

                        for i in 0..upvalues {
                            let upvalue = &cast!(self.heap.lookup(ifunction), Function).upvalues[i];
                            let upvalue = if upvalue.is_local {
                                let location = self.current_frame().slot + upvalue.index as usize;
                                self.capture_upvalue(location)
                            } else {
                                self.current_closure().upvalues[upvalue.index as usize]
                            };
                            closure.upvalues.push(upvalue);
                        }
                        let iclosure = self.heap.alloc(ObjectData::Closure(closure));
                        self.stack.push(Value::Closure(iclosure));
                    }
                }
                OpCode::GetUpValue => {
                    let slot = self.read_byte();
                    let value = {
                        let iupvalue = self.current_closure().upvalues[slot as usize];
                        let upvalue = cast!(self.heap.lookup_mut(iupvalue), UpValue);
                        match upvalue.closed {
                            Some(v) => v,
                            None => self.stack[upvalue.location],
                        }
                    };
                    self.stack.push(value)
                }
                OpCode::SetUpValue => {
                    let slot = self.read_byte();
                    let value = self.peek(0);
                    let iupvalue = self.current_closure().upvalues[slot as usize];
                    let mut upvalue = cast!(self.heap.lookup_mut(iupvalue), UpValue);
                    if upvalue.closed.is_none() {
                        self.stack[upvalue.location] = value
                    } else {
                        upvalue.closed = Some(value);
                    }
                }
                OpCode::CloseUpValue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.stack.pop();
                }
            }
        }
    }
}
