use std::collections::HashMap;
use std::{io, mem};
use dynasm::dynasm;
use dynasmrt::{AssemblyOffset, DynasmApi, ExecutableBuffer};
use dynasmrt::mmap::MutableBuffer;
use crate::core::api::{Command, Condition, NodeKind};
use crate::core::driver::driver::{Engine, NodeId, RunState};

// engine <-> generated code interop/call conventions:
//   X0 pointer to data stack start
//   X1 pointer to data stack end // never changes during execution
//   X2 pointer to result struct // never changes during execution, no need for now, will be needed for stack unwinding
// X1 & X2 can/should be moved to thread local variables since they never change during execution trace
//
// execution might abort/finish due to following reasons:
//   next node isn't registered: W0 = 0, W1 = next node id
//   next node is final: W0 = 1, W1 = final node id
//   out of space in data stack: W0 = 2, W1 = context node id
//   next node is function call: W0 = 3 + offset, W1 = function node id, upper X1 is call node id, upper X2 is next node id
//     should be changed with proper function call support in native

// to guarantee that stack doesn't spill we can have per node id guaranteed stack depth (meaning at least this number of bytes is definitely available from this point)
// and only if there is no guarantee on particular point we can check stack size and increase size if needed!
// this way we don't need to keep data stack end in a register and compare to it all the time

#[derive(Debug)]
#[repr(C)]
pub struct Input {
    pub data_stack_start: u64, // x0
    pub data_stack_end: u64, // x1
    pub stack_unwinding_dst: u64, // x2
}

#[derive(Debug)]
#[repr(C)]
pub struct Output {
    pub code: u32, // w0
    pub call_id: u32, // x0_high, only for function calls
    pub node_id: u32, // w1
    pub next_id: u32, // x1_high, only for function calls
}

enum Code {
    Executable(ExecutableBuffer),
    Writable(MutableBuffer),
    __Holder,
}

pub struct CodeGeneratorEngine {
    size: usize,
    code: Code,
    offset: usize,
    offsets: HashMap<NodeId, AssemblyOffset>,
}

impl CodeGeneratorEngine {
    pub fn new(size: usize) -> io::Result<CodeGeneratorEngine> {
        Ok(CodeGeneratorEngine {
            size,
            code: Code::Executable(ExecutableBuffer::new(size)?),
            offset: 0,
            offsets: HashMap::new(),
        })
    }

    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        let mut writable = match std::mem::replace(&mut self.code, Code::__Holder) {
            Code::Executable(buffer) => { buffer.make_mut().unwrap() }
            Code::Writable(buffer) => { buffer }
            Code::__Holder => { panic!() }
        };
        // why do we need this?
        writable.set_len(self.size);
        // todo: if last return is current id - erase it and write on top of it instead
        let mut ops = Assembler { buffer: writable, offset: self.offset };
        ops.generate(id, kind);

        self.offsets.insert(id, AssemblyOffset(self.offset));
        // todo: replace previous returns of this id to jump to new location

        self.offset = ops.offset;
        // todo: need to flush m1 code caches?, see sys_icache_invalidate at https://developer.apple.com/documentation/apple-silicon/porting-just-in-time-compilers-to-apple-silicon
        std::mem::replace(&mut self.code, Code::Executable(ops.buffer.make_exec().unwrap()));
    }

    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool {
        if let Code::Executable(executable) = &self.code {
            let frame = state.frames.pop().unwrap();

            match self.offsets.get(&frame.id) {
                None => {
                    state.frames.push(frame);
                    true
                }
                Some(code_offset) => {
                    let data_offset = state.offset();
                    let input = Input {
                        data_stack_start: stack[data_offset..].as_mut_ptr() as usize as u64,
                        data_stack_end: stack.as_ptr_range().end as usize as u64,
                        stack_unwinding_dst: 0
                    };
                    let call: extern "C" fn(Input) -> Output = unsafe { mem::transmute(executable.ptr(*code_offset)) };
                    let output = call(input);
                    match output.code {
                        0 => { // not registered
                            todo!()
                        }
                        1 => { // final node
                            return false;
                        }
                        2 => { // out of data stack
                            todo!()
                        }
                        offset_plus_3 =>  { // function call
                            todo!()
                        }
                    }
                }
            }
        } else {
            panic!()
        }
    }
}

impl Engine for CodeGeneratorEngine {
    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) { self.register(id, kind) }

    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool { self.run(state, stack) }
}

struct Assembler {
    buffer: MutableBuffer,
    offset: usize,
}

impl Assembler {
    fn generate(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        match kind {
            NodeKind::Command { command, next } => {
                self.generate_command(id, command, next);
            }
            NodeKind::Branch { condition, if_true, if_false } => {
                self.generate_condition(id, condition, if_true, if_false);
            }
            NodeKind::Call { offset, call, next } => {
                self.generate_call(id, offset, call, next);
            }
            NodeKind::Final => {
                self.generate_final(id);
            }
        }
    }

    fn generate_final(&mut self, id: NodeId) {
        // todo: move doesn't work and copies only 16 bits I think. need proper code here.
        dynasm!(self
            ; .arch aarch64
            ; mov x0, 1
            ; mov x1, id.0 as u64
            ; ret
        );
    }

    fn generate_command(&mut self, id: NodeId, command: Command, next: NodeId) {
        todo!()
    }

    fn generate_condition(&mut self, id: NodeId, condition: Condition, if_true: NodeId, if_false: NodeId) {
        todo!()
    }

    fn generate_call(&mut self, id: NodeId, offset: u32, call: NodeId, next: NodeId) {
        todo!()
    }
}

impl Extend<u8> for Assembler {
    fn extend<T: IntoIterator<Item=u8>>(&mut self, iter: T) {
        todo!()
    }
}

impl<'a> Extend<&'a u8> for Assembler {
    fn extend<T: IntoIterator<Item=&'a u8>>(&mut self, iter: T) {
        for byte in iter {
            *self.buffer.get_mut(self.offset).unwrap() = *byte;
            self.offset += 1;
        }
    }
}

impl DynasmApi for Assembler {
    fn offset(&self) -> AssemblyOffset {
        todo!()
    }

    fn push(&mut self, byte: u8) {
        todo!()
    }

    fn align(&mut self, alignment: usize, with: u8) {
        todo!()
    }
}