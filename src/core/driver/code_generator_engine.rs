use std::collections::HashMap;
use std::{io, mem};
use dynasm::dynasm;
use dynasmrt::{AssemblyOffset, DynasmApi, ExecutableBuffer, VecAssembler};
use dynasmrt::aarch64::Aarch64Relocation;
use dynasmrt::mmap::MutableBuffer;
use crate::core::api::{Command, Condition, NodeKind, Ref};
use crate::core::driver::driver::{Engine, Frame, NodeId, RunState};
use crate::core::driver::util::flush_code_cache;
use crate::core::interpreter::get_u32;
use dynasmrt::DynasmLabelApi;
use multimap::MultiMap;

// engine <-> generated code interop/call conventions:
//   X0 pointer to data stack start
//   X1 pointer to data stack end // never changes during execution
//   X2 pointer to result struct // never changes during execution, no need for now, will be needed for stack unwinding
// X1 & X2 can/should be moved to thread local variables since they never change during execution trace
//
// execution might abort/finish due to following reasons:
//   next node isn't registered or execution is suspended: W0 = 0, W1 = next node id
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

// todo: use unions
#[derive(Debug, Clone)]
#[repr(C)]
pub struct Output {
    pub code: u32, // w0
    pub call_id: u32, // x0_high, only for function calls
    pub node_id: u32, // w1
    pub next_id: u32, // x1_high, only for function calls
}

impl Output {
    fn next_node(id: NodeId) -> Output {
        Output { code: 0, call_id: 0, node_id: id.0, next_id: 0 }
    }

    fn final_node(id: NodeId) -> Output {
        Output { code: 1, call_id: 0, node_id: id.0, next_id: 0 }
    }

    fn call_node(offset: u32, call: NodeId, next: NodeId, id: NodeId) -> Output {
        Output { code: 3 + offset, call_id: call.0, node_id: id.0, next_id: next.0 }
    }
}

#[derive(Debug, Clone)]
struct ReturnInfo {
    output: Output,
    from: usize,
    to: usize,
}

macro_rules! asm {
    ($ops:ident $($t:tt)*) => {
        dynasm!($ops
            ; .arch aarch64
            ; .alias data_stack, x0
            $($t)*
        )
    }
}

fn interop(fn_ptr: *const u8, input: Input) -> Output {
    // Can't have input as input struct because such structs being passed in memory
    // But output is fine, I guess because it's under two fields
    // checked with godbolt
    let call: extern "C" fn(u64, u64, u64) -> Output = unsafe { mem::transmute(fn_ptr) };
    call(input.data_stack_start, input.data_stack_end, input.stack_unwinding_dst)
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
    returns: MultiMap<NodeId, ReturnInfo>,
}

impl CodeGeneratorEngine {
    pub fn new(size: usize) -> io::Result<CodeGeneratorEngine> {
        Ok(CodeGeneratorEngine {
            size,
            code: Code::Executable(ExecutableBuffer::new(size)?),
            offset: 0,
            offsets: HashMap::new(),
            returns: MultiMap::new(),
        })
    }

    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        if self.offsets.contains_key(&id) { return; }

        let mut writable = match std::mem::replace(&mut self.code, Code::__Holder) {
            Code::Executable(buffer) => { buffer.make_mut().unwrap() }
            Code::Writable(buffer) => { buffer }
            Code::__Holder => { panic!() }
        };
        // why do we need this?
        writable.set_len(self.size);
        self.remove_last_return_if_needed(id);
        // todo: if last return is current id - erase it and write on top of it instead
        let mut ops = Assembler { buffer: writable, offset: self.offset };
        let returns = ops.generate(id, kind.clone());

        self.offsets.insert(id, AssemblyOffset(self.offset));
        // todo: replace previous returns of this id to jump to new location

        self.offset = ops.offset;

        // replace returns
        for ret in returns {
            if ret.output.code == 0 {
                let id = NodeId(ret.output.node_id);
                if let Some(offset) = self.offsets.get(&id) {
                    Self::replace_ret(&mut ops.buffer, ret, *offset)
                } else {
                    self.returns.insert(id, ret)
                }
            }
        }
        let replacements =
            if let Some(returns) = self.returns.get_vec_mut(&id) {
                let clone = returns.clone();
                returns.clear();
                clone
            } else { vec![] };

        let offset = *self.offsets.get(&id).unwrap();
        for ret in replacements {
            Self::replace_ret(&mut ops.buffer, ret, offset);
        }

        flush_code_cache(&ops.buffer);
        std::mem::replace(&mut self.code, Code::Executable(ops.buffer.make_exec().unwrap()));
    }

    fn remove_last_return_if_needed(&mut self, id: NodeId) {
        if let Some(returns) = self.returns.get_vec_mut(&id) {
            if let Some(pos) = returns.iter().position(|e| e.to == self.offset) {
                self.offset = returns.get(pos).unwrap().from;
                returns.remove(pos);
            }
        }
    }

    fn replace_ret(buffer: &mut MutableBuffer, ret: ReturnInfo, dest: AssemblyOffset) {
        // todo: what if more than 1mb size difference?
        // todo: just generate instructions...
        let diff = (dest.0 as i32) - (ret.from as i32);
        let mut asm: VecAssembler<Aarch64Relocation> = VecAssembler::new(0);
        if diff > 0 {
            asm!(asm
                ; b >label
            );
            (0..(diff.abs() / 4 - 1)).for_each(|_| {
                asm!(asm
                    ; nop
                );
            });
            asm!(asm
                ; label:
            );
            let bytes = asm.finalize().unwrap();
            let res_idx = if (diff >= 0) { 0 } else { bytes.len() - 4 };
            buffer.as_mut()[ret.from..ret.from + 4].copy_from_slice(&bytes.as_slice()[res_idx..res_idx + 4]);
        } else {
            asm!(asm
                ; label:
            );
            (0..diff.abs() / 4).for_each(|_| {
                asm!(asm
                    ; nop
                );
            });
            asm!(asm
                ; b <label
            );
            let bytes = asm.finalize().unwrap();
            let res_idx = if (diff >= 0) { 0 } else { bytes.len() - 4 };
            buffer.as_mut()[ret.from..ret.from + 4].copy_from_slice(&bytes.as_slice()[res_idx..res_idx + 4]);
        }
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
                    let data_offset = state.offset() + frame.offset;
                    let input = Input {
                        data_stack_start: stack[data_offset..].as_mut_ptr() as usize as u64,
                        data_stack_end: stack.as_ptr_range().end as usize as u64,
                        stack_unwinding_dst: 0
                    };
                    let output = interop(executable.ptr(*code_offset), input);
                    match output.code {
                        0 => { // node not registered, or execution is suspended
                            state.frames.push(Frame { id: NodeId(output.node_id), offset: frame.offset });
                            return true;
                        }
                        1 => { // final node
                            return false;
                        }
                        2 => { // out of data stack
                            todo!()
                        }
                        offset_plus_3 =>  { // function call
                            state.frames.push(Frame { id: NodeId(output.next_id), offset: frame.offset });
                            state.frames.push(Frame { id: NodeId(output.call_id), offset:  (offset_plus_3 - 3) as usize });
                            return !self.offsets.contains_key(&NodeId(output.call_id));
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
    // todo: kind should be more like NodeKind<NodeId | AssemblyOffset>
    fn generate(&mut self, id: NodeId, kind: NodeKind<NodeId>) -> Vec<ReturnInfo> {
        match kind {
            NodeKind::Command { command, next } => {
                self.command(command);
                vec![self.ret(Output::next_node(next))]
            }
            NodeKind::Branch { condition, if_true, if_false } => {
                // todo: record into self current if_true_label offset so we can resolve it
                self.condition(condition, 5);
                vec![
                    self.ret(Output::next_node(if_false)),
                    self.ret(Output::next_node(if_true)),
                ]
            }
            NodeKind::Call { offset, call, next } => {
                vec![self.ret(Output::call_node(offset, call, next, id))]
            }
            NodeKind::Final => {
                vec![self.ret(Output::final_node(id))]
            }
        }
    }

    fn command(&mut self, command: Command) {
        match command {
            Command::Noop => { panic!("can't happen") }
            Command::PoisonFrom { .. } => { panic!("can't happen") }
            Command::Set { dst, bytes } => { self.set(dst, bytes) }
            Command::Copy { dst, size, op } => { self.copy(size, dst, op) }
            Command::Add { size, dst, op1, op2 } => { self.add(size, dst, op1, op2) }
            Command::Sub { size, dst, op1, op2 } => { self.sub(size, dst, op1, op2) }
        }
    }

    fn condition(&mut self, condition: Condition, ret_true_bytes: u8) {
        match condition {
            Condition::Ne { size, op1, op2 } => { self.ne(size, op1, op2, ret_true_bytes) }
            Condition::Ne0 { size, op } => { self.ne0(size, op, ret_true_bytes) }
        }
    }

    fn set(&mut self, dst: Ref, bytes: Vec<u8>) {
        match bytes.len() {
            4 => {
                self.mov_u32(9, get_u32(Ref::Stack(0), bytes.as_slice()).0);
                self.store_u32(9, dst);
            }
            _ => { todo!() }
        }
    }

    fn copy(&mut self, len: u32, dst: Ref, op: Ref) {
        match len {
            4 => {
                self.load_u32(9, op);
                self.store_u32(9, dst);
            }
            _ => { todo!() }
        }
    }

    fn add(&mut self, len: u32, dst: Ref, op1: Ref, op2: Ref) {
        match len {
            4 => {
                self.load_u32(9, op1);
                self.load_u32(10, op2);
                asm!(self
                    ; add w11, w9, w10
                );
                self.store_u32(11, dst);
            }
            _ => { todo!() }
        }
    }

    fn sub(&mut self, len: u32, dst: Ref, op1: Ref, op2: Ref) {
        match len {
            4 => {
                self.load_u32(9, op1);
                self.load_u32(10, op2);
                asm!(self
                    ; sub w11, w9, w10
                );
                self.store_u32(11, dst);
            }
            _ => { todo!() }
        }
    }

    fn ret(&mut self, output: Output) -> ReturnInfo {
        let mut return_info = ReturnInfo { output: output.clone(), from: self.offset, to: 0 };
        self.mov_u32(0, output.code);
        self.mov_u32(1, output.node_id);
        if output.code >= 3 {
            self.mov_u32_high(0, output.call_id);
            self.mov_u32_high(1, output.next_id);
        }
        asm!(self
            ; ret
        );
        return_info.to = self.offset;
        return_info
    }

    fn ne(&mut self, len: u32, op1: Ref, op2: Ref, ret_true_bytes: u8) {
        match len {
            4 => {
                self.load_u32(9, op1);
                self.load_u32(10, op2);
                asm!(self
                    ; cmp w9, w10
                );
                // todo: can we pass &mut asm instead?
                self.bcond(|mut asm| {
                    asm!(asm
                        ; b.ne >if_true_label
                    );
                    asm
                }, ret_true_bytes);
            }
            _ => { todo!() }
        }
    }

    fn ne0(&mut self, len: u32, op: Ref, ret_true_bytes: u8) {
        match len {
            4 => {
                self.load_u32(9, op);
                asm!(self
                    ; cmp w9, 0
                );
                self.bcond(|mut asm| {
                    asm!(asm
                        ; b.ne >if_true_label
                    );
                    asm
                }, ret_true_bytes);
            }
            _ => { todo!() }
        }
    }

    fn bcond(&mut self, cond_fn: fn(VecAssembler<Aarch64Relocation>) -> VecAssembler<Aarch64Relocation>, ret_true_bytes: u8) {
        let mut asm = cond_fn(VecAssembler::new(0));
        (0..ret_true_bytes).for_each(|_| {
            asm!(asm
                ; nop
            );
        });
        asm!(asm
            ; if_true_label:
        );
        let bytes = asm.finalize().unwrap();
        self.extend(&bytes.as_slice()[0..4]);
    }

    fn mov_u32(&mut self, register: u32, value: u32) {
        let low = value as u16;
        let high = (value >> 16) as u16;
        asm!(self
            ; mov X(register), low as u64
            ; movk X(register), high as u32, lsl 16
        );
    }

    fn mov_u32_high(&mut self, register: u32, value: u32) {
        let low = value as u16;
        let high = (value >> 16) as u16;
        asm!(self
            ; movk X(register), low as u32, lsl 32
            ; movk X(register), high as u32, lsl 48
        );
    }

    fn store_u32(&mut self, register: u32, dst: Ref) {
        match dst {
            Ref::Stack(offset) => {
                // todo: check for stack overflow
                // todo: what if offset is big?
                asm!(self
                    ; str W(register), [data_stack, offset]
                );
            }
        }
    }

    fn load_u32(&mut self, register: u32, op: Ref) {
        match op {
            Ref::Stack(offset) => {
                // todo: check for stack overflow
                // todo: what if offset is big?
                asm!(self
                    ; ldr W(register), [data_stack, offset]
                );
            }
        }
    }
}

impl Extend<u8> for Assembler {
    fn extend<T: IntoIterator<Item=u8>>(&mut self, iter: T) { todo!() }
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
    fn offset(&self) -> AssemblyOffset { todo!() }
    fn push(&mut self, byte: u8) { todo!() }
    fn align(&mut self, alignment: usize, with: u8) { todo!() }
}