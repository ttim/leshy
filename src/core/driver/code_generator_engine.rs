use std::collections::HashMap;
use std::{io, mem};
use dynasm::dynasm;
use dynasmrt::{AssemblyOffset, DynasmApi, ExecutableBuffer};
use dynasmrt::aarch64::Aarch64Relocation;
use dynasmrt::mmap::MutableBuffer;
use crate::core::api::{Command, Condition, NodeKind, Ref};
use crate::core::driver::driver::{Engine, Frame, NodeId, RunState};
use crate::core::driver::util::flush_code_cache;
use crate::core::interpreter::get_u32;
use dynasmrt::relocations::Relocation;
use lazy_static::lazy_static;
use multimap::MultiMap;

// engine <-> generated code interop/call conventions:
//   X0 pointer to data stack start
//   X1 pointer to data stack end // never changes during execution
//   X2 pointer to result struct // never changes during execution, no need for now, will be needed for stack unwinding
// X1 & X2 can/should be moved to thread local variables since they never change during execution trace
//
// execution might abort/finish due to following reasons:
//   next node is final: W0 = 0, W1 = final node id
//   next node isn't registered or execution is suspended: W0 = 1, W1 = next node id
//   out of space in data stack: W0 = 2, W1 = context node id
//   next node is function call: W0 = 3 + offset, W1 = function node id, upper X1 is call node id, upper X2 is next node id
//     should be changed with proper function call support in native

// to guarantee that stack doesn't spill we can have per node id guaranteed stack depth (meaning at least this number of bytes is definitely available from this point)
// and only if there is no guarantee on particular point we can check stack size and increase size if needed!
// this way we don't need to keep data stack end in a register and compare to it all the time

#[derive(Debug, Clone)]
pub enum Output {
    Finish(NodeId),
    // rename to suspend
    Suspend(NodeId),
    Call { offset: u32, call: NodeId, next: NodeId, id: NodeId },
    // add OutOfStack?
}

impl Output {
    fn from_registers(w0: u32, w0_high: u32, w1: u32, w1_high: u32) -> Output {
        match w0 {
            0 => { Output::Finish(NodeId(w1)) }
            1 => { Output::Suspend(NodeId(w1)) }
            2 => { todo!() }
            offset_plus_3 => { Output::Call { offset: offset_plus_3 - 3, call: NodeId(w0_high), next: NodeId(w1_high), id: NodeId(w1) } }
        }
    }

    fn to_registers(&self) -> (u32, u32, Option<(u32, u32)>) { // w0, w1, high (x0, x1)
        match self {
            Output::Finish(id) => { (0, id.0, None) }
            Output::Suspend(id) => { (1, id.0, None) }
            Output::Call { offset, call, next, id } => { (offset + 3, id.0, Some((call.0, next.0))) }
        }
    }
}

fn interop(fn_ptr: *const u8, stack_start: *mut u8, stack_end: *const u8, unwind_dst: *mut u8) -> Output {
    // Can't have input as input struct because such structs being passed in memory
    // But output is fine, I guess because it's under two fields
    // checked with godbolt
    let call: extern "C" fn(*mut u8, *const u8, *mut u8) -> (u32, u32, u32, u32) = unsafe { mem::transmute(fn_ptr) };
    let (w0, w0_high, w1, w1_high) = call(stack_start, stack_end, unwind_dst);
    Output::from_registers(w0, w0_high, w1, w1_high)
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
            if let Output::Suspend(id) = ret.output {
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
        Assembler::write_b(buffer, ret.from, dest.0 as isize - ret.from as isize)
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
                    let mut unwind_dst = [0u8; 100];
                    let output = interop(executable.ptr(*code_offset), stack[data_offset..].as_mut_ptr(), stack.as_ptr_range().end, unwind_dst.as_mut_ptr());
                    match output {
                        Output::Finish(_) => {
                            return false;
                        }
                        Output::Suspend(id) => { // node not registered, or execution is suspended
                            state.frames.push(Frame { id, offset: frame.offset });
                            return true;
                        }
                        Output::Call { offset, call, next, id } => { // function call
                            state.frames.push(Frame { id: next, offset: frame.offset });
                            state.frames.push(Frame { id: call, offset: offset as usize });
                            return !self.offsets.contains_key(&call);
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

lazy_static! {
    static ref BRANCH: HashMap<&'static str, Vec<u8>> = {
        let mut m = HashMap::new();
        // todo: got this from debuging internals of generated dynasm code. figure out where it comes from in macro!
        m.insert("ne", vec![1, 0, 0, 84]);
        m
    };
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
                vec![self.ret(Output::Suspend(next))]
            }
            NodeKind::Branch { condition, if_true, if_false } => {
                self.condition(condition, 6 * 4);
                vec![
                    self.ret(Output::Suspend(if_false)),
                    self.ret(Output::Suspend(if_true)),
                ]
            }
            NodeKind::Call { offset, call, next } => {
                vec![self.ret(Output::Call { offset, call, next, id })]
            }
            NodeKind::Final => {
                vec![self.ret(Output::Finish(id))]
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

    fn condition(&mut self, condition: Condition, ret_true_offset: isize) {
        match condition {
            Condition::Ne { size, op1, op2 } => { self.ne(size, op1, op2, ret_true_offset) }
            Condition::Ne0 { size, op } => { self.ne0(size, op, ret_true_offset) }
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
        let (w0, w1, high) = output.to_registers();
        self.mov_u32(0, w0);
        self.mov_u32(1, w1);
        if let Some((high0, high1)) = high {
            self.mov_u32_high(0, high0);
            self.mov_u32_high(1, high1);
        }
        asm!(self
            ; ret
        );
        return_info.to = self.offset;
        return_info
    }

    fn ne(&mut self, len: u32, op1: Ref, op2: Ref, ret_true_offset: isize) {
        match len {
            4 => {
                self.load_u32(9, op1);
                self.load_u32(10, op2);
                asm!(self
                    ; cmp w9, w10
                );
                self.bcond("ne", ret_true_offset);
            }
            _ => { todo!() }
        }
    }

    fn ne0(&mut self, len: u32, op: Ref, ret_true_offset: isize) {
        match len {
            4 => {
                self.load_u32(9, op);
                asm!(self
                    ; cmp w9, 0
                );
                self.bcond("ne", ret_true_offset);
            }
            _ => { todo!() }
        }
    }

    fn bcond(&mut self, modifier: &'static str, offset: isize) {
        let mut bytes = BRANCH.get(modifier).unwrap().clone();
        Aarch64Relocation::BCOND.write_value(&mut bytes, offset);
        self.extend(bytes.as_slice());
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

    fn write_b(buffer: &mut MutableBuffer, offset: usize, rel_dst: isize) {
        // todo: got this from debuging internals of generated dynasm code. figure out where it comes from in macro!
        let mut template = vec!(0, 0, 0, 20);
        Aarch64Relocation::B.write_value(&mut template, rel_dst);
        buffer.as_mut()[offset..offset + 4].copy_from_slice(template.as_slice());
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