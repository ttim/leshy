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
use dynasmrt::relocations::Relocation;
use lazy_static::lazy_static;
use multimap::MultiMap;
use dynasmrt::DynasmLabelApi;

// engine <-> generated code interop/call conventions:
//   X0 pointer to data stack start
//   X1 pointer to data stack end // never changes during execution
//   X2 pointer to result struct // never changes during execution, no need for now, will be needed for stack unwinding
// X1 & X2 can/should be moved to thread local variables since they never change during execution trace
//
// execution might abort/finish due to following reasons:
//   x0 = end of written entries into suspend struct, i.e. if it's final node it's same as x2
//   todo: out of space in data stack

// to guarantee that stack doesn't spill we can have per node id guaranteed stack depth (meaning at least this number of bytes is definitely available from this point)
// and only if there is no guarantee on particular point we can check stack size and increase size if needed!
// this way we don't need to keep data stack end in a register and compare to it all the time

#[repr(C)]
#[derive(Copy, Clone)]
struct SuspendTrace {
    offset: u32,
    id: NodeId
}

fn interop(fn_ptr: *const u8, stack_start: *mut u8, stack_end: *const u8, unwind_dst: *mut SuspendTrace) -> usize {
    // Can't have input as input struct because such structs being passed in memory
    // But output is fine, I guess because it's under two fields
    // checked with godbolt
    let call: extern "C" fn(*mut u8, *const u8, *mut SuspendTrace) -> u64 = unsafe { mem::transmute(fn_ptr) };
    call(stack_start, stack_end, unwind_dst) as usize
}

// place corresponds to: put (id, 0) into unwind_dst, ret 1. i.e the one which triggers suspend on unknown node
#[derive(Debug, Clone)]
struct ReturnInfo {
    id: NodeId,
    from: usize,
    to: usize,
}

macro_rules! asm {
    ($ops:ident $($t:tt)*) => {
        dynasm!($ops
            ; .arch aarch64
            ; .alias data_stack, x0
            ; .alias unwind_stack, x2
            ; .alias unwind_stack_end, x0
            ; .alias lr, x30
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
            if let Some(offset) = self.offsets.get(&ret.id) {
                Self::replace_ret(&mut ops.buffer, ret, *offset)
            } else {
                self.returns.insert(ret.id, ret)
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
                    let mut unwind_dst = [SuspendTrace { offset: 0, id: NodeId(0) }; 1024];
                    let output = interop(executable.ptr(*code_offset), stack[data_offset..].as_mut_ptr(), stack.as_ptr_range().end, unwind_dst.as_mut_ptr());
                    let suspended_entries = (output - (unwind_dst.as_ptr() as usize)) / 8;
                    let mut entries = unwind_dst[0..suspended_entries].to_vec();
                    entries.reverse();
                    if !entries.is_empty() {
                        entries.first_mut().unwrap().offset += frame.offset as u32;
                        entries.iter().for_each(|entry| {
                            state.frames.push(Frame { id: entry.id, offset: entry.offset as usize })
                        });
                    }
                    true // todo: not necessary!
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
                vec![ret_suspend(self, next)]
            }
            NodeKind::Branch { condition, if_true, if_false } => {
                self.condition(condition, 8 * 4);
                vec![
                    ret_suspend(self, if_false),
                    ret_suspend(self, if_true),
                ]
            }
            NodeKind::Call { offset, call, next } => {
                self.ret_call(offset, call, next)
            }
            NodeKind::Final => {
                self.ret_final();
                vec![]
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
                mov_u32(self, 9, get_u32(Ref::Stack(0), bytes.as_slice()).0);
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

    fn ret_call(&mut self, offset: u32, call: NodeId, next: NodeId) -> Vec<ReturnInfo> {
        // store `data_stack` and lc to stack
        // increase `data_stack` by offset
        // bl to ret_suspend_call
        //   ret_suspend call
        // if ret != 0 branch to unwind
        //   restore lc from stack
        //   `unwind_stack_end` is destination address to write (0, node id)
        //   modify elements of unwind struct
        //   increase `unwind_stack_end` by 8 & ret
        // restore lc and `data_stack`
        // ret_suspend next

        let mut intermediate: VecAssembler<Aarch64Relocation> = VecAssembler::new(0); // todo: not sure why we need baseaddr
        let mut infos: Vec<ReturnInfo> = Vec::new();

        asm!(intermediate
            ; stp data_stack, lr, [sp, #-16]!
        );
        mov_u32(&mut intermediate, 13, offset);
        asm!(intermediate
            ; add data_stack, data_stack, x13
            ; bl >call
            ; cmp unwind_stack_end, unwind_stack
            ; b.ne >unwind
            ; ldp data_stack, lr, [sp], #16
        );
        infos.push(ret_suspend(&mut intermediate, next));

        asm!(intermediate
            ; call:
        );
        infos.push(ret_suspend(&mut intermediate, call));

        asm!(intermediate
            ; unwind:
        );
        mov_u32(&mut intermediate, 13, offset);
        mov_u32(&mut intermediate, 9, next.0);
        // todo: remove unnesessary instructions
        asm!(intermediate
            ; ldp xzr, lr, [sp], #16
            ; sub unwind_stack_end, unwind_stack_end, 8
            ; str w13, [unwind_stack_end]
            ; add unwind_stack_end, unwind_stack_end, 8
            ; stp wzr, w9, [unwind_stack_end]
            ; add unwind_stack_end, unwind_stack_end, 8
            ; ret
        );

        for info in &mut infos {
            info.from += self.offset;
            info.to += self.offset;
        }
        self.extend(&(intermediate.finalize().unwrap()));
        infos
    }

    fn ret_final(&mut self) {
        mov_u32(self, 0, 0);
        asm!(self
            ; add unwind_stack_end, xzr, unwind_stack
            ; ret
        );
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

fn mov_u32<T: DynasmApi>(api: &mut T, register: u32, value: u32) {
    let low = value as u16;
    let high = (value >> 16) as u16;
    asm!(api
        ; mov X(register), low as u64
        ; movk X(register), high as u32, lsl 16
    );
}

fn ret_suspend<T: DynasmApi>(api: &mut T, id: NodeId) -> ReturnInfo {
    let mut return_info = ReturnInfo { id, from: api.offset().0, to: 0 };

    mov_u32(api, 0, 1); // 1 element written
    mov_u32(api, 9, id.0);
    asm!(api
        ; stp wzr, w9, [unwind_stack, 0]
        ; add unwind_stack_end, unwind_stack, 8
        ; ret
    );

    return_info.to = api.offset().0;
    return_info
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
    fn offset(&self) -> AssemblyOffset { AssemblyOffset(self.offset) }
    fn push(&mut self, byte: u8) { todo!() }
    fn align(&mut self, alignment: usize, with: u8) { todo!() }
}