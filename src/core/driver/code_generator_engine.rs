use std::collections::HashMap;
use std::{io, mem};
use dynasmrt::{AssemblyOffset, DynasmApi, ExecutableBuffer};
use dynasmrt::mmap::MutableBuffer;
use crate::core::api::NodeKind;
use crate::core::driver::driver::{Engine, Frame, NodeId, RunState};
use crate::core::driver::aarch64::{b, flush_code_cache, generate};
use multimap::MultiMap;

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
pub struct ReturnInfo {
    pub id: NodeId,
    pub from: AssemblyOffset,
    pub to: AssemblyOffset,
}

pub struct CodeGeneratorEngine {
    size: usize,
    code: Option<ExecutableBuffer>,
    offset: AssemblyOffset,
    offsets: HashMap<NodeId, AssemblyOffset>,
    returns: MultiMap<NodeId, ReturnInfo>,
}

impl CodeGeneratorEngine {
    pub fn new(size: usize) -> io::Result<CodeGeneratorEngine> {
        Ok(CodeGeneratorEngine {
            size,
            code: Some(ExecutableBuffer::new(size)?),
            offset: AssemblyOffset(0),
            offsets: HashMap::new(),
            returns: MultiMap::new(),
        })
    }

    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        if self.offsets.contains_key(&id) { return; }

        let mut writable = match std::mem::replace(&mut self.code, None) {
            Some(buffer) => { buffer.make_mut().unwrap() }
            None => { panic!() }
        };
        // why do we need this?
        writable.set_len(self.size);
        self.remove_last_return_if_needed(id);
        let mut ops = Assembler { buffer: writable, offset: self.offset };
        let returns = generate(&mut ops, kind.clone());

        self.offsets.insert(id, self.offset);

        self.offset = ops.offset;

        // replace returns
        for ret in returns {
            if let Some(offset) = self.offsets.get(&ret.id) {
                // todo: what if more than 1mb size difference?
                ops.offset = ret.from;
                b(&mut ops, offset.0 as isize - ret.from.0 as isize);
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
            ops.offset = ret.from;
            b(&mut ops, offset.0 as isize - ret.from.0 as isize);
        }

        flush_code_cache(&ops.buffer);
        std::mem::replace(&mut self.code, Some(ops.buffer.make_exec().unwrap()));
    }

    fn remove_last_return_if_needed(&mut self, id: NodeId) {
        if let Some(returns) = self.returns.get_vec_mut(&id) {
            if let Some(pos) = returns.iter().position(|e| e.to == self.offset) {
                self.offset = returns.get(pos).unwrap().from;
                returns.remove(pos);
            }
        }
    }

    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool {
        if let Some(executable) = &self.code {
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

struct Assembler {
    buffer: MutableBuffer,
    offset: AssemblyOffset,
}

impl Extend<u8> for Assembler {
    fn extend<T: IntoIterator<Item=u8>>(&mut self, iter: T) { todo!() }
}

impl<'a> Extend<&'a u8> for Assembler {
    fn extend<T: IntoIterator<Item=&'a u8>>(&mut self, iter: T) {
        for byte in iter {
            *self.buffer.get_mut(self.offset.0).unwrap() = *byte;
            self.offset.0 += 1;
        }
    }
}

impl DynasmApi for Assembler {
    fn offset(&self) -> AssemblyOffset { self.offset }
    fn push(&mut self, byte: u8) { todo!() }
    fn align(&mut self, alignment: usize, with: u8) { todo!() }
}

impl Engine for CodeGeneratorEngine {
    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) { self.register(id, kind) }
    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool { self.run(state, stack) }
}
