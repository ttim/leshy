use std::collections::HashMap;
use dynasm::dynasm;
use dynasmrt::aarch64::Aarch64Relocation;
use dynasmrt::{AssemblyOffset, DynasmApi, VecAssembler};
use dynasmrt::relocations::Relocation;
use dynasmrt::mmap::MutableBuffer;
use lazy_static::lazy_static;
use libc::size_t;
use crate::core::api::{Command, Condition, NodeKind, Ref};
use crate::core::driver::code_generator_engine::ReturnInfo;
use crate::core::driver::driver::NodeId;
use dynasmrt::DynasmLabelApi;

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

lazy_static! {
    static ref BRANCH: HashMap<&'static str, Vec<u8>> = {
        let mut m = HashMap::new();
        // todo: got this from debuging internals of generated dynasm code. figure out where it comes from in macro!
        m.insert("ne", vec![1, 0, 0, 84]);
        m
    };
}

pub fn insert_debug<T: DynasmApi>(api: &mut T, id: NodeId, debug_fn: extern "C" fn (*const u8, *const u8, *const u8, u64)) {
    // save x0, x1, x2, x3, x4, lr; put id to x3; put debug_fn to x4; call x4; restore
    asm!(api
        ; stp x0, x1, [sp, #-16]!
        ; stp x2, x3, [sp, #-16]!
        ; stp x4, lr, [sp, #-16]!
    );
    mov_u64(api, 3, id.0 as u64);
    mov_u64(api, 4, debug_fn as u64);
    asm!(api
        ; blr x4
        ; ldp x4, lr, [sp], #16
        ; ldp x2, x3, [sp], #16
        ; ldp x0, x1, [sp], #16
    );
}

// todo: kind should be more like NodeKind<NodeId | AssemblyOffset>
pub fn generate<T: DynasmApi>(api: &mut T, kind: NodeKind<NodeId>) -> Vec<ReturnInfo> {
    match kind {
        NodeKind::Command { command: command_value, next } => {
            command(api, command_value);
            vec![ret_suspend(api, next)]
        }
        NodeKind::Branch { condition: condition_value, if_true, if_false } => {
            // todo: remove this const by calculation
            condition(api, condition_value, 8 * 4);
            vec![
                ret_suspend(api, if_false),
                ret_suspend(api, if_true),
            ]
        }
        NodeKind::Call { offset, call, next } => {
            ret_call(api, offset, call, next)
        }
        NodeKind::Final => {
            ret_final(api);
            vec![]
        }
    }
}

fn ret_call<T: DynasmApi>(api: &mut T, offset: u32, call: NodeId, next: NodeId) -> Vec<ReturnInfo> {
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
        info.from.0 += api.offset().0;
        info.to.0 += api.offset().0;
    }
    api.extend(&(intermediate.finalize().unwrap()));
    infos
}

fn ret_final<T: DynasmApi>(api: &mut T) {
    mov_u32(api, 0, 0);
    asm!(api
        ; add unwind_stack_end, xzr, unwind_stack
        ; ret
    );
}

fn ret_suspend<T: DynasmApi>(api: &mut T, id: NodeId) -> ReturnInfo {
    let mut return_info = ReturnInfo { id, from: api.offset(), to: AssemblyOffset(0) };

    mov_u32(api, 0, 1); // 1 element written
    mov_u32(api, 9, id.0);
    asm!(api
        ; stp wzr, w9, [unwind_stack, 0]
        ; add unwind_stack_end, unwind_stack, 8
        ; ret
    );

    return_info.to = api.offset();
    return_info
}

fn command<T: DynasmApi>(api: &mut T, command: Command) {
    match command {
        Command::Noop => { panic!("can't happen") }
        Command::PoisonFrom { .. } => { panic!("can't happen") }
        Command::Set { dst, bytes } => { set(api, dst, bytes) }
        Command::Copy { dst, size, op } => { copy(api, size, dst, op) }
        Command::Add { size, dst, op1, op2 } => { add(api, size, dst, op1, op2) }
        Command::Sub { size, dst, op1, op2 } => { sub(api, size, dst, op1, op2) }
    }
}

fn set<T: DynasmApi>(api: &mut T, dst: Ref, bytes: Vec<u8>) {
    match bytes.len() {
        4 => {
            mov_u32(api, 9, u32::from_le_bytes(bytes.as_slice().try_into().unwrap()));
            store_u32(api, 9, dst);
        }
        8 => {
            mov_u64(api, 14, u64::from_le_bytes(bytes.as_slice().try_into().unwrap()));
            store_u64(api, 14, dst);
        }
        _ => { todo!() }
    }
}

fn copy<T: DynasmApi>(api: &mut T, len: u32, dst: Ref, op: Ref) {
    match len {
        4 => {
            load_u32(api, 9, op);
            store_u32(api, 9, dst);
        }
        8 => {
            load_u64(api, 14, op);
            store_u64(api, 14, dst);
        }
        _ => { todo!() }
    }
}

fn add<T: DynasmApi>(api: &mut T, len: u32, dst: Ref, op1: Ref, op2: Ref) {
    match len {
        4 => {
            load_u32(api, 9, op1);
            load_u32(api, 10, op2);
            asm!(api
                ; add w11, w9, w10
            );
            store_u32(api, 11, dst);
        }
        8 => {
            load_u64(api, 14, op1);
            load_u64(api, 15, op2);
            asm!(api
                ; add x16, x14, x15
            );
            store_u64(api, 16, dst);
        }
        _ => { todo!() }
    }
}

fn sub<T: DynasmApi>(api: &mut T, len: u32, dst: Ref, op1: Ref, op2: Ref) {
    match len {
        4 => {
            load_u32(api, 9, op1);
            load_u32(api, 10, op2);
            asm!(api
                ; sub w11, w9, w10
            );
            store_u32(api, 11, dst);
        }
        _ => { todo!() }
    }
}

fn condition<T: DynasmApi>(api: &mut T, condition: Condition, ret_true_offset: isize) {
    match condition {
        Condition::Ne { size, op1, op2 } => { ne(api,size, op1, op2, ret_true_offset) }
        Condition::Ne0 { size, op } => { ne0(api, size, op, ret_true_offset) }
    }
}

fn ne<T: DynasmApi>(api: &mut T, len: u32, op1: Ref, op2: Ref, ret_true_offset: isize) {
    match len {
        4 => {
            load_u32(api, 9, op1);
            load_u32(api, 10, op2);
            asm!(api
                ; cmp w9, w10
            );
            bcond(api, "ne", ret_true_offset);
        }
        _ => { todo!() }
    }
}

fn ne0<T: DynasmApi>(api: &mut T, len: u32, op: Ref, ret_true_offset: isize) {
    match len {
        4 => {
            load_u32(api, 9, op);
            asm!(api
                ; cmp w9, 0
            );
            bcond(api, "ne", ret_true_offset);
        }
        _ => { todo!() }
    }
}

fn store_u32<T: DynasmApi>(api: &mut T, register: u32, dst: Ref) {
    match dst {
        Ref::Stack(offset) => {
            // todo: check for stack overflow
            // todo: what if offset is big?
            asm!(api
                ; str W(register), [data_stack, offset]
            );
        }
    }
}

fn store_u64<T: DynasmApi>(api: &mut T, register: u32, dst: Ref) {
    match dst {
        Ref::Stack(offset) => {
            // todo: check for stack overflow
            // todo: what if offset is big?
            asm!(api
                ; str X(register), [data_stack, offset]
            );
        }
    }
}

fn load_u32<T: DynasmApi>(api: &mut T, register: u32, op: Ref) {
    match op {
        Ref::Stack(offset) => {
            // todo: check for stack overflow
            // todo: what if offset is big?
            asm!(api
                ; ldr W(register), [data_stack, offset]
            );
        }
    }
}

fn load_u64<T: DynasmApi>(api: &mut T, register: u32, op: Ref) {
    match op {
        Ref::Stack(offset) => {
            // todo: check for stack overflow
            // todo: what if offset is big?
            asm!(api
                ; ldr X(register), [data_stack, offset]
            );
        }
    }
}

pub fn flush_code_cache(buffer: &MutableBuffer) {
    // flush needed for M1 macs: https://developer.apple.com/documentation/apple-silicon/porting-just-in-time-compilers-to-apple-silicon
    unsafe { sys_icache_invalidate(buffer.as_ptr(), buffer.size()); }
}

extern {
    pub fn sys_icache_invalidate(start: *const u8, len: size_t);
}

fn mov_u32<T: DynasmApi>(api: &mut T, register: u32, value: u32) {
    let low = value as u16;
    let high = (value >> 16) as u16;
    asm!(api
        ; mov X(register), low as u64
        ; movk X(register), high as u32, lsl 16
    );
}

fn mov_u64<T: DynasmApi>(api: &mut T, register: u32, value: u64) {
    asm!(api
        ; mov X(register), (value as u16) as u64
        ; movk X(register), ((value >> 16) as u16) as u32, lsl 16
        ; movk X(register), ((value >> 32) as u16) as u32, lsl 32
        ; movk X(register), ((value >> 48) as u16) as u32, lsl 48
    );
}

fn mov_u32_high<T: DynasmApi>(api: &mut T, register: u32, value: u32) {
    let low = value as u16;
    let high = (value >> 16) as u16;
    asm!(api
        ; movk X(register), low as u32, lsl 32
        ; movk X(register), high as u32, lsl 48
    );
}

fn bcond<T: DynasmApi>(api: &mut T, modifier: &'static str, offset: isize) {
    let mut bytes = BRANCH.get(modifier).unwrap().clone();
    Aarch64Relocation::BCOND.write_value(&mut bytes, offset);
    api.extend(bytes.as_slice());
}

pub fn b<T: DynasmApi>(api: &mut T, rel_dst: isize) {
    // todo: got this from debuging internals of generated dynasm code. figure out where it comes from in macro!
    let mut template = vec!(0, 0, 0, 20);
    Aarch64Relocation::B.write_value(&mut template, rel_dst);
    api.extend(template.as_slice());
}
