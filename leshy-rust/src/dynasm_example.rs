use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};

use std::{slice, mem};
use dynasmrt::aarch64::Aarch64Relocation;

#[test]
fn test() {
    let mut ops = dynasmrt::aarch64::Assembler::new().unwrap();
    let mut ops = dynasmrt::VecAssembler::<Aarch64Relocation>::new(7777);

    let string = "Hello World!";

    dynasm!(ops
        ; .arch aarch64
        ; ->hello:
        ; .bytes string.as_bytes()
        ; .align 4
        ; ->print:
        ; .qword print as _
    );

    let hello = ops.offset();
    dynasm!(ops
        ; .arch aarch64
        ; adr x0, ->hello
        ; movz x1, string.len() as u32
        ; ldr x9, ->print
        ; str x30, [sp, #-16]!
        ; blr x9
        ; ldr x30, [sp], #16
        ; ret
    );


    // let reader = ops.reader();
    ops.finalize();
    // let buf = ops.finalize().unwrap();

    // let hello_fn: extern "C" fn() = unsafe { mem::transmute(reader.lock().ptr(hello)) };

    // hello_fn()
}

pub extern "C" fn print(buffer: *const u8, length: u64) {
    let str = String::from_utf8_lossy(unsafe { slice::from_raw_parts(buffer, length as usize) });
    println!("{}", str);
}

// generated code will return next node id to run
// ret node_id basically
// all positions of ret node_id are stored so when node_id get materialized into instruction they are replaced by jumps
// if there is ret node_id at last position in buffer and we materialize exact the same id we just overwrite this bytes with new instruction
//
// not sure it's safe to replace returns with jumps tho because of not enough guarantees on execution
// if it's impossible then we need to rewrite code from time to time with replaced refs
// it feels tho this way sometimes you want to come back to runtime to check node_id -> memory address correspondance, how?
