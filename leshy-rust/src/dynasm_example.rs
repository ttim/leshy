use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};

use std::{slice, mem};

#[test]
fn test() {
    let mut ops = dynasmrt::aarch64::Assembler::new().unwrap();
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


    let reader = ops.reader();
    ops.commit();
    // let buf = ops.finalize().unwrap();

    let hello_fn: extern "C" fn() = unsafe { mem::transmute(reader.lock().ptr(hello)) };

    hello_fn()
}

pub extern "C" fn print(buffer: *const u8, length: u64) {
    let str = String::from_utf8_lossy(unsafe { slice::from_raw_parts(buffer, length as usize) });
    println!("{}", str);
}