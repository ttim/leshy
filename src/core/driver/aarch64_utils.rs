use std::collections::HashMap;
use dynasm::dynasm;
use dynasmrt::aarch64::Aarch64Relocation;
use dynasmrt::DynasmApi;
use dynasmrt::relocations::Relocation;
use dynasmrt::mmap::MutableBuffer;
use lazy_static::lazy_static;
use libc::size_t;

lazy_static! {
    static ref BRANCH: HashMap<&'static str, Vec<u8>> = {
        let mut m = HashMap::new();
        // todo: got this from debuging internals of generated dynasm code. figure out where it comes from in macro!
        m.insert("ne", vec![1, 0, 0, 84]);
        m
    };
}

pub fn flush_code_cache(buffer: &MutableBuffer) {
    // flush needed for M1 macs: https://developer.apple.com/documentation/apple-silicon/porting-just-in-time-compilers-to-apple-silicon
    unsafe { sys_icache_invalidate(buffer.as_ptr(), buffer.size()); }
}

extern {
    pub fn sys_icache_invalidate(start: *const u8, len: size_t);
}

pub fn mov_u32<T: DynasmApi>(api: &mut T, register: u32, value: u32) {
    let low = value as u16;
    let high = (value >> 16) as u16;
    dynasm!(api
        ; .arch aarch64
        ; mov X(register), low as u64
        ; movk X(register), high as u32, lsl 16
    );
}

pub fn mov_u32_high<T: DynasmApi>(api: &mut T, register: u32, value: u32) {
    let low = value as u16;
    let high = (value >> 16) as u16;
    dynasm!(api
        ; .arch aarch64
        ; movk X(register), low as u32, lsl 32
        ; movk X(register), high as u32, lsl 48
    );
}

pub fn bcond<T: DynasmApi>(api: &mut T, modifier: &'static str, offset: isize) {
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