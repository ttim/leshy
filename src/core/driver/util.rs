use dynasmrt::mmap::MutableBuffer;
use libc::size_t;

pub fn flush_code_cache(buffer: &MutableBuffer) {
    // flush needed for M1 macs: https://developer.apple.com/documentation/apple-silicon/porting-just-in-time-compilers-to-apple-silicon
    unsafe { sys_icache_invalidate(buffer.as_ptr(), buffer.size()); }
}

extern {
    pub fn sys_icache_invalidate(start: *const u8, len: size_t);
}
