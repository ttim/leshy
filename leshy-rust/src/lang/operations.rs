pub fn equal(len: usize, r1: &[u8], r2: &[u8]) -> bool {
    todo!()
}

pub fn less(len: usize, r1: &[u8], r2: &[u8], or_equal: bool) -> bool {
    todo!()
}

pub fn as_i32(r: &[u8]) -> Option<i32> {
    r[0..4].try_into().map(i32::from_le_bytes).ok()
}
