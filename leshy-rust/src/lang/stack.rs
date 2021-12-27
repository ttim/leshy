pub struct Stack {
    bytes: [u8; 1000],
    offset: usize,
    size: usize,
}

impl Stack {
    pub fn create() -> Stack {
        Stack { bytes: [0; 1000], offset: 0, size: 0 }
    }

    pub fn push(&mut self, bytes: &[u8]) {
        self.bytes.as_mut()[self.offset..self.offset + bytes.len()].copy_from_slice(bytes);
        self.size += bytes.len();
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.bytes.as_slice()[self.offset..self.size]
    }

    pub fn check_frame_size(&self, size: usize) {
        assert_eq!(size, self.size - self.offset)
    }
}
