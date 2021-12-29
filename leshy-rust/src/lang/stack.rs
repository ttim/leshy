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
        self.bytes[self.offset..self.offset + bytes.len()].copy_from_slice(bytes);
        self.size += bytes.len();
    }

    pub fn extend(&mut self, size: usize) {
        self.size += size;
    }

    pub fn shrink(&mut self, size: usize) {
        self.size -= size;
    }

    pub fn as_slice(&self, offset: usize) -> &[u8] {
        &self.bytes[(self.offset+offset)..self.size]
    }

    pub fn as_slice_mut(&mut self, offset: usize) -> &mut [u8] {
        &mut self.bytes[self.offset+offset..self.size]
    }

    pub fn frame_offset(&self, raw_offset: i32) -> usize {
        if raw_offset >= 0 { raw_offset as usize } else { ((self.size - self.offset) as i32 + raw_offset) as usize }
    }

    pub fn check_frame_size(&self, size: usize) {
        assert_eq!(size, self.size - self.offset)
    }

    pub fn move_forward(&mut self, offset: usize) {
        self.offset += offset;
    }

    pub fn move_back(&mut self, offset: usize) {
        self.offset -= offset;
    }
}
