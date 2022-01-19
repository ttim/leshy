use std::any::type_name;
use std::fmt::{Debug, Formatter};
use std::io::{Read, Seek, SeekFrom};
use lazycell::LazyCell;

pub struct Lazy<T> {
    pub(crate) start_offset: u32,
    pub(crate) finish_offset: u32,
    // todo: replace with OnceCell once it's in stable rust
    pub(crate) cell: LazyCell<T>,
}

pub trait Readable: Sized {
    type Error: Debug;

    fn read(src: &mut (impl Read + Seek), finish_offset: u64) -> Result<Self, Self::Error>;
}

impl<T: Readable> Lazy<T> {
    pub fn get(&self, src: &mut (impl Read + Seek)) -> &T {
        self.cell.borrow_with(|| {
            src.seek(SeekFrom::Start(self.start_offset as u64)).unwrap();
            let result = T::read(src, self.finish_offset as u64).unwrap();
            assert_eq!(self.finish_offset as u64, src.stream_position().unwrap());
            result
        })
    }
}

impl<T: Debug> Debug for Lazy<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.cell.filled() {
            self.cell.borrow().unwrap().fmt(f)
        } else {
            f.write_str(&format!("<lazy {} {}..{}>", type_name::<T>(), self.start_offset, self.finish_offset))
        }
    }
}
