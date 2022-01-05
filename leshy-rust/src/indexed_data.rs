// first line for test

use std::fs::File;
use std::rc::Rc;
use memmap::{Mmap, MmapOptions};

// Index to represent file being split into lines
struct Index<'a> {
    lines: Vec<&'a str>,
}

impl <'a> Index<'a> {
    fn line(&self, num: usize) -> &'a str {
        &self.lines[num]
    }
}

struct DataWithIndex {
    data: Rc<Mmap>,
    // todo: what else it can be, how to make it clear lifetime is equal to lifetime of DataWithIndex?!?
    // found 'this lifetime you can use with self referential structs and ouroboros (https://docs.rs/ouroboros/latest/ouroboros/)
    index: Index<'static>,
}

impl DataWithIndex {
    pub fn index(&self) -> &Index {
        &self.index
    }
}

fn build_index(bytes: &[u8]) -> Index {
    Index { lines: std::str::from_utf8(bytes).unwrap().split("\n").collect::<Vec<&str>>() }
}

fn create_index(mmap: Mmap) -> DataWithIndex {
    // this seems unnecessary but otherwise we both borrow & reference data
    let rc = Rc::from(mmap);
    let data = rc.clone();
    let index = build_index(&&rc); // one for Rc, and one for Mmap
    // transmute is safe because we don't expose index directly and it always outlives DataWithIndex
    unsafe { DataWithIndex { data: data, index: std::mem::transmute::<Index, Index<'static>>(index) } }
}

#[test]
fn test_from_memory() {
    let file = File::open("src/indexed_data.rs").unwrap();
    let mmap = unsafe { MmapOptions::new().map(&file).unwrap() };
    let data_with_index = create_index(mmap);

    assert_eq!("// first line for test", data_with_index.index().line(0));
}
