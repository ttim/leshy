use std::hash::{Hash, Hasher};
use std::rc::Rc;
use crate::api::{Node, NodeKind};
use crate::webasm::ast::{FuncIdx, InstructionIdx, Module};

// todo: seems wrong
struct Source {
    uuid: u128,
    bytes: Vec<u8>,
    module: Module,
}

impl Hash for Source {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u128(self.uuid)
    }
}

impl PartialEq for Source {
    fn eq(&self, other: &Self) -> bool {
        self.uuid == other.uuid
    }
}

#[derive(Hash, PartialEq)]
struct WebAsmNode {
    source: Rc<Source>,
    func: FuncIdx,
    inst: InstructionIdx,
}

impl Node for WebAsmNode {
    fn get(&self) -> NodeKind<Self> {
        todo!()
    }
}
