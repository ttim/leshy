use crate::api::{Node, NodeKind};

#[derive(Hash, PartialEq)]
enum WebAsmNode {
}

impl Node for WebAsmNode {
    fn get(&self) -> NodeKind<Self> {
        todo!()
    }
}
