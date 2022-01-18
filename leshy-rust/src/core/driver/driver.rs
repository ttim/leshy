use std::collections::HashMap;
use crate::core::api::{Node, NodeKind};
use crate::core::driver::interpreter::Interpreter;
use crate::core::simple_interpreter::get_final_kind;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct NodeId(pub u32);

impl NodeId {
    pub fn next(self) -> NodeId { NodeId(self.0 + 1) }
}

pub struct Frame {
    pub id: NodeId,
    pub offset: usize,
}

pub struct CallCtx {
    pub callstack: Vec<Frame>,
}

pub struct Driver<N: Node> {
    nodes: Vec<N>,
    idx: HashMap<N, NodeId>,

    interpreter: Interpreter,
}

impl<N: Node> Driver<N> {
    pub fn new() -> Driver<N> {
        Driver {
            nodes: vec![],
            idx: HashMap::new(),
            interpreter: Interpreter::new()
        }
    }

    pub fn eval(&mut self, node: N, stack: &mut [u8]) {
        let id = self.get_id(node);
        let mut ctx = CallCtx { callstack: vec![Frame { id, offset: 0 }] };
        self.eval_inner(&mut ctx, stack);
        assert!(ctx.callstack.is_empty());
    }

    fn eval_inner(&mut self, ctx: &mut CallCtx, stack: &mut[u8]) {
        while !ctx.callstack.is_empty() {
            if self.interpreter.run(ctx, stack) {
                let id_to_register = ctx.callstack.last().unwrap().id;
                let kind = self.get_kind(id_to_register);
                self.interpreter.register(id_to_register, kind);
            }
        }
    }

    fn get_kind(&mut self, node: NodeId) -> NodeKind<NodeId> {
        match get_final_kind(self.nodes.get(node.0 as usize).unwrap()) {
            NodeKind::Command { command, next } => {
                NodeKind::Command { command, next: self.get_id(next) }
            }
            NodeKind::Branch { condition, if_true, if_false } => {
                NodeKind::Branch { condition, if_true: self.get_id(if_true), if_false: self.get_id(if_false) }
            }
            NodeKind::Call { offset, call, next } => {
                NodeKind::Call { offset, call: self.get_id(call), next: self.get_id(next) }
            }
            NodeKind::Final => { NodeKind::Final }
        }
    }

    fn get_id(&mut self, node: N) -> NodeId {
        if self.idx.contains_key(&node) {
            *self.idx.get(&node).unwrap()
        } else {
            self.nodes.push(node.clone());
            let id = NodeId((self.nodes.len() - 1) as u32);
            self.idx.insert(node, id);
            id
        }
    }
}
