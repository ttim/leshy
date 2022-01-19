use std::collections::HashMap;
use crate::core::api::{Node, NodeKind};
use crate::core::interpreter::get_final_kind;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct NodeId(pub u32);

impl NodeId {
    pub fn next(self) -> NodeId { NodeId(self.0 + 1) }
}

pub trait Engine {
    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>);

    // returns true - suspended on unknown node, false - otherwise
    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool;
}

pub struct Frame {
    pub id: NodeId,
    // offset represents with what offset *this and subsequent* nodes should be executed
    pub offset: usize,
}

pub struct RunState {
    pub frames: Vec<Frame>,
}

impl RunState {
    pub fn offset(&self) -> usize { self.frames.iter().map(|f| f.offset).sum() }
}

pub struct Driver<N: Node, E: Engine> {
    nodes: Vec<N>,
    idx: HashMap<N, NodeId>,

    engine: E,
}

impl<N: Node, E: Engine> Driver<N, E> {
    pub fn new(engine: E) -> Driver<N, E> {
        Driver {
            nodes: vec![],
            idx: HashMap::new(),
            engine
        }
    }

    pub fn eval(&mut self, node: N, stack: &mut [u8]) {
        let id = self.get_id(node);
        let mut ctx = RunState { frames: vec![Frame { id, offset: 0 }] };
        self.eval_inner(&mut ctx, stack);
        assert!(ctx.frames.is_empty());
    }

    fn eval_inner(&mut self, ctx: &mut RunState, stack: &mut[u8]) {
        while !ctx.frames.is_empty() {
            if self.engine.run(ctx, stack) {
                let id_to_register = ctx.frames.last().unwrap().id;
                let kind = self.get_kind(id_to_register);
                self.engine.register(id_to_register, kind);
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
