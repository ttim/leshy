use std::collections::HashMap;
use crate::core::api::{Command, Condition, Node, NodeKind};
use crate::core::interpreter::{eval_command, eval_condition};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
struct NodeId(u32);

#[derive(Debug, Eq, PartialEq)]
enum ComputedKind {
    NotComputed,

    Full(u32),
    Final,
}

enum FullComputedKind {
    Command { command: Command, next: NodeId },
    Branch { condition: Condition, if_true: NodeId, if_false: NodeId },
    Call { offset: u32, call: NodeId, next: NodeId },
}

pub struct Interpreter<N: Node> {
    nodes: Vec<N>,
    idx: HashMap<N, NodeId>,
    computed: Vec<ComputedKind>,
    computed_full: Vec<FullComputedKind>,
}

impl<N: Node> Interpreter<N> {
    pub fn new() -> Interpreter<N> {
        Interpreter {
            nodes: vec![],
            idx: HashMap::new(),
            computed: vec![],
            computed_full: vec![],
        }
    }

    pub fn eval(&mut self, node: N, stack: &mut [u8]) {
        let id = self.get_id(node);
        self.eval_id(id, stack)
    }

    fn eval_id(&mut self, node: NodeId, stack: &mut[u8]) {
        let mut current = node;
        loop {
            match self.get_kind(current) {
                ComputedKind::Final => { break; }
                ComputedKind::Full(id) => {
                    let id = *id as usize;
                    match self.computed_full.get(id).unwrap() {
                        FullComputedKind::Command { command, next } => {
                            eval_command(command, stack);
                            current = *next;
                        }
                        FullComputedKind::Branch { condition, if_true, if_false } => {
                            if eval_condition(condition, stack) {
                                current = *if_true;
                            } else {
                                current = *if_false;
                            }
                        }
                        FullComputedKind::Call { offset, call, next } => {
                            let offset_deref = *offset as usize;
                            let call_deref = *call;
                            let next_deref = *next;
                            self.eval_id(call_deref, &mut stack[offset_deref..]);
                            current = next_deref;
                        }
                    }
                }
                ComputedKind::NotComputed => { panic!("can't happen") }
            }
        }
    }

    fn get_kind(&mut self, node: NodeId) -> &ComputedKind {
        if *self.computed.get(node.0 as usize).unwrap() == ComputedKind::NotComputed {
            let computed_kind = match Self::get_final_kind(self.nodes.get(node.0 as usize).unwrap()) {
                // compressed
                NodeKind::Final => { ComputedKind::Final }
                // full
                NodeKind::Command { command, next } => {
                    let next = self.get_id(next);
                    self.computed_full.push(FullComputedKind::Command { command, next });
                    ComputedKind::Full((self.computed_full.len() - 1) as u32)
                }
                NodeKind::Branch { condition, if_true, if_false } => {
                    let if_true = self.get_id(if_true);
                    let if_false = self.get_id(if_false);
                    self.computed_full.push(FullComputedKind::Branch { condition, if_true, if_false });
                    ComputedKind::Full((self.computed_full.len() - 1) as u32)
                }
                NodeKind::Call { offset, call, next } => {
                    let call = self.get_id(call);
                    let next = self.get_id(next);
                    self.computed_full.push(FullComputedKind::Call { offset, call, next });
                    ComputedKind::Full((self.computed_full.len() - 1) as u32)
                }
            };
            *self.computed.get_mut(node.0 as usize).unwrap() = computed_kind;
        }
        self.computed.get(node.0 as usize).unwrap()
    }

    fn get_final_kind(node: &N) -> NodeKind<N> {
        let kind = node.get();
        match kind {
            // noop ops
            NodeKind::Command { command: Command::Noop, next } => { Self::get_final_kind(&next) }
            NodeKind::Command { command: Command::PoisonFrom { .. }, next } => { Self::get_final_kind(&next) }

            NodeKind::Command { .. } => { kind }
            NodeKind::Branch { .. } => { kind }
            NodeKind::Call { .. } => { kind }
            NodeKind::Final => { kind }
        }
    }

    fn get_id(&mut self, node: N) -> NodeId {
        if self.idx.contains_key(&node) {
            *self.idx.get(&node).unwrap()
        } else {
            self.nodes.push(node.clone());
            let id = NodeId((self.nodes.len() - 1) as u32);
            self.idx.insert(node, id);
            self.computed.push(ComputedKind::NotComputed);
            id
        }
    }
}

#[test]
fn test_sizes() {
    assert_eq!(8, std::mem::size_of::<ComputedKind>());
    assert_eq!(40, std::mem::size_of::<FullComputedKind>());
}