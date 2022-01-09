use std::collections::HashMap;
use crate::core::api::{Command, Condition, Node, NodeKind};
use crate::core::interpreter::{eval_command, eval_condition};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
struct NodeId(u32);

#[derive(Debug, Eq, PartialEq)]
enum ComputedKind {
    NotComputed,
    ComputedCommand { command: Command, next: NodeId },
    ComputedBranch { condition: Condition, if_true: NodeId, if_false: NodeId },
    ComputedCall { offset: u32, call: NodeId, next: NodeId },
    ComputedFinal,
}

pub struct Interpreter<N: Node> {
    nodes: Vec<N>,
    idx: HashMap<N, NodeId>,
    computed: Vec<ComputedKind>,
}

impl<N: Node> Interpreter<N> {
    pub fn new() -> Interpreter<N> {
        Interpreter {
            nodes: vec![],
            idx: HashMap::new(),
            computed: vec![]
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
                ComputedKind::NotComputed => { panic!("can't happen") }
                ComputedKind::ComputedCommand { command, next } => {
                    eval_command(command, stack);
                    current = *next;
                }
                ComputedKind::ComputedBranch { condition, if_true, if_false } => {
                    if eval_condition(condition, stack) {
                        current = *if_true;
                    } else {
                        current = *if_false;
                    }
                }
                ComputedKind::ComputedCall { offset, call, next } => {
                    let offset_deref = *offset as usize;
                    let call_deref = *call;
                    let next_deref = *next;
                    self.eval_id(call_deref, &mut stack[offset_deref..]);
                    current = next_deref;
                }
                ComputedKind::ComputedFinal => { break; }
            }
        }
    }

    fn get_kind(&mut self, node: NodeId) -> &ComputedKind {
        if *self.computed.get(node.0 as usize).unwrap() == ComputedKind::NotComputed {
            let computed_kind = match self.nodes.get(node.0 as usize).unwrap().get() {
                NodeKind::Command { command, next } => {
                    ComputedKind::ComputedCommand { command, next: self.get_id(next) }
                }
                NodeKind::Branch { condition, if_true, if_false } => {
                    ComputedKind::ComputedBranch { condition, if_true: self.get_id(if_true), if_false: self.get_id(if_false) }
                }
                NodeKind::Call { offset, call, next } => {
                    ComputedKind::ComputedCall { offset, call: self.get_id(call), next: self.get_id(next) }
                }
                NodeKind::Final => { ComputedKind::ComputedFinal }
            };
            *self.computed.get_mut(node.0 as usize).unwrap() = computed_kind;
        }
        self.computed.get(node.0 as usize).unwrap()
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
