use std::collections::HashMap;
use std::num::Wrapping;
use crate::core::api::{Command, Condition, Node, NodeKind, Ref};
use crate::core::interpreter::{eval_command, eval_condition, get_u32, put_u32};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
struct NodeId(u32);

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
struct SmallStackRef(u8);

#[derive(Debug, Eq, PartialEq)]
struct SmallNodeId(i16);

impl SmallNodeId {
    fn get(&self, ctx: NodeId) -> NodeId {
        NodeId((ctx.0 as i32 + (self.0 as i32)) as u32)
    }
}

fn small_id(ctx: NodeId, id: NodeId) -> Option<SmallNodeId> {
    (id.0 as i64 - ctx.0 as i64).try_into().ok().map(|id| SmallNodeId(id))
}

fn small_ref(r: Ref) -> Option<SmallStackRef> {
    match r {
        Ref::Stack(offset) => { offset.try_into().ok().map(|id| SmallStackRef(id)) }
    }
}

impl Into<Ref> for SmallStackRef {
    fn into(self) -> Ref { Ref::Stack(self.0 as u32) }
}


#[derive(Debug, Eq, PartialEq)]
enum ComputedKind {
    NotComputed,

    Set4 { dst: SmallStackRef, value: Wrapping<u32>, next: SmallNodeId },
    Copy4 { dst: SmallStackRef, op: SmallStackRef, next: SmallNodeId },

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
                ComputedKind::Set4 { dst, value, next } => {
                    put_u32((*dst).into(), stack, *value);
                    current = next.get(current);
                }
                ComputedKind::Copy4 { dst, op , next } => {
                    put_u32((*dst).into(), stack, get_u32((*op).into(), stack));
                    current = next.get(current);
                }
            }
        }
    }

    fn get_kind(&mut self, node: NodeId) -> &ComputedKind {
        if *self.computed.get(node.0 as usize).unwrap() == ComputedKind::NotComputed {
            let computed_kind = match Self::get_final_kind(self.nodes.get(node.0 as usize).unwrap()) {
                NodeKind::Final => { ComputedKind::Final }
                NodeKind::Command { command, next } => {
                    let next = self.get_id(next);

                    let compressed = if let Some(next) = small_id(node, next) {
                        Self::get_compressed_command(&command, next)
                    } else { ComputedKind::NotComputed };

                    if compressed == ComputedKind::NotComputed {
                        self.computed_full.push(FullComputedKind::Command { command, next });
                        ComputedKind::Full((self.computed_full.len() - 1) as u32)
                    } else { compressed }
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

    fn get_compressed_command(command: &Command, next: SmallNodeId) -> ComputedKind {
        match command {
            Command::Set { dst, bytes }
            if small_ref(*dst).is_some() && bytes.len() == 4 => {
                ComputedKind::Set4 {
                    dst: small_ref(*dst).unwrap(),
                    value: get_u32(Ref::Stack(0), bytes.as_slice()),
                    next,
                }
            }
            Command::Copy { size: 4, dst, op }
            if small_ref(*dst).is_some() && small_ref(*op).is_some() => {
                ComputedKind::Copy4 {
                    dst: small_ref(*dst).unwrap(),
                    op: small_ref(*op).unwrap(),
                    next
                }
            }

            _ => { ComputedKind::NotComputed }
        }
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