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

impl NodeId {
    fn next(self) -> NodeId { NodeId(self.0 + 1) }
}

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
    Set4N { dst: SmallStackRef, value: Wrapping<u32> },
    Copy4N { dst: SmallStackRef, op: SmallStackRef },

    Add4 { dst: SmallStackRef, op1: SmallStackRef, op2: SmallStackRef, next: SmallNodeId},
    Add4N { dst: SmallStackRef, op1: SmallStackRef, op2: SmallStackRef},
    Sub4 { dst: SmallStackRef, op1: SmallStackRef, op2: SmallStackRef, next: SmallNodeId},
    Sub4N { dst: SmallStackRef, op1: SmallStackRef, op2: SmallStackRef},

    Eq4 { op1: SmallStackRef, op2: SmallStackRef, if_true: SmallNodeId, if_false: SmallNodeId },
    Ne04 { op: SmallStackRef, if_true: SmallNodeId, if_false: SmallNodeId },

    Call { offset: SmallStackRef, call: SmallNodeId, next: SmallNodeId },

    Full(u32),
    Final,
}

#[derive(Debug)]
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

    pub fn print_stats(&self) {
        println!("full kinds:");
        self.computed_full.iter().for_each(|kind| println!("{:?}", kind));
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
                ComputedKind::Set4N { dst, value } => {
                    put_u32((*dst).into(), stack, *value);
                    current = current.next();
                }
                ComputedKind::Copy4 { dst, op , next } => {
                    put_u32((*dst).into(), stack, get_u32((*op).into(), stack));
                    current = next.get(current);
                }
                ComputedKind::Copy4N { dst, op  } => {
                    put_u32((*dst).into(), stack, get_u32((*op).into(), stack));
                    current = current.next();
                }
                ComputedKind::Add4 { dst, op1, op2, next } => {
                    put_u32((*dst).into(), stack, get_u32((*op1).into(), stack) + get_u32((*op2).into(), stack));
                    current = next.get(current);
                }
                ComputedKind::Add4N { dst, op1, op2 } => {
                    put_u32((*dst).into(), stack, get_u32((*op1).into(), stack) + get_u32((*op2).into(), stack));
                    current = current.next();
                }
                ComputedKind::Sub4 { dst, op1, op2, next } => {
                    put_u32((*dst).into(), stack, get_u32((*op1).into(), stack) - get_u32((*op2).into(), stack));
                    current = next.get(current);
                }
                ComputedKind::Sub4N { dst, op1, op2 } => {
                    put_u32((*dst).into(), stack, get_u32((*op1).into(), stack) - get_u32((*op2).into(), stack));
                    current = current.next();
                }
                ComputedKind::Eq4 { op1, op2, if_true, if_false } => {
                    current = if get_u32((*op1).into(), stack) == get_u32((*op2).into(), stack) {
                        if_true.get(current)
                    } else {
                        if_false.get(current)
                    }
                }
                ComputedKind::Ne04 { op, if_true, if_false } => {
                    current = if get_u32((*op).into(), stack) != Wrapping(0) {
                        if_true.get(current)
                    } else {
                        if_false.get(current)
                    }
                }
                ComputedKind::Call { offset, call, next } => {
                    let offset_deref = offset.0 as usize;
                    let call_deref = call.get(current);
                    let next_deref = next.get(current);
                    self.eval_id(call_deref, &mut stack[offset_deref..]);
                    current = next_deref;
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

                    let compressed = if small_id(node, if_true).is_some() && small_id(node, if_false).is_some() {
                        Self::get_compressed_condition(&condition, small_id(node, if_true).unwrap(), small_id(node, if_false).unwrap())
                    } else { ComputedKind::NotComputed };

                    if compressed == ComputedKind::NotComputed {
                        self.computed_full.push(FullComputedKind::Branch { condition, if_true, if_false });
                        ComputedKind::Full((self.computed_full.len() - 1) as u32)
                    } else { compressed }
                }
                NodeKind::Call { offset, call, next } => {
                    let call = self.get_id(call);
                    let next = self.get_id(next);

                    if small_id(node, call).is_some() && small_id(node, next).is_some() && offset <= u8::MAX as u32 {
                        ComputedKind::Call {
                            offset: SmallStackRef(offset as u8),
                            call: small_id(node, call).unwrap(),
                            next: small_id(node, next).unwrap()
                        }
                    } else {
                        self.computed_full.push(FullComputedKind::Call { offset, call, next });
                        ComputedKind::Full((self.computed_full.len() - 1) as u32)
                    }
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
                if next == SmallNodeId(1) {
                    ComputedKind::Set4N {
                        dst: small_ref(*dst).unwrap(),
                        value: get_u32(Ref::Stack(0), bytes.as_slice()),
                    }
                } else {
                    ComputedKind::Set4 {
                        dst: small_ref(*dst).unwrap(),
                        value: get_u32(Ref::Stack(0), bytes.as_slice()),
                        next,
                    }
                }
            }
            Command::Copy { size: 4, dst, op }
            if small_ref(*dst).is_some() && small_ref(*op).is_some() => {
                if next == SmallNodeId(1) {
                    ComputedKind::Copy4N {
                        dst: small_ref(*dst).unwrap(),
                        op: small_ref(*op).unwrap()
                    }
                } else {
                    ComputedKind::Copy4 {
                        dst: small_ref(*dst).unwrap(),
                        op: small_ref(*op).unwrap(),
                        next
                    }
                }
            }
            Command::Add { size: 4, dst, op1, op2 }
            if small_ref(*dst).is_some() && small_ref(*op1).is_some() && small_ref(*op2).is_some() => {
                if next == SmallNodeId(1) {
                    ComputedKind::Add4N {
                        dst: small_ref(*dst).unwrap(),
                        op1: small_ref(*op1).unwrap(),
                        op2: small_ref(*op2).unwrap()
                    }
                } else {
                    ComputedKind::Add4 {
                        dst: small_ref(*dst).unwrap(),
                        op1: small_ref(*op1).unwrap(),
                        op2: small_ref(*op2).unwrap(),
                        next
                    }
                }
            }
            Command::Sub { size: 4, dst, op1, op2 }
            if small_ref(*dst).is_some() && small_ref(*op1).is_some() && small_ref(*op2).is_some() => {
                if next == SmallNodeId(1) {
                    ComputedKind::Sub4N {
                        dst: small_ref(*dst).unwrap(),
                        op1: small_ref(*op1).unwrap(),
                        op2: small_ref(*op2).unwrap()
                    }
                } else {
                    ComputedKind::Sub4 {
                        dst: small_ref(*dst).unwrap(),
                        op1: small_ref(*op1).unwrap(),
                        op2: small_ref(*op2).unwrap(),
                        next
                    }
                }
            }
            _ => { ComputedKind::NotComputed }
        }
    }

    fn get_compressed_condition(condition: &Condition, if_true: SmallNodeId, if_false: SmallNodeId) -> ComputedKind {
        match condition {
            Condition::Eq { size: 4, op1, op2 } if small_ref(*op1).is_some() && small_ref(*op2).is_some() => {
                ComputedKind::Eq4 { op1: small_ref(*op1).unwrap(), op2: small_ref(*op2).unwrap(), if_true, if_false }
            }
            Condition::Ne0 { size: 4, op } if small_ref(*op).is_some() => {
                ComputedKind::Ne04 { op: small_ref(*op).unwrap(), if_true, if_false }
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