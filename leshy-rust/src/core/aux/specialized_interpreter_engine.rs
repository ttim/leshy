use std::num::Wrapping;
use crate::core::api::{Command, Condition, NodeKind, Ref};
use crate::core::driver::driver::{Frame, RunState, NodeId, Engine};
use crate::core::interpreter::{eval_command, eval_condition, get_u32, put_u32};

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
enum CompactKind {
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

pub struct SpecializedInterpreterEngine {
    computed: Vec<CompactKind>,
    full: Vec<NodeKind<NodeId>>,
}

static NOT_COMPUTED: CompactKind = CompactKind::NotComputed;

impl SpecializedInterpreterEngine {
    pub fn new() -> SpecializedInterpreterEngine {
        SpecializedInterpreterEngine { computed: Vec::new(), full: Vec::new() }
    }

    pub fn print_stats(&self) {
        println!("full kinds:");
        self.full.iter().for_each(|kind| println!("{:?}", kind));
    }

    pub fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        while self.computed.len() <= id.0 as usize {
            self.computed.push(CompactKind::NotComputed);
        }
        *self.computed.get_mut(id.0 as usize).unwrap() = self.compact(id, kind);
    }

    // returns true - suspended on unknown node, false - otherwise
    pub fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool {
        let frame = state.frames.pop().unwrap();
        match self.run_internal(frame.id, &mut stack[frame.offset..]) {
            None => { false }
            Some(mut suspended_in) => {
                suspended_in.reverse();
                suspended_in.iter_mut().for_each(|suspended_frame| suspended_frame.offset += frame.offset);
                state.frames.append(&mut suspended_in);
                true
            }
        }
    }

    fn run_internal(&self, node: NodeId, stack: &mut [u8]) -> Option<Vec<Frame>> {
        let mut current = node;
        loop {
            match self.computed.get(current.0 as usize).unwrap_or(&NOT_COMPUTED) {
                CompactKind::Final => { return None; }
                CompactKind::Set4 { dst, value, next } => {
                    put_u32((*dst).into(), stack, *value);
                    current = next.get(current);
                }
                CompactKind::Set4N { dst, value } => {
                    put_u32((*dst).into(), stack, *value);
                    current = current.next();
                }
                CompactKind::Copy4 { dst, op , next } => {
                    put_u32((*dst).into(), stack, get_u32((*op).into(), stack));
                    current = next.get(current);
                }
                CompactKind::Copy4N { dst, op  } => {
                    put_u32((*dst).into(), stack, get_u32((*op).into(), stack));
                    current = current.next();
                }
                CompactKind::Add4 { dst, op1, op2, next } => {
                    put_u32((*dst).into(), stack, get_u32((*op1).into(), stack) + get_u32((*op2).into(), stack));
                    current = next.get(current);
                }
                CompactKind::Add4N { dst, op1, op2 } => {
                    put_u32((*dst).into(), stack, get_u32((*op1).into(), stack) + get_u32((*op2).into(), stack));
                    current = current.next();
                }
                CompactKind::Sub4 { dst, op1, op2, next } => {
                    put_u32((*dst).into(), stack, get_u32((*op1).into(), stack) - get_u32((*op2).into(), stack));
                    current = next.get(current);
                }
                CompactKind::Sub4N { dst, op1, op2 } => {
                    put_u32((*dst).into(), stack, get_u32((*op1).into(), stack) - get_u32((*op2).into(), stack));
                    current = current.next();
                }
                CompactKind::Eq4 { op1, op2, if_true, if_false } => {
                    current = if get_u32((*op1).into(), stack) == get_u32((*op2).into(), stack) {
                        if_true.get(current)
                    } else {
                        if_false.get(current)
                    }
                }
                CompactKind::Ne04 { op, if_true, if_false } => {
                    current = if get_u32((*op).into(), stack) != Wrapping(0) {
                        if_true.get(current)
                    } else {
                        if_false.get(current)
                    }
                }
                CompactKind::Call { offset, call, next } => {
                    let offset = offset.0 as usize;
                    match self.run_internal(call.get(current), &mut stack[offset..]) {
                        None => {
                            current = next.get(current);
                        }
                        Some(mut suspended_trace) => {
                            return Some(Self::subcall_suspended_trace(suspended_trace, next.get(current), offset))
                        }
                    }
                }
                CompactKind::Full(id) => {
                    let id = *id as usize;
                    match self.full.get(id).unwrap() {
                        NodeKind::Command { command, next } => {
                            eval_command(command, stack);
                            current = *next;
                        }
                        NodeKind::Branch { condition, if_true, if_false } => {
                            if eval_condition(condition, stack) {
                                current = *if_true;
                            } else {
                                current = *if_false;
                            }
                        }
                        NodeKind::Call { offset, call, next } => {
                            let offset = *offset as usize;
                            match self.run_internal(*call, &mut stack[offset..]) {
                                None => {
                                    current = *next;
                                }
                                Some(mut suspended_trace) => {
                                    return Some(Self::subcall_suspended_trace(suspended_trace, *next, offset))
                                }
                            }
                        }
                        _ => { panic!("full command can't be neither of command, branch or call") }
                    }
                }
                CompactKind::NotComputed => { return Some(vec![Frame { id: current, offset: 0 }]); }
            }
        }
    }

    fn subcall_suspended_trace(mut trace: Vec<Frame>, next: NodeId, offset: usize) -> Vec<Frame> {
        // suspended in sub call
        trace.iter_mut().for_each(|e| e.offset += offset);
        trace.push(Frame { id: next, offset: 0 });
        trace
    }

    fn compact(&mut self, ctx: NodeId, kind: NodeKind<NodeId>) -> CompactKind {
        match &kind {
            NodeKind::Command { command, next } => {
                if let Some(next) = small_id(ctx, *next) {
                    return self.compact_command(command, next, ctx)
                }
            }
            NodeKind::Branch { condition, if_true, if_false } => {
                if let Some(if_true) = small_id(ctx, *if_true) {
                    if let Some(if_false) = small_id(ctx, *if_false) {
                        return self.compact_condition(condition, if_true, if_false, ctx)
                    }
                }
            }
            NodeKind::Call { offset, call, next } => {
                if let Some(call) = small_id(ctx, *call) {
                    if let Some(next) = small_id(ctx, *next) {
                        if *offset <= u8::MAX as u32 {
                            return CompactKind::Call { offset: SmallStackRef(*offset as u8), call, next }
                        }
                    }
                }
            }
            NodeKind::Final => {
                return CompactKind::Final
            }
        }
        self.full_kind(kind)
    }

    fn compact_command(&mut self, command: &Command, next: SmallNodeId, ctx: NodeId) -> CompactKind {
        match command {
            Command::Set { dst, bytes }
            if small_ref(*dst).is_some() && bytes.len() == 4 => {
                if next == SmallNodeId(1) {
                    CompactKind::Set4N {
                        dst: small_ref(*dst).unwrap(),
                        value: get_u32(Ref::Stack(0), bytes.as_slice()),
                    }
                } else {
                    CompactKind::Set4 {
                        dst: small_ref(*dst).unwrap(),
                        value: get_u32(Ref::Stack(0), bytes.as_slice()),
                        next,
                    }
                }
            }
            Command::Copy { size: 4, dst, op }
            if small_ref(*dst).is_some() && small_ref(*op).is_some() => {
                if next == SmallNodeId(1) {
                    CompactKind::Copy4N {
                        dst: small_ref(*dst).unwrap(),
                        op: small_ref(*op).unwrap(),
                    }
                } else {
                    CompactKind::Copy4 {
                        dst: small_ref(*dst).unwrap(),
                        op: small_ref(*op).unwrap(),
                        next,
                    }
                }
            }
            Command::Add { size: 4, dst, op1, op2 }
            if small_ref(*dst).is_some() && small_ref(*op1).is_some() && small_ref(*op2).is_some() => {
                if next == SmallNodeId(1) {
                    CompactKind::Add4N {
                        dst: small_ref(*dst).unwrap(),
                        op1: small_ref(*op1).unwrap(),
                        op2: small_ref(*op2).unwrap(),
                    }
                } else {
                    CompactKind::Add4 {
                        dst: small_ref(*dst).unwrap(),
                        op1: small_ref(*op1).unwrap(),
                        op2: small_ref(*op2).unwrap(),
                        next,
                    }
                }
            }
            Command::Sub { size: 4, dst, op1, op2 }
            if small_ref(*dst).is_some() && small_ref(*op1).is_some() && small_ref(*op2).is_some() => {
                if next == SmallNodeId(1) {
                    CompactKind::Sub4N {
                        dst: small_ref(*dst).unwrap(),
                        op1: small_ref(*op1).unwrap(),
                        op2: small_ref(*op2).unwrap(),
                    }
                } else {
                    CompactKind::Sub4 {
                        dst: small_ref(*dst).unwrap(),
                        op1: small_ref(*op1).unwrap(),
                        op2: small_ref(*op2).unwrap(),
                        next,
                    }
                }
            }
            _ => {
                self.full_kind(NodeKind::Command { command: command.clone(), next: next.get(ctx) })
            }
        }
    }

    fn compact_condition(&mut self, condition: &Condition, if_true: SmallNodeId, if_false: SmallNodeId, ctx: NodeId) -> CompactKind {
        match condition {
            Condition::Eq { size: 4, op1, op2 } if small_ref(*op1).is_some() && small_ref(*op2).is_some() => {
                CompactKind::Eq4 { op1: small_ref(*op1).unwrap(), op2: small_ref(*op2).unwrap(), if_true, if_false }
            }
            Condition::Ne0 { size: 4, op } if small_ref(*op).is_some() => {
                CompactKind::Ne04 { op: small_ref(*op).unwrap(), if_true, if_false }
            }
            _ => {
                self.full_kind(NodeKind::Branch { condition: condition.clone(), if_true: if_true.get(ctx), if_false: if_false.get(ctx) })
            }
        }
    }

    fn full_kind(&mut self, kind: NodeKind<NodeId>) -> CompactKind {
        self.full.push(kind);
        CompactKind::Full((self.full.len() - 1) as u32)
    }
}

impl Engine for SpecializedInterpreterEngine {
    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) { self.register(id, kind) }
    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool { self.run(state, stack) }
}

#[test]
fn test_sizes() {
    assert_eq!(8, std::mem::size_of::<CompactKind>());
    assert_eq!(40, std::mem::size_of::<NodeKind<NodeId>>());
}