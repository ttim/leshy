use std::collections::HashMap;
use crate::core::api::NodeKind;
use crate::core::driver::driver::{Frame, CallCtx, NodeId};
use crate::core::simple_interpreter::{eval_command, eval_condition};

pub struct Interpreter {
    nodes: Vec<Option<NodeKind<NodeId>>>,
}

impl Interpreter {
    pub fn new() -> Interpreter { Interpreter { nodes: Vec::new() } }

    pub fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        while self.nodes.len() <= id.0 as usize {
            self.nodes.push(None);
        }
        *self.nodes.get_mut(id.0 as usize).unwrap() = Some(kind);
    }

    fn get(&self, id: NodeId) -> Option<&NodeKind<NodeId>> {
        match self.nodes.get(id.0 as usize) {
            None => { None }
            Some(inner) => { inner.as_ref() }
        }
    }

    // returns true - current top is unknown
    //         false - current top is known but break, or finish of execution
    pub fn run(&self, ctx: &mut CallCtx, stack: &mut [u8]) -> bool {
        while !ctx.callstack.is_empty() {
            let frame = ctx.callstack.pop().unwrap();
            match self.run_internal(frame.id, &mut stack[frame.offset..]) {
                None => {
                    // do nothing
                }
                Some(mut suspended_in) => {
                    suspended_in.reverse();
                    suspended_in.iter_mut().for_each(|suspended_frame| suspended_frame.offset += frame.offset);
                    ctx.callstack.append(&mut suspended_in);
                    return true;
                }
            }
        }

        false
    }

    fn run_internal(&self, node: NodeId, stack: &mut [u8]) -> Option<Vec<Frame>> {
        let mut current = node;
        loop {
            match self.get(current) {
                None => {
                    return Some(vec![Frame { id: current, offset: 0 }]);
                }
                Some(kind) => {
                    match kind {
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
                                Some(mut sub_call) => {
                                    // suspended in sub call
                                    sub_call.iter_mut().for_each(|e| e.offset += offset);
                                    sub_call.push(Frame { id: *next, offset: 0 });
                                    return Some(sub_call);
                                }
                            }
                        }
                        NodeKind::Final => {
                            return None;
                        }
                    }
                }
            }
        }
    }
}
