use std::collections::HashMap;
use crate::core::api::NodeKind;
use crate::core::driver::driver::{Frame, RunState, NodeId};
use crate::core::simple_interpreter::{eval_command, eval_condition};

pub struct Interpreter {
    computed: Vec<Option<NodeKind<NodeId>>>,
}

impl Interpreter {
    pub fn new() -> Interpreter { Interpreter { computed: Vec::new() } }

    pub fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        while self.computed.len() <= id.0 as usize {
            self.computed.push(None);
        }
        *self.computed.get_mut(id.0 as usize).unwrap() = Some(kind);
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
                                Some(mut suspended_trace) => {
                                    // suspended in sub call
                                    suspended_trace.iter_mut().for_each(|e| e.offset += offset);
                                    suspended_trace.push(Frame { id: *next, offset: 0 });
                                    return Some(suspended_trace);
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

    fn get(&self, id: NodeId) -> Option<&NodeKind<NodeId>> {
        match self.computed.get(id.0 as usize) {
            None => { None }
            Some(inner) => { inner.as_ref() }
        }
    }
}
