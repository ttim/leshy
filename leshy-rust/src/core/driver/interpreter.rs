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
        while !state.frames.is_empty() {
            let current = state.frames.pop().unwrap();

            match self.get(current.id) {
                None => {
                    state.frames.push(current);
                    return true;
                }
                Some(kind) => {
                    match kind {
                        NodeKind::Command { command, next } => {
                            eval_command(command, &mut stack[current.offset..]);
                            state.frames.push(Frame { id: *next, offset: current.offset } );
                        }
                        NodeKind::Branch { condition, if_true, if_false } => {
                            if eval_condition(condition, &mut stack[current.offset..]) {
                                state.frames.push(Frame { id: *if_true, offset: current.offset });
                            } else {
                                state.frames.push(Frame { id: *if_false, offset: current.offset });
                            }
                        }
                        NodeKind::Call { offset, call, next } => {
                            state.frames.push(Frame { id: *next, offset: current.offset });
                            state.frames.push(Frame { id: *call, offset: current.offset + (*offset as usize) })
                        }
                        NodeKind::Final => {
                            continue
                        }
                    }
                }
            }
        }

        false
    }

    fn get(&self, id: NodeId) -> Option<&NodeKind<NodeId>> {
        match self.computed.get(id.0 as usize) {
            None => { None }
            Some(inner) => { inner.as_ref() }
        }
    }
}
