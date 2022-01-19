use std::collections::HashMap;
use crate::core::api::NodeKind;
use crate::core::driver::driver::{Frame, CallCtx, NodeId};
use crate::core::simple_interpreter::{eval_command, eval_condition};

pub struct Interpreter {
    nodes: HashMap<NodeId, NodeKind<NodeId>>,
}

impl Interpreter {
    pub fn new() -> Interpreter { Interpreter { nodes: HashMap::new() } }

    pub fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        self.nodes.insert(id, kind);
    }

    // returns true - current top is unknown
    //         false - current top is known but break, or finish of execution
    pub fn run(&self, ctx: &mut CallCtx, stack: &mut [u8]) -> bool {
        while !ctx.callstack.is_empty() {
            let frame = ctx.callstack.pop().unwrap();
            let current_stack = &mut stack[frame.offset..];
            let mut current= frame.id;

            loop {
                match self.nodes.get(&current) {
                    None => {
                        ctx.callstack.push(Frame { id: current, offset: frame.offset });
                        return true;
                    }
                    Some(kind) => {
                        match kind {
                            NodeKind::Command { command, next } => {
                                eval_command(command, current_stack);
                                current = *next;
                            }
                            NodeKind::Branch { condition, if_true, if_false } => {
                                if eval_condition(condition, current_stack) {
                                    current = *if_true;
                                } else {
                                    current = *if_false;
                                }
                            }
                            NodeKind::Call { offset, call, next } => {
                                ctx.callstack.push(Frame { id: *next, offset: frame.offset });
                                ctx.callstack.push(Frame { id: *call, offset: frame.offset + (*offset as usize) });
                                break;
                            }
                            NodeKind::Final => {
                                break;
                            }
                        }
                    }
                }
            }
        }

        false
    }
}
