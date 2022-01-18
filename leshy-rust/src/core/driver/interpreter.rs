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
            let mut current = ctx.callstack.pop().unwrap();

            match self.nodes.get(&current.id) {
                None => {
                    ctx.callstack.push(current);
                    return true;
                }
                Some(kind) => {
                    match kind {
                        NodeKind::Command { command, next } => {
                            eval_command(command, &mut stack[current.offset..]);
                            ctx.callstack.push(Frame { id: *next, offset: current.offset } );
                        }
                        NodeKind::Branch { condition, if_true, if_false } => {
                            if eval_condition(condition, &mut stack[current.offset..]) {
                                ctx.callstack.push(Frame { id: *if_true, offset: current.offset });
                            } else {
                                ctx.callstack.push(Frame { id: *if_false, offset: current.offset });
                            }
                        }
                        NodeKind::Call { offset, call, next } => {
                            ctx.callstack.push(Frame { id: *next, offset: current.offset });
                            ctx.callstack.push(Frame { id: *call, offset: current.offset + (*offset as usize) })
                        }
                        NodeKind::Final => {
                            continue
                        }
                    }
                }
            }
        }

        !ctx.callstack.is_empty()
    }
}
