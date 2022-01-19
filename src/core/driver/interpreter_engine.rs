use crate::core::api::NodeKind;
use crate::core::driver::driver::{Frame, RunState, NodeId, Engine};
use crate::core::interpreter::{eval_command, eval_condition};

pub struct InterpreterEngine {
    computed: Vec<Option<NodeKind<NodeId>>>,
}

impl InterpreterEngine {
    pub fn new() -> InterpreterEngine { InterpreterEngine { computed: Vec::new() } }

    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        while self.computed.len() <= id.0 as usize {
            self.computed.push(None);
        }
        *self.computed.get_mut(id.0 as usize).unwrap() = Some(kind);
    }

    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool {
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

impl Engine for InterpreterEngine {
    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) { self.register(id, kind) }
    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool { self.run(state, stack) }
}

#[test]
fn test_sizes() {
    assert_eq!(40, std::mem::size_of::<Option<NodeKind<NodeId>>>());
}