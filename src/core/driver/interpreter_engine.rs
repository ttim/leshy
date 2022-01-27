use crate::core::api::NodeKind;
use crate::core::driver::driver::{Frame, RunState, NodeId, Engine};
use crate::core::interpreter::{eval_command, eval_condition};

pub struct InterpreterEngine {
    computed: Vec<Option<NodeKind<NodeId>>>,
    debug: bool,
}

impl InterpreterEngine {
    pub fn new(debug: bool) -> InterpreterEngine { InterpreterEngine { computed: Vec::new(), debug } }

    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        while self.computed.len() <= id.0 as usize {
            self.computed.push(None);
        }
        *self.computed.get_mut(id.0 as usize).unwrap() = Some(kind);
    }

    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool {
        let mut offset: usize = state.offset();
        while !state.frames.is_empty() {
            let current = state.frames.pop().unwrap();

            match self.get(current.id) {
                None => {
                    state.frames.push(current);
                    return true;
                }
                Some(kind) => {
                    if self.debug {
                        println!("run {}", current.id.0);
                        let count = (offset + 24) / 4;
                        (0..count).for_each(|num| unsafe {
                            let val = u32::from_le_bytes(stack[num * 4 .. num * 4 + 4].try_into().unwrap());
                            print!("{} ", val);
                        });
                        println!()
                    }
                    match kind {
                        NodeKind::Command { command, next } => {
                            eval_command(command, &mut stack[offset..]);
                            state.frames.push(Frame { id: *next, offset: current.offset } );
                        }
                        NodeKind::Branch { condition, if_true, if_false } => {
                            if eval_condition(condition, &mut stack[offset..]) {
                                state.frames.push(Frame { id: *if_true, offset: current.offset });
                            } else {
                                state.frames.push(Frame { id: *if_false, offset: current.offset });
                            }
                        }
                        NodeKind::Call { offset: call_offset, call, next } => {
                            state.frames.push(Frame { id: *next, offset: current.offset });
                            state.frames.push(Frame { id: *call, offset: *call_offset as usize });
                            offset += *call_offset as usize;
                        }
                        NodeKind::Final => {
                            offset -= current.offset;
                            continue;
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