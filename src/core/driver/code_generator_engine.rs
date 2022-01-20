use crate::core::api::NodeKind;
use crate::core::driver::driver::{Engine, NodeId, RunState};

// engine <-> generated code interop/call conventions:
//   X0 pointer to data stack start
//   X1 pointer to data stack end // never changes during execution
//   X2 pointer to result struct // never changes during execution
// X1 & X2 can/should be moved to thread local variables since they never change during execution trace
//
// execution might abort/finish due to following reasons:
//   next node isn't registered: W0 = 0, W1 = next node id
//   next node is final: W0 = 1, W1 = final node id
//   out of space in data stack: W0 = 2, W1 = context node id
//   next node is function call: W0 = 3 + offset, W1 = function node id, upper X1 is call node id, upper X2 is next node id
//     should be changed with proper function call support in native

// to guarantee that stack doesn't spill we can have per node id guaranteed stack depth (meaning at least this number of bytes is definitely available from this point)
// and only if there is no guarantee on particular point we can check stack size and increase size if needed!
// this way we don't need to keep data stack end in a register and compare to it all the time

pub struct CodeGeneratorEngine {
}

impl CodeGeneratorEngine {
    pub fn new() -> CodeGeneratorEngine {
        CodeGeneratorEngine {}
    }

    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) {
        todo!()
    }

    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool {
        todo!()
    }
}

impl Engine for CodeGeneratorEngine {
    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) { self.register(id, kind) }

    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool { self.run(state, stack) }
}