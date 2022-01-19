use crate::core::api::NodeKind;
use crate::core::driver::driver::{Engine, NodeId, RunState};

struct CodeGeneratorEngine {
}

impl CodeGeneratorEngine {
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