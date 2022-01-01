use std::hash::Hash;

trait Node: Hash + PartialEq + Sized {
    fn get(&self) -> NodeKind<Self>;
}

enum NodeKind<N: Node> {
    Command { command: Command, next: N },
    Branch { condition: Condition, if_true: N, if_false: N },
    Call { offset: u32, call: N, next: N },
    Final,
    // More complicated version of Call where next node depends on returned value ctx, final => next
    // CallDynamic { offset: u32, call: N, next: fn(N, N) -> N },
    // Specialize { offset: u32, length: u32, next: fn(N, [u8]) -> N }, // ctx, bytes => next
    // To support invoke_dynamic like functionality
    // Swap { src: N, dst: fn(N, N) -> N, next: N }, // replaces src node with dst node (as function of context node and previously set dst node) in execution
}

enum Command {
}

enum Condition {
}

#[derive(Hash, PartialEq)]
struct ExampleNodeImpl {
    id: u64,
}

impl Node for ExampleNodeImpl {
    fn get(&self) -> NodeKind<Self> {
        NodeKind::Final
    }
}

// There is more dynamic available using Box<dyn> approach.
// It might enable mixing different nodes in one runtime for example.
// But I think it's not worth changing so far.
//
// struct Node {
//     content: Box<dyn NodeImpl>,
// }
//
// trait NodeImpl {
//     fn get(&self) -> NodeKind;
//
//     fn as_any(&self) -> &dyn Any;
//     fn eq(&self, other: &dyn NodeImpl) -> bool;
//     fn hash(&self, state: &mut dyn Hasher);
// }
//
// enum NodeKind {
//     Command { command: Command, next: Node },
//     Branch { condition: Condition, if_true: Node, if_false: Node },
//     Call { offset: u32, call: Node, next: Box<dyn Fn(Node) -> Node> },
//     Final,
// }
//
// impl PartialEq for Node {
//     fn eq(&self, other: &Self) -> bool {
//         if self.type_id() != other.type_id() {
//             false
//         } else {
//             NodeImpl::eq(self.content.as_ref(), other.content.as_ref())
//         }
//     }
// }
//
// impl Hash for Node {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.type_id().hash(state);
//         self.content.hash(state)
//     }
// }
//
// struct ExampleNodeImpl {
//     id: u64,
// }
//
// impl NodeImpl for ExampleNodeImpl {
//     fn get(&self) -> NodeKind {
//         NodeKind::Final
//     }
//
//     fn eq(&self, other: &dyn NodeImpl) -> bool {
//         match other.as_any().downcast_ref::<ExampleNodeImpl>() {
//             Some(other) => other.id == self.id,
//             None => false
//         }
//     }
//
//     fn hash(&self, state: &mut dyn Hasher) {
//         state.write_u64(self.id)
//     }
//
//     fn as_any(&self) -> &dyn Any {
//         self
//     }
// }
