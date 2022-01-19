use std::fmt::Debug;
use std::hash::Hash;

pub trait Node: Hash + Eq + Sized + Debug + Clone {
    fn get(&self) -> NodeKind<Self>;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum NodeKind<N> {
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

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum Ref {
    Stack(u32),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Command {
    Noop,

    // There've been explicit stack size change operations before here, but not anymore.
    // We already have all information needed in nodes - from their `size` and `Ref` arguments.
    // The only missing information is indication that rest of stack isn't needed anymore - we substitute it with poison.

    // Instead of having stack changing operations we use poison
    // Maybe we want "unspecified" instead of poison, or both.
    PoisonFrom { dst: Ref },
    // Poison { size: u32, dst: Ref },

    Set { dst: Ref, bytes: Vec<u8> },
    Copy { dst: Ref, size: u32, op: Ref },

    Add { size: u32, dst: Ref, op1: Ref, op2: Ref },
    Sub { size: u32, dst: Ref, op1: Ref, op2: Ref },
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Condition {
    Eq { size: u32, op1: Ref, op2: Ref },
    Ne0 { size: u32, op: Ref }, // not equal to zero, similar to c ifs
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

#[test]
fn test_sizes() {
    assert_eq!(4, std::mem::size_of::<Ref>())
}