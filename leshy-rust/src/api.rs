use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

pub trait Node: Hash + Eq + Sized + Debug {
    fn get(&self) -> NodeKind<Self>;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum NodeKind<N: Node> {
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

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Ref {
    Stack { offset: u32 },
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Command {
    Push { size: u32, src: Ref },
    PushConst { bytes: Vec<u8> },
    WriteConst { dst: Ref, bytes: Vec<u8> },
    Shrink { size: u32 },
}

pub fn stack_size_change(command: &Command) -> i32 {
    match command {
        Command::Push { size, .. } => { *size as i32 }
        Command::PushConst { bytes } => { bytes.len() as i32 }
        Command::Shrink { size } => { -(*size as i32) }
        Command::WriteConst { .. } => { 0 }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Condition {
    Eq { size: u32, op1: Ref, op2: Ref },
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct ExampleNodeImpl {
    id: u64,
}

impl Node for ExampleNodeImpl {
    fn get(&self) -> NodeKind<Self> {
        NodeKind::Final
    }
}

pub fn traverse_node<N: Node>(node: N) -> HashSet<N> {
    fn rec<N: Node>(node: N, visited: &mut HashSet<N>) {
        if visited.contains(&node) { return; }
        let kind = node.get();
        visited.insert(node);
        match kind {
            NodeKind::Command { next, .. } => {
                rec(next, visited);
            }
            NodeKind::Branch { if_true, if_false, .. } => {
                rec(if_true, visited);
                rec(if_false, visited);
            }
            NodeKind::Call { call, next, .. } => {
                rec(call, visited);
                rec(next, visited);
            }
            NodeKind::Final => {}
        }
    }

    let mut visited: HashSet<N> = HashSet::new();
    rec(node, &mut visited);
    visited
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
