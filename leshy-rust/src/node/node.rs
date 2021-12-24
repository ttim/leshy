use std::hash::{Hash, Hasher};
use std::any::Any;

struct Node {
    content: Box<dyn NodeImpl>,
}

trait NodeImpl {
    fn get(&self) -> NodeKind;

    fn as_any(&self) -> &dyn Any;
    fn eq(&self, other: &dyn NodeImpl) -> bool;
    fn hash(&self, state: &mut dyn Hasher);
}

enum NodeKind {
    Command { command: Command, next: Node },
    Branch { condition: Condition, if_true: Node, if_false: Node },
    Call { offset: u32, call: Node, next: Box<dyn Fn(Node) -> Node> },
    Final,
}

enum Command {
}

enum Condition {
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        if self.type_id() != other.type_id() {
            false
        } else {
            NodeImpl::eq(self.content.as_ref(), other.content.as_ref())
        }
    }
}

impl Hash for Node {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.type_id().hash(state);
        self.content.hash(state)
    }
}

struct ExampleNodeImpl {
    id: u64,
}

impl NodeImpl for ExampleNodeImpl {
    fn get(&self) -> NodeKind {
        NodeKind::Final
    }

    fn eq(&self, other: &dyn NodeImpl) -> bool {
        match other.as_any().downcast_ref::<ExampleNodeImpl>() {
            Some(other) => other.id == self.id,
            None => false
        }
    }

    fn hash(&self, state: &mut dyn Hasher) {
        state.write_u64(self.id)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
