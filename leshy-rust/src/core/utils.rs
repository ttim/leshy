use std::collections::HashSet;
use crate::core::api::{Node, NodeKind};

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