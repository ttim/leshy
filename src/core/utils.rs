use std::collections::{HashMap, HashSet};
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

pub fn pretty_print<N: Node>(node: N) {
    fn print_line(offset: u32, line: &mut u32) {
        *line = *line + 1;
        print!("{}\t", line);
        (0..offset).for_each(|_| print!("\t"));
    }

    fn rec<N: Node>(offset: u32, line: &mut u32, node: N, visited: &mut HashMap<N, u32>) {
        print_line(offset, line);
        if visited.contains_key(&node) {
            println!("<ref {}>", visited.get(&node).unwrap());
            return;
        }
        visited.insert(node.clone(), *line);

        match node.get() {
            NodeKind::Command { command, next } => {
                println!("{:?}", command);
                rec(offset, line, next, visited);
            }
            NodeKind::Branch { condition, if_true, if_false } => {
                println!("{:?}", condition);
                rec(offset + 1, line, if_true, visited);
                print_line(offset, line);
                println!("else");
                rec(offset + 1, line, if_false, visited);
            }
            NodeKind::Call { offset: call_offset, call, next } => {
                println!("call {}", call_offset);
                rec(offset + 1, line, call, visited);
                rec(offset, line, next, visited);
            }
            NodeKind::Final => {
                println!("<final>")
            }
        }
    }

    let mut visited: HashMap<N, u32> = HashMap::new();
    let mut line = 0u32;
    rec(0, &mut line, node, &mut visited);
}
