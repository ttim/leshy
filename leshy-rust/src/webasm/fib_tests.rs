use std::cell::RefCell;
use std::fs::File;
use std::num::Wrapping;
use std::rc::Rc;
use crate::core::api::{Node, Ref};
use crate::core::cached_node::Cache;
use crate::core::interpreter::{eval, get_u32, put_u32};
use crate::core::utils::{pretty_print, traverse_node};
use crate::webasm::ast::Module;
use crate::webasm::node::{Source, WebAsmNode};
use crate::webasm::parser::hydrate::hydrate_module;

fn fib_node() -> WebAsmNode {
    let name = String::from("data/fib.wasm");
    let mut file = File::open(&name).unwrap();
    let module = Module::read(&mut file).unwrap();
    hydrate_module(&module, &mut file);
    let source = Rc::new(Source { uuid: 0, name: name.clone(), file: RefCell::new(file), module });
    WebAsmNode::exported_func(source.clone(), "fib")
}

#[test]
fn test_node_creation() { traverse_node(fib_node()); }

#[test]
fn test_node_pretty_print() { pretty_print(fib_node()); }

#[test]
fn test_cached_node_pretty_print() { pretty_print(Cache::new().cache(fib_node())); }

fn run_fib<N: Node>(node: N, n: u32) {
    let mut stack = [0u8; 1000];
    put_u32(&Ref::Stack(0), &mut stack, Wrapping(n));
    eval(node, &mut stack);
    let res = get_u32(&Ref::Stack(0), &stack).0;
    println!("fib({}) = {}", n ,res);
}

// run in release mode with `cargo test webasm::fib_tests::test_node_eval --release`
#[test]
fn test_node_eval() {
    run_fib(fib_node(), 25);
}

#[test]
fn test_cached_node_eval() {
    run_fib(Cache::new().cache(fib_node()), 25);
}
