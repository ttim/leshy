use std::cell::RefCell;
use std::fs::File;
use std::num::Wrapping;
use std::rc::Rc;
use crate::core::api::{Node, Ref};
use crate::core::aux::cached_node::Cache;
use crate::core::driver::driver::Driver;
use crate::core::driver::interpreter_engine::InterpreterEngine;
use crate::core::aux::specialized_interpreter_engine::SpecializedInterpreterEngine;
use crate::core::driver::code_generator_engine::CodeGeneratorEngine;
use crate::core::interpreter::{eval, get_u32, put_u32};
use crate::core::utils::{pretty_print, traverse_node};
use crate::example::native_impl::fib4;
use crate::webasm::ast::Module;
use crate::webasm::node::{Source, WebAsmNode};
use crate::webasm::parser::hydrate::hydrate_module;

const CODE_SIZE: usize = 8 * 1024;

fn fib_node(func_name: &str) -> WebAsmNode {
    let name = String::from("data/fib.wasm");
    let mut file = File::open(&name).unwrap();
    let module = Module::read(&mut file).unwrap();
    hydrate_module(&module, &mut file);
    let source = Rc::new(Source { uuid: 0, name: name.clone(), file: RefCell::new(file), module });
    WebAsmNode::exported_func(source.clone(), func_name)
}

fn fib_node_32() -> WebAsmNode { fib_node("fib32") }
fn fib_node_64() -> WebAsmNode { fib_node("fib64") }

#[test]
fn test_node_creation() { traverse_node(fib_node_32()); }

#[test]
fn test_node_pretty_print() { pretty_print(fib_node_32()); }

#[test]
fn test_cached_node_pretty_print() { pretty_print(Cache::new().cache(fib_node_32())); }

fn run_fib<F: FnOnce(&mut [u8])>(eval: F, n: u32) -> u32 {
    let mut stack = [0u8; 1000];
    put_u32(Ref::Stack(0), &mut stack, Wrapping(n));
    eval(&mut stack);
    get_u32(Ref::Stack(0), &stack).0
}

fn run_fib_node<N: Node>(node: N, n: u32) -> u32 { run_fib(|stack| eval(node, stack), n) }

// run in release mode with `cargo test webasm::fib_tests::test_node_eval --release`
#[test]
fn test_node_eval() {
    run_fib_node(fib_node_32(), 25);
}

#[test]
fn test_cached_node_eval() {
    run_fib_node(Cache::new().cache(fib_node_32()), 35);
}

#[test]
fn test_specialized_interpreter_eval() {
    let mut interpreter = Driver::new(SpecializedInterpreterEngine::new());
    run_fib(|stack| interpreter.eval(fib_node_32(), stack), 35);
    // interpreter.print_stats()
}

#[test]
fn test_caching_interpreter_eval() {
    run_fib(|stack| Driver::new(InterpreterEngine::new()).eval(fib_node_32(), stack), 35);
}

#[test]
fn test_code_generator_eval() {
    let res = run_fib(|stack| Driver::new(CodeGeneratorEngine::new(CODE_SIZE).unwrap()).eval(fib_node_32(), stack), 39);
    assert_eq!(63245986, res);
}

#[test]
fn test_code_generator_eval_64() {
    let res = run_fib(|stack| Driver::new(CodeGeneratorEngine::new(CODE_SIZE).unwrap()).eval(fib_node_64(), stack), 6);
    assert_eq!(13, res);
}

#[test]
fn test_native() {
    println!("fib({}) = {}", 39, fib4(39));
}