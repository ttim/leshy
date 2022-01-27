use std::ops::Deref;
use crate::core::api::{Command, Condition, Node, NodeKind, Ref};
use crate::core::aux::specialized_interpreter_engine::SpecializedInterpreterEngine;
use crate::core::driver::code_generator_engine::CodeGeneratorEngine;
use crate::core::driver::driver::{Driver, Engine, NodeId, RunState};
use crate::core::driver::interpreter_engine::InterpreterEngine;
use crate::core::interpreter::eval;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct TestNode(Box<NodeKind<TestNode>>);

fn node(kind: NodeKind<TestNode>) -> TestNode { TestNode(Box::new(kind)) }

impl Node for TestNode {
    fn get(&self) -> NodeKind<Self> { self.0.deref().clone() }
}

// todo: this seems like unnessesary boiler place, how to avoid it?
struct EngineBox(Box<dyn Engine>);

impl Engine for EngineBox {
    fn register(&mut self, id: NodeId, kind: NodeKind<NodeId>) { self.0.register(id, kind) }
    fn run(&self, state: &mut RunState, stack: &mut [u8]) -> bool { self.0.run(state, stack) }
}

fn engines() -> Vec<(&'static str, EngineBox)> {
    vec![
        ("interpreter", EngineBox(Box::new(InterpreterEngine::new(false)))),
        ("specialized", EngineBox(Box::new(SpecializedInterpreterEngine::new()))),
        ("code generator", EngineBox(Box::new(CodeGeneratorEngine::new(1024, false).unwrap()))),
    ]
}

const TEST_STACK_SIZE: usize = 24;

fn test_node(input: Vec<u8>, node: TestNode) {
    let mut expected = [0u8; TEST_STACK_SIZE];
    expected[0..input.len()].copy_from_slice(input.as_slice());
    // todo: eval should track poisons and return poison information
    // todo: otherwise it's impossible to compare between results since impl can do whatever in poisoned bytes (i.e. do not clean them)
    eval(node.clone(), &mut expected);

    for (name, engine) in engines() {
        let mut actual = [0u8; TEST_STACK_SIZE];
        actual[0..input.len()].copy_from_slice(input.as_slice());
        Driver::<TestNode, EngineBox>::new(engine).eval(node.clone(), &mut actual);
        assert_eq!(expected, actual, "\"{}\" output differs from expected for {:?} on {:?}", name, node, input);
    }
}

fn test_command(input: Vec<u8>, command: Command) {
    test_node(input,node(NodeKind::Command { command, next: node(NodeKind::Final) }))
}

fn test_condition(input: Vec<u8>, condition: Condition) {
    test_node(input, node(NodeKind::Branch {
        condition,
        if_true: write_node(vec![2, 2, 2, 2]),
        if_false: write_node(vec![3, 3, 3, 3]),
    }))
}

fn write_node(bytes: Vec<u8>) -> TestNode {
    let command = Command::Set { dst: Ref::Stack(0), bytes };
    node(NodeKind::Command { command, next: node(NodeKind::Final) })
}

#[test]
fn test_final() {
    test_node(vec![1, 2, 3, 4], node(NodeKind::Final));
}

#[test]
fn test_noop() {
    test_command(vec![7, 7, 7, 7],Command::Noop)
}

#[test]
fn test_set() {
    test_command(vec![7, 7, 7, 7],Command::Set { dst: Ref::Stack(0), bytes: vec![1, 0, 0, 0] });
    test_command(vec![7, 7, 7, 7],Command::Set { dst: Ref::Stack(0), bytes: vec![1, 2, 3, 4] });
    test_command(vec![7, 7, 7, 7, 7, 7, 7, 7], Command::Set { dst: Ref::Stack(0), bytes: vec![1, 2, 3, 4, 5, 6, 7, 8] });
}

#[test]
fn test_copy() {
    test_command(vec![1, 2, 3, 4],Command::Copy { size: 4, dst: Ref::Stack(4), op: Ref::Stack(0) });
    test_command(vec![1, 2, 3, 4, 5, 6, 7, 8],Command::Copy { size: 8, dst: Ref::Stack(8), op: Ref::Stack(0) });
}

#[test]
fn test_poison_from() {
    test_command(vec![1, 2, 3, 4],Command::PoisonFrom { dst: Ref::Stack(0) });
}

#[test]
fn test_add() {
    test_command(vec![1, 2, 3, 4, 5, 6, 7, 8],Command::Add { size: 4, dst: Ref::Stack(8), op1: Ref::Stack(0), op2: Ref::Stack(4) });
    test_command(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],Command::Add { size: 8, dst: Ref::Stack(16), op1: Ref::Stack(0), op2: Ref::Stack(8) });
}

#[test]
fn test_sub() {
    test_command(vec![1, 2, 3, 4, 5, 6, 7, 8],Command::Sub { size: 4, dst: Ref::Stack(8), op1: Ref::Stack(0), op2: Ref::Stack(4) });
}

#[test]
fn test_eq() {
    let condition = Condition::Ne { size: 4, op1: Ref::Stack(0), op2: Ref::Stack(4) };
    test_condition(vec![1, 2, 3, 4, 1, 2, 3, 4], condition.clone());
    test_condition(vec![1, 2, 3, 4, 5, 6, 7, 8], condition.clone());
}

#[test]
fn test_ne0() {
    let condition = Condition::Ne0 { size: 4, op: Ref::Stack(0) };
    test_condition(vec![1, 2, 3, 4], condition.clone());
    test_condition(vec![0, 0, 0, 0], condition.clone());
}

#[test]
fn test_call() {
    test_node(vec![], node(NodeKind::Call {
        offset: 4,
        call: write_node(vec![1, 2, 3, 4]),
        next: write_node(vec![5, 6, 7, 8])
    }))
}