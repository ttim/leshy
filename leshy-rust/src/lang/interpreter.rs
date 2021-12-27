use std::borrow::Borrow;
use std::path::Path;
use std::rc::Rc;
use crate::lang::ast::{Const, Func, Operation};
use crate::lang::common::{Bytes, Stack};
use crate::lang::loader::{files_loader, FuncLoader};

pub fn run(loader: &impl FuncLoader, name: &str, stack: &mut Stack) {
    let func: Rc<Func> = loader.load(name).unwrap();
    let mut line = 0 as usize;
    while line != func.ops.len() {
        line = run_line(loader, func.as_ref(), line, stack);
    }
}

// returns next line to evaluate
fn run_line(loader: &impl FuncLoader, func: &Func, line: usize, stack: &mut Stack) -> usize {
    match &func.ops.get(line).unwrap().0 {
        Operation::Extend { .. } => { todo!() }
        Operation::Shrink { .. } => { todo!() }
        Operation::CheckSize { length } => {
            stack.check_frame_size(eval_const(length).as_i32().unwrap() as usize)
        }
        Operation::Branch { .. } => { todo!() }
        Operation::Jump { .. } => { todo!() }
        Operation::Call { .. } => { todo!() }
        Operation::Specialize { .. } => { todo!() }
        Operation::NotSpecialize { .. } => { todo!() }
        Operation::Set { .. } => { todo!() }
        Operation::Add { .. } => { todo!() }
        Operation::Mult { .. } => { todo!() }
        Operation::Neg { .. } => { todo!() }
    }
    line + 1
}

fn eval_const(value: &Const) -> &Bytes {
    match value {
        Const::Literal { bytes } => { bytes }
        Const::Stack { .. } => { todo!() }
        Const::Symbol { .. } => { todo!() }
    }
}

#[test]
fn test_fib() {
    let loader = files_loader(vec![Path::new("../examples/fib.lsh")]);
    let mut stack = Stack::create();
    stack.push(&Bytes::from_i32(8));
    run(&loader, "fib4", &mut stack);
    assert_eq!(Some(21), stack.pull().as_i32());
}