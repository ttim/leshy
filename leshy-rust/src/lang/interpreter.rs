use std::borrow::Borrow;
use std::path::Path;
use std::rc::Rc;
use crate::lang::ast::{Address, Const, ConstOrAddress, Func, Operation};
use crate::lang::common::{Bytes, Stack};
use crate::lang::operations;
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
            stack.check_frame_size(operations::as_i32(eval_const(length)).unwrap() as usize)
        }
        Operation::Branch { modifier, length, op1, op2, target } => {
            let modifierV = eval_const(modifier);
            let lengthV = operations::as_i32(eval_const(length)).unwrap() as usize;
            let op1V = eval_const_or_address(op1);
            let op2V = eval_const_or_address(op2);

            let branch_result = if modifierV == "eq".as_bytes() {
                operations::equal(lengthV, op1V, op2V)
            } else if modifierV == "ne".as_bytes() {
                !operations::equal(lengthV, op1V, op2V)
            } else if modifierV == "le".as_bytes() {
                operations::less(lengthV, op1V, op2V, true)
            } else if modifierV == "gt".as_bytes() {
                !operations::less(lengthV, op1V, op2V, false)
            } else {
                panic!("can't interpret {}", String::from_utf8_lossy(modifierV))
            };

            if branch_result {
                return func.labels.get(eval_symbol(target)).unwrap().clone();
            }
        }
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

fn eval_const(value: &Const) -> &[u8] {
    match value {
        Const::Literal { bytes } => { bytes.as_bytes() }
        Const::Stack { .. } => { todo!() }
        Const::Symbol { name } => { todo!() }
    }
}

fn eval_const_or_address(value: &ConstOrAddress) -> &[u8] {
    match value {
        ConstOrAddress::Left { value } => { eval_const(value) }
        ConstOrAddress::Right { value } => { eval_address(value) }
    }
}

fn eval_address(value: &Address) -> &mut [u8] {
    todo!()
}

fn eval_symbol(value: &Const) -> &str {
    todo!()
}

#[test]
fn test_fib() {
    let loader = files_loader(vec![Path::new("../examples/fib.lsh")]);
    let mut stack = Stack::create();
    stack.push(&Bytes::from_i32(8));
    run(&loader, "fib4", &mut stack);
    assert_eq!(Some(21), stack.pull().as_i32());
}