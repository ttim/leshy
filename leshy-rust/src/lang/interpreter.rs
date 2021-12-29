use std::path::Path;
use std::rc::Rc;
use crate::lang::ast::{Address, Const, ConstOrAddress, Func, Operation};
use crate::lang::stack::Stack;
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
        Operation::Extend { length } => { stack.extend(eval_stack_size(stack, length)) }
        Operation::Shrink { .. } => { todo!() }
        Operation::CheckSize { length } => {
            stack.check_frame_size(eval_stack_size(stack, length))
        }
        Operation::Branch { modifier, length, op1, op2, target } => {
            let modifier_v = eval_symbol(modifier);
            let length_v = eval_stack_size(stack, length);
            let op1_v = eval_const_or_address(stack, op1);
            let op2_v = eval_const_or_address(stack, op2);

            let branch_result = match modifier_v {
                "eq" => { operations::cmp_eq(length_v, op1_v, op2_v) }
                "ne" => { !operations::cmp_eq(length_v, op1_v, op2_v) }
                "le" => { !operations::cmp_le(length_v, op1_v, op2_v) }
                "gt" => { !operations::cmp_gt(length_v, op1_v, op2_v) }
                _ => { panic!("can't interpret {}", modifier_v) }
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
        Operation::Add { length, op1, op2, dst } => {
            let len = eval_stack_size(stack, length);
            // todo: is it possible to extract this code into separate function operations::add? seems like borrowing makes it hard
            match len {
                4 => {
                    let v1 = operations::get_i32(eval_const_or_address(stack, op1)).unwrap();
                    let v2 = operations::get_i32(eval_const_or_address(stack, op2)).unwrap();
                    operations::put_i32(eval_address(stack, dst), v1 + v2)
                }
                _ => { todo!() }
            }
        }
        Operation::Mult { .. } => { todo!() }
        Operation::Neg { .. } => { todo!() }
    }
    line + 1
}

fn eval_stack_size(stack: &Stack, value: &Const) -> usize {
    stack.frame_offset(operations::bytes_as_i32(eval_const(value)).unwrap())
}

fn eval_const(value: &Const) -> &[u8] {
    match value {
        Const::Literal { bytes } => { bytes.as_slice() }
        Const::Stack { .. } => { todo!() }
        Const::Symbol { name } => { todo!() }
    }
}

fn eval_const_or_address<'a>(stack: &'a Stack, value: &'a ConstOrAddress) -> &'a [u8] {
    match value {
        ConstOrAddress::Left { value } => { eval_const(value) }
        ConstOrAddress::Right { value } => {
            // todo: can't unify this with eval_address because of different mut requirements, is it possible to unify?
            match value {
                Address::Stack { address } => {
                    let offset = operations::bytes_as_i32(eval_const(address)).unwrap();
                    stack.as_slice(offset as usize)
                }
                Address::Native { .. } => { todo!() }
            }
        }
    }
}

fn eval_address<'a>(stack: &'a mut Stack, value: &'a Address) -> &'a mut [u8] {
    match value {
        Address::Stack { address } => {
            stack.as_slice_mut(eval_stack_size(stack, address))
        }
        Address::Native { .. } => { todo!() }
    }
}

fn eval_symbol(value: &Const) -> &str {
    match value {
        Const::Literal { .. } => { todo!() }
        Const::Stack { .. } => { todo!() }
        Const::Symbol { name } => { name }
    }
}

#[test]
fn test_fib() {
    let loader = files_loader(vec![Path::new("../examples/fib.lsh")]);
    let mut stack = Stack::create();
    stack.push(&operations::bytes_from_i32(8));
    run(&loader, "fib4", &mut stack);
    assert_eq!(Some(21), operations::bytes_as_i32(stack.as_slice(0)));
}