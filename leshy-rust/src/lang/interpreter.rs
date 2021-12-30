use std::borrow::Cow;
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
        line = run_line(loader, &func, line, stack);
    }
}

// returns next line to evaluate
fn run_line(loader: &impl FuncLoader, func: &Func, line: usize, stack: &mut Stack) -> usize {
    // println!("{:?}: {:?}@{}", stack.as_slice(0), &func.ops.get(line).unwrap().0, line);
    match &func.ops.get(line).unwrap().0 {
        Operation::Extend { length } => { stack.extend(eval_stack_size(stack, length)) }
        Operation::Shrink { length } => { stack.shrink(eval_stack_size(stack, length)) }
        Operation::CheckSize { length } => {
            stack.check_frame_size(eval_stack_size(stack, length))
        }
        Operation::Branch { modifier, length, op1, op2, target } => {
            let modifier_v = eval_symbol(modifier);
            let length_v = eval_stack_size(stack, length);
            let op1_v = eval_const_or_address(stack, op1, length_v);
            let op2_v = eval_const_or_address(stack, op2, length_v);

            let branch_result = match modifier_v {
                "eq" => { operations::cmp_eq(length_v, &op1_v, &op2_v) }
                "ne" => { !operations::cmp_eq(length_v, &op1_v, &op2_v) }
                "le" => { !operations::cmp_le(length_v, &op1_v, &op2_v) }
                "gt" => { !operations::cmp_gt(length_v, &op1_v, &op2_v) }
                _ => { panic!("can't interpret {}", modifier_v) }
            };

            if branch_result {
                return func.labels.get(eval_symbol(target)).unwrap().clone();
            }
        }
        Operation::Jump { target } => {
            return *func.labels.get(eval_symbol(target)).unwrap()
        }
        Operation::Call { offset, target } => {
            let offset_v = eval_stack_size(stack, offset);
            stack.move_forward(offset_v);
            run(loader, eval_symbol(target), stack);
            stack.move_back(offset_v);
        }
        Operation::Specialize { .. } => {
            // todo: track consts, noop for now
        }
        Operation::NotSpecialize { .. } => {
            // todo: track consts, noop for now
        }
        Operation::Set { length, src, dst } => {
            let len = eval_stack_size(stack, length);
            match len {
                4 => {
                    let v = operations::get_i32(&eval_const_or_address(stack, src, len)).unwrap();
                    operations::put_i32(eval_address(stack, dst), v)
                }
                8 => {
                    let v = operations::get_i64(&eval_const_or_address(stack, src, len)).unwrap();
                    operations::put_i64(eval_address(stack, dst), v)
                }
                _ => { todo!() }
            }
        }
        Operation::Add { length, op1, op2, dst } => {
            let len = eval_stack_size(stack, length);
            // todo: is it possible to extract this code into separate function operations::add? seems like borrowing makes it hard
            match len {
                4 => {
                    let v1 = operations::get_i32(&eval_const_or_address(stack, op1, len)).unwrap();
                    let v2 = operations::get_i32(&eval_const_or_address(stack, op2, len)).unwrap();
                    operations::put_i32(eval_address(stack, dst), v1 + v2)
                }
                8 => {
                    let v1 = operations::get_i64(&eval_const_or_address(stack, op1, len)).unwrap();
                    let v2 = operations::get_i64(&eval_const_or_address(stack, op2, len)).unwrap();
                    operations::put_i64(eval_address(stack, dst), v1 + v2)
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
    stack.frame_offset(operations::bytes_as_i32(&eval_const(stack, value, 4)).unwrap())
}

fn eval_const<'a>(stack: &'a Stack, value: &'a Const, expected_len: usize) -> Cow<'a, [u8]> {
    match value {
        Const::Literal { bytes } => { extend(bytes, expected_len) }
        Const::Stack { from_offset, length } => {
            let from_offset_v = stack.frame_offset(operations::bytes_as_i32(from_offset).unwrap());
            let length_v = operations::bytes_as_i32(length).unwrap() as usize;
            assert!(length_v >= expected_len);
            Cow::Borrowed(&stack.as_slice(from_offset_v)[0..length_v])
        }
        Const::Symbol { .. } => { todo!() }
    }
}

fn extend(bytes: &[u8], expected_len: usize) -> Cow<[u8]> {
    if bytes.len() >= expected_len { Cow::Borrowed(bytes) } else {
        let mut extended = Vec::with_capacity(expected_len);
        extended.extend_from_slice(bytes);
        extended.resize(expected_len, 0);
        Cow::Owned(extended)
    }
}

fn eval_const_or_address<'a>(stack: &'a Stack, value: &'a ConstOrAddress, const_expected_len: usize) -> Cow<'a, [u8]> {
    match value {
        ConstOrAddress::Left { value } => { eval_const(stack, value, const_expected_len) }
        ConstOrAddress::Right { value } => {
            // todo: can't unify this with eval_address because of different mut requirements, is it possible to unify?
            match value {
                Address::Stack { address } => {
                    Cow::Borrowed(stack.as_slice(eval_stack_size(stack, address)))
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
fn test_fib4() {
    let loader = files_loader(vec![Path::new("../examples/fib.lsh")]);
    let mut stack = Stack::create();
    stack.push(&operations::bytes_from_i32(8));
    run(&loader, "fib4", &mut stack);
    assert_eq!(Some(21), operations::bytes_as_i32(stack.as_slice(0)));
}

#[test]
fn test_fib8() {
    let loader = files_loader(vec![Path::new("../examples/fib.lsh")]);
    let mut stack = Stack::create();
    stack.push(&operations::bytes_from_i32(8));
    run(&loader, "fib8", &mut stack);
    assert_eq!(Some(21), operations::bytes_as_i64(stack.as_slice(0)));
}

#[test]
fn test_fibx4() {
    let loader = files_loader(vec![Path::new("../examples/fib.lsh")]);
    let mut stack = Stack::create();
    stack.push(&operations::bytes_from_i32(4));
    stack.push(&operations::bytes_from_i32(8));
    run(&loader, "fibx", &mut stack);
    assert_eq!(0, stack.offset);
    assert_eq!(8, stack.size);
    assert_eq!(Some(4), operations::get_i32(stack.as_slice(0)));
    assert_eq!(Some(21), operations::get_i32(stack.as_slice(4)));
}

#[test]
fn test_fibx8() {
    let loader = files_loader(vec![Path::new("../examples/fib.lsh")]);
    let mut stack = Stack::create();
    stack.push(&operations::bytes_from_i32(8));
    stack.push(&operations::bytes_from_i32(8));
    run(&loader, "fibx", &mut stack);
    assert_eq!(0, stack.offset);
    assert_eq!(12, stack.size);
    assert_eq!(Some(8), operations::get_i32(stack.as_slice(0)));
    assert_eq!(Some(21), operations::get_i64(stack.as_slice(4)));
}
