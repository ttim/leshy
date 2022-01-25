use std::num::Wrapping;
use crate::core::api::{Command, Condition, Node, NodeKind, Ref};

pub fn eval<N: Node>(node: N, stack: &mut [u8]) {
    let mut current = node;
    loop {
        match current.get() {
            NodeKind::Command { command, next } => {
                eval_command(&command, stack);
                current = next;
            }
            NodeKind::Branch { condition, if_true, if_false } => {
                if eval_condition(&condition, stack) {
                    current = if_true;
                } else {
                    current = if_false;
                }
            }
            NodeKind::Call { offset, call, next } => {
                eval(call, &mut stack[(offset as usize)..]);
                current = next;
            }
            NodeKind::Final => { break; }
        }
    }
}

pub fn get_final_kind<N: Node>(node: &N) -> NodeKind<N> {
    let kind = node.get();
    match kind {
        // noop ops
        NodeKind::Command { command: Command::Noop, next } => { get_final_kind(&next) }
        NodeKind::Command { command: Command::PoisonFrom { .. }, next } => { get_final_kind(&next) }

        NodeKind::Command { .. } => { kind }
        NodeKind::Branch { .. } => { kind }
        NodeKind::Call { .. } => { kind }
        NodeKind::Final => { kind }
    }
}

pub fn eval_command(command: &Command, stack: &mut [u8]) {
    match &command {
        Command::Noop => {}
        Command::PoisonFrom { .. } => {}
        Command::Set { dst, bytes } => {
            match dst {
                Ref::Stack(offset) => {
                    stack[*offset as usize..*offset as usize + bytes.len()].copy_from_slice(bytes.as_slice());
                }
            }
        }
        Command::Copy { size: 4, dst, op } => {
            put_u32(*dst, stack, get_u32(*op, stack));
        }
        Command::Copy { size: 8, dst, op } => {
            put_u64(*dst, stack, get_u64(*op, stack));
        }
        Command::Add { size: 4, dst, op1, op2 } => {
            put_u32(*dst, stack, get_u32(*op1, stack) + get_u32(*op2, stack))
        }
        Command::Add { size: 8, dst, op1, op2 } => {
            put_u64(*dst, stack, get_u64(*op1, stack) + get_u64(*op2, stack))
        }
        Command::Sub { size: 4, dst, op1, op2 } => {
            put_u32(*dst, stack, get_u32(*op1, stack) - get_u32(*op2, stack))
        }
        Command::Sub { size: 8, dst, op1, op2 } => {
            put_u64(*dst, stack, get_u64(*op1, stack) - get_u64(*op2, stack))
        }
        _ => {
            todo!("unsupported command: {:?}", command)
        }
    }
    // println!("{:?} <- eval {:?}", stack, command);
}

pub fn eval_condition(condition: &Condition, stack: &mut [u8]) -> bool {
    let result = match condition {
        Condition::Ne { size: 4, op1, op2 } => {
            get_u32(*op1, stack) != get_u32(*op2, stack)
        }
        Condition::Ne0 { size: 4, op: src } => {
            get_u32(*src, stack).0 != 0
        }
        _ => {
            todo!("unsupported condition: {:?}", condition)
        }
    };
    // println!("eval {} <- {:?}", result, condition);
    result
}

pub fn get_u32(src: Ref, stack: &[u8]) -> Wrapping<u32> {
    match src {
        Ref::Stack(offset) => {
            Wrapping(u32::from_le_bytes(stack[offset as usize..(offset + 4) as usize].try_into().unwrap()))
        }
    }
}

pub fn get_u64(src: Ref, stack: &[u8]) -> Wrapping<u64> {
    match src {
        Ref::Stack(offset) => {
            Wrapping(u64::from_le_bytes(stack[offset as usize..(offset + 8) as usize].try_into().unwrap()))
        }
    }
}

pub fn put_u32(dst: Ref, stack: &mut [u8], value: Wrapping<u32>) {
    match dst {
        Ref::Stack(offset) => {
            stack[offset as usize..(offset + 4) as usize].copy_from_slice(value.0.to_le_bytes().as_slice());
        }
    }
}

pub fn put_u64(dst: Ref, stack: &mut [u8], value: Wrapping<u64>) {
    match dst {
        Ref::Stack(offset) => {
            stack[offset as usize..(offset + 8) as usize].copy_from_slice(value.0.to_le_bytes().as_slice());
        }
    }
}