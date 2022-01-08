use crate::core::api::{Command, Condition, Node, NodeKind};

pub fn eval<N: Node>(node: N, stack: &mut [u8]) {
    let mut current = node;
    loop {
        match current.get() {
            NodeKind::Command { command, next } => {
                eval_command(command, stack);
                current = next;
            }
            NodeKind::Branch { condition, if_true, if_false } => {
                if eval_condition(condition, stack) {
                    current = if_true;
                } else {
                    current = if_false;
                }
            }
            NodeKind::Call { offset, call, next } => {
                eval(call, &mut stack[(offset as usize)..]);
                eval(next, stack);
            }
            NodeKind::Final => { break; }
        }
    }
}

fn eval_command(command: Command, stack: &mut [u8]) {
    match command {
        Command::Noop => {}
        Command::PoisonFrom { .. } => {}
        Command::Set { .. } => {
            todo!()
        }
        Command::Copy { .. } => {
            todo!()
        }
        Command::Add { .. } => {
            todo!()
        }
        Command::Sub { .. } => {
            todo!()
        }
    }
}

fn eval_condition(condition: Condition, stack: &mut [u8]) -> bool {
    match condition {
        Condition::Eq { .. } => {
            todo!()
        }
        Condition::Ne0 { .. } => {
            todo!()
        }
    }
}
