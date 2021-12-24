use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::lang::{
    ast::{Address, Const, ConstOrAddress, Func, Operation, Source},
    common::Bytes,
};

pub fn parse(path: &Path) -> Vec<Func> {
    parse_text(path, &std::fs::read_to_string(path).unwrap())
}

pub fn parse_text(src: &Path, text: &str) -> Vec<Func> {
    let src_rc: Rc<PathBuf> = Rc::new(PathBuf::from(src));

    let mut current_func: Vec<(usize, &str)> = vec![];
    let mut funcs: Vec<Func> = vec![];

    text.split('\n').enumerate().for_each(|(line_num, line)| {
        if line.starts_with("def ") {
            if !current_func.is_empty() {
                parse_func(src_rc.clone(), &current_func)
                    .into_iter()
                    .for_each(|f| funcs.push(f));
                current_func.clear();
            } // else do nothing
        }
        current_func.push((line_num, line));
    });
    if !current_func.is_empty() {
        parse_func(src_rc.clone(), &current_func)
            .into_iter()
            .for_each(|f| funcs.push(f));
    }

    funcs
}

fn parse_func(src: Rc<PathBuf>, lines: &Vec<(usize, &str)>) -> Option<Func> {
    if !lines[0].1.trim().starts_with("def ") {
        // todo: check all lines are comments
        return None;
    }

    let name = String::from(&lines[0].1.trim()[4..]); // remove "def "

    let mut ops: Vec<(Operation, Source)> = vec![];
    let mut labels: HashMap<String, usize> = HashMap::new();

    lines[1..].iter().for_each(|(line_num, line)| {
        let no_comments = remove_comments(line);
        if no_comments.ends_with(":") {
            // label
            labels.insert(
                String::from(&no_comments[0..no_comments.len() - 1]),
                ops.len(),
            );
        } else {
            // operation
            parse_operation(no_comments)
                .into_iter()
                .for_each(|operation| {
                    ops.push((
                        operation,
                        Source {
                            file: src.clone(),
                            line: line_num.clone(),
                        },
                    ));
                });
        }
    });

    Some(Func {
        name: name,
        ops: ops,
        labels: labels,
    })
}

fn remove_comments(s: &str) -> &str {
    let end_index = s.find(";").unwrap_or(s.len());
    s[0..end_index].trim()
}

fn parse_operation(s: &str) -> Option<Operation> {
    let v: Vec<&str> = s.split(" ").collect();
    match &v[..] {
        // noop
        [""] => None,

        // stack
        ["extend", length_str] => Some(Operation::Extend {
            length: parse_const(length_str),
        }),
        ["shrink", length_str] => Some(Operation::Shrink {
            length: parse_const(length_str),
        }),
        ["check_size", length_str] => Some(Operation::CheckSize {
            length: parse_const(length_str),
        }),

        // control flow
        ["branch", modifier_str, length_str, op1_str, op2_str, target_str] => {
            Some(Operation::Branch {
                modifier: parse_const(modifier_str),
                length: parse_const(length_str),
                op1: parse_const_or_address(op1_str),
                op2: parse_const_or_address(op2_str),
                target: parse_const(target_str),
            })
        }
        ["jump", target_str] => Some(Operation::Jump {
            target: parse_const(target_str),
        }),

        // call
        ["call", offset_str, target_str] => Some(Operation::Call {
            offset: parse_const(offset_str),
            target: parse_const(target_str),
        }),

        // const
        ["not_specialize", length_str, dst_str] => Some(Operation::NotSpecialize {
            length: parse_const(length_str),
            dst: parse_address(dst_str),
        }),
        ["specialize", length_str, dst_str] => Some(Operation::Specialize {
            length: parse_const(length_str),
            dst: parse_address(dst_str),
        }),

        // memory
        ["set", length_str, src_str, dst_str] => Some(Operation::Set {
            length: parse_const(length_str),
            src: parse_const_or_address(src_str),
            dst: parse_address(dst_str),
        }),

        // arithmetic
        ["add", length_str, op1_str, op2_str, dst_str] => Some(Operation::Add {
            length: parse_const(length_str),
            op1: parse_const_or_address(op1_str),
            op2: parse_const_or_address(op2_str),
            dst: parse_address(dst_str),
        }),
        ["mult", length_str, op1_str, op2_str, dst_str] => Some(Operation::Mult {
            length: parse_const(length_str),
            op1: parse_const_or_address(op1_str),
            op2: parse_const_or_address(op2_str),
            dst: parse_address(dst_str),
        }),
        ["neg", length_str, op_str, dst_str] => Some(Operation::Neg {
            length: parse_const(length_str),
            op: parse_const_or_address(op_str),
            dst: parse_address(dst_str),
        }),

        _ => panic!("can't parse operation: {}", s),
    }
}

fn parse_const_or_address(s: &str) -> ConstOrAddress {
    if s.starts_with("*") || s.starts_with("#") {
        ConstOrAddress::Right {
            value: parse_address(s),
        }
    } else {
        ConstOrAddress::Left {
            value: parse_const(s),
        }
    }
}

fn parse_address(s: &str) -> Address {
    if s.starts_with("*") {
        Address::Native {
            stack_offset: parse_const(&s[1..]),
        }
    } else if s.starts_with("#") {
        Address::Stack {
            address: parse_const(&s[1..]),
        }
    } else {
        panic!("can't parse address {}", s);
    }
}

fn parse_const(s: &str) -> Const {
    if s.starts_with("$") {
        Const::Stack {
            from_offset: parse_literal(&s[1..]).unwrap(),
            length: Bytes::from_i32(4),
        }
    } else if s.starts_with(":") {
        Const::Symbol {
            name: String::from(&s[1..]),
        }
    } else if let Some(bytes) = parse_literal(s) {
        Const::Literal { bytes: bytes }
    } else {
        todo!("can't parse const {}", s)
    }
}

fn parse_literal(s: &str) -> Option<Bytes> {
    let as_i32: Result<i32, _> = s.parse();
    as_i32.map(Bytes::from_i32).ok()
}

#[test]
fn test_parse() {
    let text = "def test
  init:
    check_size 4 ; comment
  ret:
    extend 4
";

    parse_text(Path::new("/example/path.lsh"), text);
    parse(Path::new("../examples/factorial.lsh"));
    parse(Path::new("../examples/fib.lsh"));
}
