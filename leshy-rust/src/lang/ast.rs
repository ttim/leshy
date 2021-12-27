use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::path::PathBuf;
use std::rc::Rc;

use crate::lang::operations;
use crate::lang::operations::bytes_to_string;

#[derive(Clone)]
pub enum Const {
    Literal { bytes: Vec<u8> },
    Stack { from_offset: Vec<u8>, length: Vec<u8> },
    Symbol { name: String }, // get resolved to 4 bytes during execution
}

impl Debug for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_string())
    }
}

#[derive(Debug)]
pub enum Address {
    // Address in stack corresponding to `address`
    // #const
    Stack { address: Const },

    // Address in stack corresponding to 4 bytes in stack starting from `offset` after `base`, and must be less than `limit`
    // #[const, const, const]
    // while `base` and `limit` correspond to actual addresses on stack,
    // `offset` is correspond to stack position, where 4 bytes describe offset
    // StackOffset {
    //    address: Const,
    //    limit: Const,
    //    offset: Const,
    //},

    // Address in native memory corresponding to 8 bytes in stack starting from `stackOffset`
    // *const
    Native { stack_offset: Const },
}

#[derive(Debug)]
pub enum ConstOrAddress {
    Left { value: Const },
    Right { value: Address },
}

#[derive(Debug)]
pub enum Operation {
    // Stack operations
    Extend {
        length: Const,
    },
    Shrink {
        length: Const,
    },
    CheckSize {
        length: Const,
    },

    // Control flow operation
    Branch {
        modifier: Const,
        length: Const,
        op1: ConstOrAddress,
        op2: ConstOrAddress,
        target: Const,
    },
    Jump {
        target: Const,
    },

    // Call operations
    Call {
        offset: Const,
        target: Const,
    },

    // Constant operations
    Specialize {
        length: Const,
        dst: Address,
    },
    NotSpecialize {
        length: Const,
        dst: Address,
    },

    // Memory operations
    // Should be more different `Set` operations, to cover use cases with offsets and array offsets
    Set {
        length: Const,
        src: ConstOrAddress,
        dst: Address,
    },

    // `length` is treated as 8 bytes
    // todo: src can be only const or native address
    // todo: dst can be only Address::Native
    // SetNative {
    //    length: Address,
    //    src: ConstOrAddress,
    //    dst: Address,
    //},

    // Integer arithmetic operations
    Add {
        length: Const,
        op1: ConstOrAddress,
        op2: ConstOrAddress,
        dst: Address,
    },
    Mult {
        length: Const,
        op1: ConstOrAddress,
        op2: ConstOrAddress,
        dst: Address,
    },
    Neg {
        length: Const,
        op: ConstOrAddress,
        dst: Address,
    },
}

#[derive(Debug)]
pub struct Source {
    pub file: Rc<PathBuf>,
    pub line: usize,
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub ops: Vec<(Operation, Source)>,
    pub labels: HashMap<String, usize>,
}

impl ToString for Const {
    fn to_string(&self) -> String {
        match self {
            Const::Literal { bytes } => bytes_to_string(bytes),
            Const::Stack {
                from_offset,
                length,
            } => String::from("${") + &bytes_to_string(from_offset) + ", " + &bytes_to_string(length) + "}",
            Const::Symbol { name } => String::from(":") + name,
        }
    }
}

#[test]
fn test_to_string() {
    assert_eq!(
        String::from("4"),
        Const::Literal {
            bytes: operations::bytes_from_i32(4)
        }
        .to_string()
    );
    //    assert_eq!(
    //        String::from("${4, 16_L}"),
    //        Const::Stack {
    //            from_offset: Bytes::from_i32(4),
    //            length: Bytes::from_i64(16)
    //        }
    //        .to_string()
    //    );
    assert_eq!(
        String::from(":symbol"),
        Const::Symbol {
            name: String::from("symbol")
        }
        .to_string()
    );
}
