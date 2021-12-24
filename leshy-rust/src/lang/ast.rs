use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use crate::lang::common::Bytes;

#[derive(Clone, Debug)]
pub enum Const {
    Literal { bytes: Bytes },
    Stack { from_offset: Bytes, length: Bytes },
    Symbol { name: String }, // get resolved to 4 bytes during execution
}

#[derive(Debug)]
pub enum Address {
    // Address in stack corresponding to `address`
    // #const
    Stack {
        address: Const,
    },

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
    Native {
        stack_offset: Const,
    },
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
            Const::Literal { bytes } => bytes.to_string(),
            Const::Stack {
                from_offset,
                length,
            } => String::from("${") + &from_offset.to_string() + ", " + &length.to_string() + "}",
            Const::Symbol { name } => String::from(":") + name,
        }
    }
}

#[test]
fn test_to_string() {
    assert_eq!(
        String::from("4"),
        Const::Literal {
            bytes: Bytes::from_i32(4)
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
