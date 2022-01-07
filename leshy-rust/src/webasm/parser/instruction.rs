use std::io::{Read, Seek};
use crate::webasm::ast::{FuncIdx, Instruction, LocalIdx, NumType};
use crate::webasm::parser::common::{read_i32, Result};

impl Instruction {
    pub fn read_non_blocked(src: &mut (impl Read + Seek), opcode: u8) -> Result<Instruction> {
        Ok(match opcode {
            0x0F => { Instruction::Return }
            0x10 => { Instruction::Call(FuncIdx::read(src)?) }
            0x20 => { Instruction::LocalGet(LocalIdx::read(src)?) }
            0x41 => { Instruction::I32Const(read_i32(src)?) }
            0x46 => { Instruction::Eq(NumType::I32) }
            0x6A => { Instruction::Add(NumType::I32) }
            0x6B => { Instruction::Sub(NumType::I32) }
            _ => { panic!("unsupported opcode {}", opcode) }
        })
    }
}