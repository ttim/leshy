use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use lazycell::LazyCell;
use crate::webasm::ast::*;
use crate::webasm::lazy::{Lazy, Readable};
use crate::webasm::parser::common::{read_string, read_u32, read_u8, read_vector, Result};
use crate::webasm::parser::hydrate::hydrate_module;

// TODO: implement/use serde instead?
// we can create abstraction of "tagged/with id" sources and keep this (uu)id in structs to enforce usage with same underlying reader

impl Module {
    pub fn read(src: &mut (impl Read + Seek)) -> Result<Module> {
        let mut asm_buf: [u8; 4] = [0; 4];
        src.read_exact(&mut asm_buf)?;
        assert_eq!("\0asm".as_bytes(), asm_buf);

        let mut version_buf: [u8; 4] = [0; 4];
        src.read_exact(&mut version_buf)?;
        assert_eq!(1, u32::from_le_bytes(version_buf));

        let mut module = Module {
            type_section: None,
            func_section: None,
            export_section: None,
            code_section: None,
            other_sections: vec![],
        };
        loop {
            let mut id = [0u8; 1];
            if src.read(&mut id)? == 0 { break; }

            let content_size = read_u32(src)? as i64;
            let start_offset = src.stream_position()? as u32;
            let finish_offset = src.seek(SeekFrom::Current(content_size))? as u32;

            match id[0] {
                1 => { Self::set_section(&mut module.type_section, start_offset, finish_offset) }
                3 => { Self::set_section(&mut module.func_section, start_offset, finish_offset) }
                7 => { Self::set_section(&mut module.export_section, start_offset, finish_offset) }
                10 => { Self::set_section(&mut module.code_section, start_offset, finish_offset) }
                other => {
                    let section = Lazy::<UnrecognizedSection> { start_offset, finish_offset, cell: LazyCell::new() };
                    module.other_sections.push((other, section));
                }
            }
        }

        Ok(module)
    }

    fn set_section<T>(dst: &mut Option<Lazy<T>>, start_offset: u32, finish_offset: u32) {
        *dst = Some(Lazy { start_offset, finish_offset, cell: LazyCell::new() })
    }
}

impl Readable for TypeSection {
    type Error = crate::webasm::parser::common::Error;
    fn read(src: &mut (impl Read + Seek), _: u64) -> Result<Self> {
        Ok(TypeSection(read_vector(src, |src| FuncType::read(src))?))
    }
}

impl Readable for FuncSection {
    type Error = crate::webasm::parser::common::Error;
    fn read(src: &mut (impl Read + Seek), _: u64) -> Result<Self> {
        Ok(FuncSection(read_vector(src, |src| TypeIdx::read(src))?))
    }
}

impl Readable for ExportSection {
    type Error = crate::webasm::parser::common::Error;
    fn read(src: &mut (impl Read + Seek), _: u64) -> Result<Self> {
        Ok(ExportSection(read_vector(src, |src| Export::read(src))?))
    }
}

impl Readable for CodeSection {
    type Error = crate::webasm::parser::common::Error;
    fn read(src: &mut (impl Read + Seek), _: u64) -> Result<Self> {
        Ok(CodeSection(read_vector(src, |src| Code::read(src))?))
    }
}

impl Readable for UnrecognizedSection {
    type Error = crate::webasm::parser::common::Error;
    fn read(src: &mut (impl Read + Seek), finish_offset: u64) -> Result<Self> {
        let name = read_string(src)?;
        src.seek(SeekFrom::Start(finish_offset))?;
        Ok(UnrecognizedSection(name))
    }
}

impl Export {
    fn read(src: &mut (impl Read + Seek)) -> Result<Export> {
        Ok(Export {
            name: read_string(src)?,
            tag: Export::read_tag(src)?,
            idx: read_u32(src)?,
        })
    }

    fn read_tag(src: &mut (impl Read + Seek)) -> Result<ExportTag> {
        Ok(match read_u8(src)? {
            0 => { ExportTag::Func }
            1 => { ExportTag::Table }
            2 => { ExportTag::Mem }
            3 => { ExportTag::Global }
            other => { panic!("unsupported export tag: {}", other) }
        })
    }
}

impl Code {
    fn read(src: &mut (impl Read + Seek)) -> Result<Code> {
        let size = read_u32(src)?;
        let finish_offset = src.stream_position()? as u32 + size;
        let locals = read_vector(src, |src| {
            Ok(Locals { n: read_u32(src)?, tpe: ValType::read(src)? })
        })?;
        let start_offset = src.stream_position()? as u32;
        src.seek(SeekFrom::Start(finish_offset as u64))?;
        Ok(Code { locals, expr: Lazy { start_offset, finish_offset, cell: LazyCell::new() } })
    }
}

impl Readable for Instructions {
    type Error = crate::webasm::parser::common::Error;
    fn read(src: &mut (impl Read + Seek), _: u64) -> Result<Self> { Instructions::read(src) }
}

impl Instructions {
    fn read(src: &mut (impl Read + Seek)) -> Result<Instructions> {
        let mut vec = vec![];
        Self::read_internal(src, |b| b == 0x0B, &mut vec)?;
        Ok(Instructions(vec))
    }

    fn read_internal<F: Fn(u8) -> bool>(src: &mut (impl Read + Seek), break_on: F, instructions: &mut Vec<Instruction>) -> Result<u8> {
        loop {
            let opcode = read_u8(src)?;
            if break_on(opcode) { return Ok(opcode); };
            if opcode == 0x02 || opcode == 0x03 || opcode == 0x04 {
                Self::read_block(src, opcode, instructions)?
            } else {
                instructions.push(Instruction::read(src, opcode)?)
            }
        }
    }

    fn read_block(src: &mut (impl Read + Seek), opcode: u8, instructions: &mut Vec<Instruction>) -> Result<()> {
        instructions.push(Instruction::__Temporary);
        let idx = instructions.len() - 1;

        match opcode {
            0x04 => {
                let bt = Self::read_block_type(src)?;
                let finish_opcode = Self::read_internal(src, |opcode| opcode == 0x0B || opcode == 0x05, instructions)?;
                if finish_opcode == 0x0B {
                    let next = InstructionIdx(instructions.len() as u32);
                    instructions[idx] = Instruction::If { bt, if_false: None, next };
                } else {
                    todo!()
                }
            }
            other => {
                panic!("unsupported block opcode {}", other)
            }
        }

        Ok(())
    }

    fn read_block_type(src: &mut (impl Read + Seek)) -> Result<BlockType> {
        Ok(match read_u8(src)? {
            0x40 => { BlockType::Empty }
            other => { panic!("unsupported block type {}", other) }
        })
    }
}

impl FuncType {
    fn read(src: &mut (impl Read + Seek)) -> Result<FuncType> {
        assert_eq!(0x60, read_u8(src)?);
        Ok(FuncType {
            params: read_vector(src, |src| ValType::read(src))?,
            results: read_vector(src, |src| ValType::read(src))?,
        })
    }
}

impl ValType {
    fn read(src: &mut (impl Read + Seek)) -> Result<ValType> {
        Ok(match read_u8(src)? {
            0x7F => { ValType::Num(NumType::I32) }
            0x7E => { ValType::Num(NumType::I64) }
            0x7D => { ValType::Num(NumType::F32) }
            0x7C => { ValType::Num(NumType::F64) }
            0x70 => { ValType::Ref(RefType::Func) }
            0x6F => { ValType::Ref(RefType::Func) }
            other => { panic!("unsupported val type {}", other); }
        })
    }
}

impl TypeIdx {
    pub fn read(src: &mut (impl Read + Seek)) -> Result<TypeIdx> { Ok(TypeIdx(read_u32(src)?)) }
}

impl LocalIdx {
    pub fn read(src: &mut (impl Read + Seek)) -> Result<LocalIdx> { Ok(LocalIdx(read_u32(src)?)) }
}

impl FuncIdx {
    pub fn read(src: &mut (impl Read + Seek)) -> Result<FuncIdx> { Ok(FuncIdx(read_u32(src)?)) }
}

#[test]
fn test() {
    let mut file = File::open("data/fib.wasm").unwrap();
    let module = Module::read(&mut file).unwrap();
    hydrate_module(&module, &mut file);
    println!("{:#?}", module);
}
