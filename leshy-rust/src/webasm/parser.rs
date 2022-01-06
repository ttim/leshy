use std::any::type_name;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use lazycell::LazyCell;
use crate::webasm::ast::*;

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

    pub fn hydrate(&self, src: &mut (impl Read + Seek)) {
        Self::hydrate_section(&self.type_section, src);
        Self::hydrate_section(&self.func_section, src);
        Self::hydrate_section(&self.export_section, src);
        Self::hydrate_section(&self.code_section, src);
        self.other_sections.iter().for_each(|section| section.1.hydrate(src) );
    }

    fn set_section<T>(dst: &mut Option<Lazy<T>>, start_offset: u32, finish_offset: u32) {
        *dst = Some(Lazy { start_offset, finish_offset, cell: LazyCell::new() })
    }

    fn hydrate_section<T, V: LazyImpl<T>>(dst: &Option<V>, src: &mut (impl Read + Seek)) {
        if let Some(section) = &dst {
            section.hydrate(src)
        }
    }
}

pub trait LazyImpl<T> {
    fn get(&self, src: &mut (impl Read + Seek)) -> &T;

    fn hydrate(&self, src: &mut (impl Read + Seek)) {
        self.get(src);
    }
}

impl LazyImpl<TypeSection> for Lazy<TypeSection> {
    fn get(&self, src: &mut (impl Read + Seek)) -> &TypeSection {
        get_lazy(self, src, |src| {
            Ok(TypeSection(read_vector(src, |src| FuncType::read(src))?))
        })
    }
}

impl LazyImpl<FuncSection> for Lazy<FuncSection> {
    fn get(&self, src: &mut (impl Read + Seek)) -> &FuncSection {
        get_lazy(self, src, |src| {
            Ok(FuncSection(read_vector(src, |src| TypeIdx::read(src))?))
        })
    }
}

impl LazyImpl<ExportSection> for Lazy<ExportSection> {
    fn get(&self, src: &mut (impl Read + Seek)) -> &ExportSection {
        get_lazy(self, src, |src| {
            Ok(ExportSection(read_vector(src, |src| Export::read(src))?))
        })
    }
}

impl LazyImpl<CodeSection> for Lazy<CodeSection> {
    fn get(&self, src: &mut (impl Read + Seek)) -> &CodeSection {
        get_lazy(self, src, |src| {
            Ok(CodeSection(read_vector(src, |src| Code::read(src))?))
        })
    }

    fn hydrate(&self, src: &mut (impl Read + Seek)) {
        self.get(src).0.iter().for_each(|code| code.expr.hydrate(src))
    }
}

impl LazyImpl<UnrecognizedSection> for Lazy<UnrecognizedSection> {
    fn get(&self, src: &mut (impl Read + Seek)) -> &UnrecognizedSection {
        get_lazy(self, src, |src| {
            let name = read_string(src)?;
            src.seek(SeekFrom::Start(self.finish_offset as u64));
            Ok(UnrecognizedSection(name))
        })
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

impl LazyImpl<Vec<Instruction>> for Lazy<Vec<Instruction>> {
    fn get(&self, src: &mut (impl Read + Seek)) -> &Vec<Instruction> {
        get_lazy(self, src, |src| { Instruction::read_multiple(src) })
    }
}

impl Instruction {
    fn read_multiple(src: &mut (impl Read + Seek)) -> Result<Vec<Instruction>> {
        let mut vec = vec![];
        Self::read_internal(src, |b| b == 0x0B, &mut vec)?;
        Ok(vec)
    }

    fn read_internal<F: Fn(u8) -> bool>(src: &mut (impl Read + Seek), break_on: F, instructions: &mut Vec<Instruction>) -> Result<u8> {
        loop {
            let opcode = read_u8(src)?;
            if break_on(opcode) { return Ok(opcode); };
            if opcode == 0x02 || opcode == 0x03 || opcode == 0x04 {
                Self::read_block(src, opcode, instructions)?
            } else {
                instructions.push(Self::read_single(src, opcode)?)
            }
        }
    }

    fn read_single(src: &mut (impl Read + Seek), opcode: u8) -> Result<Instruction> {
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
    fn read(src: &mut (impl Read + Seek)) -> Result<TypeIdx> { Ok(TypeIdx(read_u32(src)?)) }
}

impl LocalIdx {
    fn read(src: &mut (impl Read + Seek)) -> Result<LocalIdx> { Ok(LocalIdx(read_u32(src)?)) }
}

impl FuncIdx {
    fn read(src: &mut (impl Read + Seek)) -> Result<FuncIdx> { Ok(FuncIdx(read_u32(src)?)) }
}

// Utilities

fn read_u32(src: &mut (impl Read + Seek)) -> Result<u32> {
    Ok(u32::try_from(leb128::read::unsigned(src)?)?)
}

fn read_i32(src: &mut (impl Read + Seek)) -> Result<i32> {
    Ok(i32::try_from(leb128::read::signed(src)?)?)
}

fn read_u8(src: &mut (impl Read + Seek)) -> Result<u8> {
    let mut buf: [u8; 1] = [0; 1];
    src.read_exact(&mut buf)?;
    Ok(buf[0])
}

fn read_vector<T: Read + Seek, V, F: Fn(&mut T) -> Result<V>>(src: &mut T, parse_once: F) -> Result<Vec<V>> {
    let len = read_u32(src)?;
    let mut result = Vec::with_capacity(len as usize);
    for _ in 0..len {
        result.push(parse_once(src)?)
    }
    Ok(result)
}

fn read_string(src: &mut (impl Read + Seek)) -> Result<String> {
    Ok(String::from_utf8(read_vector(src, |src| read_u8(src))?)?)
}

fn get_lazy<'a, 'b, T, S: Read + Seek, F: Fn(&mut S) -> Result<T>>(lazy: &'a Lazy<T>, src: &'b mut S, read: F) -> &'a T {
    lazy.cell.borrow_with(|| {
        src.seek(SeekFrom::Start(lazy.start_offset as u64)).unwrap();
        let result = read(src).unwrap();
        assert_eq!(lazy.finish_offset as u64, src.stream_position().unwrap());
        result
    })
}

impl<T: Debug> std::fmt::Debug for Lazy<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.cell.filled() {
            self.cell.borrow().unwrap().fmt(f)
        } else {
            f.write_str(&format!("<lazy {} {}..{}>", type_name::<T>(), self.start_offset, self.finish_offset))
        }
    }
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error(Box<dyn std::error::Error>);

impl<T: std::error::Error + 'static> From<T> for Error {
    fn from(error: T) -> Self {
        Error(Box::from(error))
    }
}

#[test]
fn test() {
    let mut file = File::open("data/fib.wasm").unwrap();
    let module = Module::read(&mut file).unwrap();
    module.hydrate(&mut file);
    println!("{:#?}", module);
}
