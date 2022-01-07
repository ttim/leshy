use std::collections::HashMap;
use crate::webasm::lazy::Lazy;

#[derive(Debug)]
pub struct Module {
    pub type_section: Option<Lazy<TypeSection>>,
    pub func_section: Option<Lazy<FuncSection>>,
    pub export_section: Option<Lazy<ExportSection>>,
    pub code_section: Option<Lazy<CodeSection>>,
    pub other_sections: Vec<(u8, Lazy<UnrecognizedSection>)>,
}

#[derive(Debug)]
pub struct TypeSection(pub Vec<FuncType>);

#[derive(Debug)]
pub struct FuncSection(pub Vec<TypeIdx>);

#[derive(Debug)]
pub struct ExportSection(pub Vec<Export>);

#[derive(Debug)]
pub struct CodeSection(pub Vec<Code>);

#[derive(Debug)]
pub struct UnrecognizedSection(pub String);

#[derive(Debug)]
pub struct FuncType {
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug)]
pub enum RefType {
    Func,
    // Extern,
}

#[derive(Debug)]
pub enum ValType {
    Num(NumType),
    Ref(RefType),
}

#[derive(Debug)]
pub struct TypeIdx(pub u32);

#[derive(Debug, PartialEq, Eq)]
pub struct LocalIdx(pub u32);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncIdx(pub u32);

#[derive(Debug)]
pub struct GlobalIdx(pub u32);

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub tag: ExportTag,
    pub idx: u32,
}

#[derive(Debug, PartialEq)]
pub enum ExportTag {
    Func,
    Table,
    Mem,
    Global,
}

#[derive(Debug)]
pub struct Instructions {
    pub instructions: Vec<Instruction>,
    // (idx of blocked instruction) -> (idx of next instruction after block)
    pub blocks: HashMap<InstructionIdx, InstructionIdx>,
}

#[derive(Debug)]
pub struct Code {
    pub locals: Vec<Locals>,
    pub expr: Lazy<Instructions>,
}

#[derive(Debug)]
pub struct Locals {
    pub n: u32,
    pub tpe: ValType,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    If { bt: BlockType },
    Else,
    BlockEnd,
    Return,
    Call(FuncIdx),

    LocalGet(LocalIdx),
    I32Const(i32),
    Eq(NumType),
    Add(NumType),
    Sub(NumType),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstructionIdx(pub u32);

#[derive(Debug, PartialEq, Eq)]
pub enum BlockType {
    Empty,
    // ValType(ValType),
    // TypeIdx(TypeIdx),
}
