use lazycell::LazyCell;

#[derive(Debug)]
pub struct Module(pub Vec<Lazy<Section>>);

#[derive(Debug)]
pub enum Section {
    Type(Vec<FuncType>),
    Func(Vec<TypeIdx>),
    Export(Vec<Export>),
    Code(Vec<Code>),
    NotRecognized,
}

#[derive(Debug)]
pub struct FuncType {
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

#[derive(Debug)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug)]
pub enum RefType {
    Func,
    Extern,
}

#[derive(Debug)]
pub enum ValType {
    Num(NumType),
    Ref(RefType),
}

#[derive(Debug)]
pub struct TypeIdx(pub u32);

#[derive(Debug)]
pub struct LocalIdx(pub u32);

#[derive(Debug)]
pub struct FuncIdx(pub u32);

#[derive(Debug)]
pub struct GlobalIdx(pub u32);

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub tag: ExportTag,
    pub idx: u32,
}

#[derive(Debug)]
pub enum ExportTag {
    Func,
    Table,
    Mem,
    Global,
}

#[derive(Debug)]
pub struct Code {
    pub locals: Vec<Locals>,
    pub expr: Lazy<Vec<Instruction>>,
}

#[derive(Debug)]
pub struct Locals {
    pub n: u32,
    pub tpe: ValType,
}

#[derive(Debug)]
pub enum Instruction {
    If { bt: BlockType, if_false: Option<InstructionId>, next: InstructionId },
    Return,
    Call(FuncIdx),
    LocalGet(LocalIdx),
    I32Const(i32),
    Eq(NumType),
    Add(NumType),
    Sub(NumType),
    __Temporary,
}

#[derive(Debug)]
pub struct InstructionId(pub u32);

#[derive(Debug)]
pub enum BlockType {
    Empty,
    ValType(ValType),
    TypeIdx(TypeIdx),
}

pub struct Lazy<T> {
    pub(crate) start_offset: u32,
    pub(crate) finish_offset: u32,
    // todo: replace with OnceCell once it's in stable rust
    pub(crate) cell: LazyCell<T>,
}
