use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::ops::DerefMut;
use std::rc::Rc;
use crate::api::{Command, Node, NodeKind, Ref, traverse_node};
use crate::webasm::ast::{Code, ExportTag, FuncIdx, FuncType, Instruction, InstructionIdx, LocalIdx, Module, NumType, ValType};
use crate::webasm::lazy::{Lazy, Readable};
use crate::webasm::parser::hydrate::hydrate_module;

struct Source {
    uuid: u128,
    name: String,
    file: RefCell<File>,
    module: Module,
}

impl Debug for Source {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)?;
        f.write_str(":")?;
        self.uuid.fmt(f)
    }
}

#[derive(Debug)]
struct WebAsmNode {
    source: Rc<Source>,
    func: FuncIdx,
    inst: InstructionIdx,
}

impl WebAsmNode {
    fn func(source: Rc<Source>, func: FuncIdx) -> WebAsmNode {
        WebAsmNode { source, func, inst: InstructionIdx(0) }
    }

    fn exported_func(source: Rc<Source>, name: &str) -> WebAsmNode {
        match &source.module.export_section {
            None => { panic!() }
            Some(section) => {
                let export = section.get(&mut source.file.borrow_mut().deref_mut());
                let entry = export.0.iter().find(|export| export.name == name);
                assert_eq!(ExportTag::Func, entry.unwrap().tag);
                Self::func(source.clone(), FuncIdx(entry.unwrap().idx))
            }
        }
    }

    fn instruction(&self) -> &Instruction {
        let instructions = self.get(&self.code().expr);
        instructions.0.get(self.inst.0 as usize).unwrap()
    }

    fn code(&self) -> &Code {
        self.get_option(&self.source.module.code_section).0.get(self.func.0 as usize).unwrap()
    }

    fn func_type(&self) -> &FuncType {
        let type_id = self.get_option(&self.source.module.func_section).0.get(self.func.0 as usize).unwrap();
        self.get_option(&self.source.module.type_section).0.get(type_id.0 as usize).unwrap()
    }

    fn get<'a, T: Readable>(&'a self, lazy: &'a Lazy<T>) -> &'a T {
        lazy.get(self.source.file.borrow_mut().deref_mut())
    }

    fn get_option<'a, T: Readable>(&'a self, lazy_opt: &'a Option<Lazy<T>>) -> &'a T {
        match &lazy_opt {
            None => { todo!() }
            Some(lazy) => { self.get(lazy) }
        }
    }

    fn local_ref(&self, id: &LocalIdx) -> Ref {
        if (id.0 as usize) < self.func_type().params.len() {
            let prev_locals = &self.func_type().params.as_slice()[0..id.0 as usize];
            let offset = prev_locals.iter().map(|local| Self::val_type_size(local) as u32).sum();
            Ref::Stack { offset }
        } else {
            todo!()
        }
    }

    fn local_size(&self, id: &LocalIdx) -> u8 {
        if (id.0 as usize) < self.func_type().params.len() {
            Self::val_type_size(self.func_type().params.get(id.0 as usize).unwrap())
        } else {
            todo!()
        }
    }

    fn val_type_size(val_type: &ValType) -> u8 {
        match val_type {
            ValType::Num(num_type) => { Self::num_type_size(num_type) }
            ValType::Ref(_) => { todo!() }
        }
    }

    fn num_type_size(num_type: &NumType) -> u8 {
        match num_type {
            NumType::I32 => { 4 }
            NumType::I64 => { 8 }
            NumType::F32 => { 4 }
            NumType::F64 => { 8 }
        }
    }

    fn next(&self) -> WebAsmNode {
        WebAsmNode {
            source: self.source.clone(),
            func: self.func.clone(),
            inst: InstructionIdx(self.inst.0 + 1),
        }
    }

    fn command(&self, command: Command) -> NodeKind<WebAsmNode> {
        NodeKind::Command { command, next: self.next() }
    }

    fn after_last_instruction(&self) -> bool {
        self.inst.0 as usize == self.get(&self.code().expr).0.len()
    }
}

impl Hash for WebAsmNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u128(self.source.uuid);
        state.write_u32(self.func.0);
        state.write_u32(self.inst.0);
    }
}

impl Eq for WebAsmNode {}

impl PartialEq<Self> for WebAsmNode {
    fn eq(&self, other: &Self) -> bool {
        self.source.uuid == other.source.uuid &&
            self.func.0 == other.func.0 &&
            self.inst.0 == other.inst.0
    }
}

impl Node for WebAsmNode {
    fn get(&self) -> NodeKind<Self> {
        println!("getting: {:?}", &self);

        let kind = if self.after_last_instruction() {
            NodeKind::Final
        } else {
            match self.instruction() {
                Instruction::If { .. } => { todo!() }
                Instruction::Return => { todo!() }
                Instruction::Call(_) => { todo!() }
                Instruction::LocalGet(id) => {
                    self.command(Command::Push { src: self.local_ref(id), size: self.local_size(id) as u32 })
                }
                Instruction::I32Const(value) => {
                    self.command(Command::PushConst { bytes: value.to_le_bytes().to_vec() })
                }
                Instruction::Eq(_) => { todo!() }
                Instruction::Add(_) => { todo!() }
                Instruction::Sub(_) => { todo!() }
                Instruction::__Temporary => { todo!() }
            }
        };

        println!("computed: {:?}", kind);
        kind
    }
}

#[test]
fn test_node_creation() {
    let name = String::from("data/fib.wasm");
    let mut file = File::open(&name).unwrap();
    let module = Module::read(&mut file).unwrap();
    hydrate_module(&module, &mut file);
    let source = Rc::new(Source { uuid: 0, name: name.clone(), file: RefCell::new(file), module });
    let fib = WebAsmNode::exported_func(source.clone(), "fib");
    traverse_node(fib);
}