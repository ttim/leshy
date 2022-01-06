use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::ops::DerefMut;
use std::rc::Rc;
use crate::api::{Node, NodeKind, traverse_node};
use crate::webasm::ast::{Code, ExportTag, FuncIdx, Instruction, InstructionIdx, Module};
use crate::webasm::parser::hydrate;

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
        match &self.source.module.code_section {
            None => { panic!() }
            Some(section) => {
                let mut src_ref = self.source.file.borrow_mut();
                let code = section.get(src_ref.deref_mut());
                let entry: &Code = code.0.get(self.func.0 as usize).unwrap();
                let instructions = entry.expr.get(src_ref.deref_mut());
                let inst = instructions.0.get(self.inst.0 as usize).unwrap();
                inst
            }
        }
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
        println!("getting content for {:?}", &self);
        match self.instruction() {
            Instruction::If { .. } => { todo!() }
            Instruction::Return => { todo!() }
            Instruction::Call(_) => { todo!() }
            Instruction::LocalGet(_) => { todo!() }
            Instruction::I32Const(_) => { todo!() }
            Instruction::Eq(_) => { todo!() }
            Instruction::Add(_) => { todo!() }
            Instruction::Sub(_) => { todo!() }
            Instruction::__Temporary => { todo!() }
        }
    }
}

#[test]
fn test_node_creation() {
    let name = String::from("data/fib.wasm");
    let mut file = File::open(&name).unwrap();
    let module = Module::read(&mut file).unwrap();
    hydrate::module(&module, &mut file);
    let source = Rc::new(Source { uuid: 0, name: name.clone(), file: RefCell::new(file), module });
    let fib = WebAsmNode::exported_func(source.clone(), "fib");
    traverse_node(fib);
}