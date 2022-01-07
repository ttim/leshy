use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::ops::DerefMut;
use std::rc::Rc;
use crate::api::{Command, Node, NodeKind, Ref, stack_size_change, traverse_node};
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

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
enum WebAsmNode {
    Instruction(InstructionNode),
    Intermediate(Box<NodeKind<WebAsmNode>>),
}

#[derive(Debug, Clone)]
struct InstructionNode {
    source: Rc<Source>,
    func: FuncIdx,
    inst: InstructionIdx,
    stack_size: u32,
}

impl InstructionNode {
    fn func(source: Rc<Source>, func: FuncIdx) -> InstructionNode {
        InstructionNode { source, func, inst: InstructionIdx(0), stack_size: 0 }
    }

    fn exported_func(source: Rc<Source>, name: &str) -> InstructionNode {
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

    fn next(&self, stack_size_change: i32) -> WebAsmNode {
        WebAsmNode::Instruction(InstructionNode {
            source: self.source.clone(),
            func: self.func.clone(),
            inst: InstructionIdx(self.inst.0 + 1),
            stack_size: ((self.stack_size as i32) + stack_size_change) as u32
        })
    }

    fn command(&self, command: Command) -> NodeKind<WebAsmNode> {
        let change = stack_size_change(&command);
        NodeKind::Command { command, next: self.next(change) }
    }

    fn after_last_instruction(&self) -> bool {
        self.inst.0 as usize == self.get(&self.code().expr).0.len()
    }

    fn get_kind(&self) -> NodeKind<WebAsmNode> {
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
                Instruction::Eq(num_type) => {
                    let size = InstructionNode::num_type_size(num_type) as u32;
                    let shrink_size = size * 2 - 4;
                    let cmd1 = Command::Eq {
                        size,
                        op1: Ref::Stack { offset: self.stack_size - size * 2 },
                        op2: Ref::Stack { offset: self.stack_size - size },
                        dst: Ref::Stack { offset: self.stack_size - size * 2 },
                    };
                    let cmd2 = Command::Shrink { size: shrink_size }; // - 2 operands + 1 bool
                    let inner_cmd = NodeKind::Command { command: cmd2, next: self.next(-(shrink_size as i32)) };

                    NodeKind::Command {
                        command: cmd1,
                        next: WebAsmNode::Intermediate(Box::new(inner_cmd)),
                    }
                }
                Instruction::Add(_) => { todo!() }
                Instruction::Sub(_) => { todo!() }
                Instruction::__Temporary => { panic!("can't have temporary nodes in parsed content") }
            }
        };

        println!("computed: {:?}", kind);
        kind
    }
}

impl WebAsmNode {
    pub fn exported_func(source: Rc<Source>, name: &str) -> WebAsmNode {
        WebAsmNode::Instruction(InstructionNode::exported_func(source, name))
    }
}

impl Hash for InstructionNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u128(self.source.uuid);
        state.write_u32(self.func.0);
        state.write_u32(self.inst.0);
    }
}

impl Eq for InstructionNode {}

impl PartialEq<Self> for InstructionNode {
    fn eq(&self, other: &Self) -> bool {
        self.source.uuid == other.source.uuid &&
            self.func.0 == other.func.0 &&
            self.inst.0 == other.inst.0
    }
}

impl Node for WebAsmNode {
    fn get(&self) -> NodeKind<Self> {
        match self {
            WebAsmNode::Instruction(node) => { node.get_kind() }
            WebAsmNode::Intermediate(kind) => { (*kind.as_ref()).clone() }
        }
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