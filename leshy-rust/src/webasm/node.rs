use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::ops::DerefMut;
use std::rc::Rc;
use crate::core::api::{Command, Condition, Node, NodeKind, Ref};
use crate::core::utils::{pretty_print, traverse_node};
use crate::webasm::ast::{Code, CodeSection, ExportSection, ExportTag, FuncIdx, FuncSection, FuncType, Instruction, InstructionIdx, Instructions, LocalIdx, Module, NumType, TypeSection, ValType};
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
    CallFunc(CallFuncNode),
    Instruction(InstructionNode),
    Intermediate(Box<NodeKind<WebAsmNode>>),
}

#[derive(Debug, Clone)]
struct FuncContext {
    source: Rc<Source>,
    id: FuncIdx,
}

#[derive(Debug, Clone)]
struct CallFuncNode {
    ctx: Rc<FuncContext>,
}

#[derive(Debug, Clone)]
struct InstructionNode {
    ctx: Rc<FuncContext>,
    inst: InstructionIdx,
    stack_size: u32,
}

impl Source {
    fn export_section(&self) -> &ExportSection { self.get_option(&self.module.export_section) }
    fn code_section(&self) -> &CodeSection { self.get_option(&self.module.code_section) }
    fn func_section(&self) -> &FuncSection { self.get_option(&self.module.func_section) }
    fn type_section(&self) -> &TypeSection { self.get_option(&self.module.type_section) }

    fn func_type(&self, id: &FuncIdx) -> &FuncType {
        let type_id = self.func_section().0.get(id.0 as usize).unwrap();
        self.type_section().0.get(type_id.0 as usize).unwrap()
    }

    fn get<'a, T: Readable>(&'a self, lazy: &'a Lazy<T>) -> &'a T {
        lazy.get(self.file.borrow_mut().deref_mut())
    }

    fn get_option<'a, T: Readable>(&'a self, lazy_opt: &'a Option<Lazy<T>>) -> &'a T {
        match &lazy_opt {
            None => { todo!() }
            Some(lazy) => { self.get(lazy) }
        }
    }
}

fn size_of(items: &Vec<ValType>) -> u32 {
    items.iter().map(|i| InstructionNode::val_type_size(i) as u32).sum()
}

impl FuncContext {
    fn by_id(source: Rc<Source>, id: FuncIdx) -> FuncContext {
        FuncContext { source, id }
    }

    fn exported(source: Rc<Source>, name: &str) -> FuncContext {
        let export = source.export_section().0.iter().find(|export| export.name == name);
        assert_eq!(ExportTag::Func, export.unwrap().tag);
        Self::by_id(source.clone(), FuncIdx(export.unwrap().idx))
    }

    fn code(&self) -> &Code { self.source.code_section().0.get(self.id.0 as usize).unwrap() }
    fn instructions(&self) -> &Instructions { self.source.get(&self.code().expr) }
    fn block_end(&self, idx: &InstructionIdx) -> &InstructionIdx { self.instructions().blocks.get(idx).unwrap() }

    fn func_type(&self) -> &FuncType { self.source.func_type(&self.id) }
}

impl CallFuncNode {
    fn get_kind(&self) -> NodeKind<WebAsmNode> {
        let stack_size = size_of(&self.ctx.func_type().params);
        NodeKind::Command {
            command: Command::Noop, // todo: allocate locals here
            next: WebAsmNode::Instruction(
                InstructionNode { ctx: self.ctx.clone(), inst: InstructionIdx(0), stack_size }
            ),
        }
    }
}

impl InstructionNode {
    fn instruction(&self) -> &Instruction {
        self.ctx.instructions().instructions.get(self.inst.0 as usize).unwrap()
    }

    fn local_ref(&self, id: &LocalIdx) -> Ref {
        if (id.0 as usize) < self.ctx.func_type().params.len() {
            let prev_locals = &self.ctx.func_type().params.as_slice()[0..id.0 as usize];
            let offset = prev_locals.iter().map(|local| Self::val_type_size(local) as u32).sum();
            Ref::Stack(offset)
        } else {
            todo!()
        }
    }

    fn local_size(&self, id: &LocalIdx) -> u8 {
        if (id.0 as usize) < self.ctx.func_type().params.len() {
            Self::val_type_size(self.ctx.func_type().params.get(id.0 as usize).unwrap())
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

    fn next(&self, stack_size_delta: i32) -> WebAsmNode {
        WebAsmNode::Instruction(InstructionNode {
            ctx: self.ctx.clone(),
            inst: InstructionIdx(self.inst.0 + 1),
            stack_size: ((self.stack_size as i32) + stack_size_delta) as u32
        })
    }

    fn goto(&self, op: InstructionIdx) -> WebAsmNode {
        WebAsmNode::Instruction(InstructionNode {
            ctx: self.ctx.clone(),
            inst: op,
            stack_size: self.stack_size
        })
    }

    fn last_instruction(&self) -> bool {
        self.inst.0 as usize == self.ctx.instructions().instructions.len() - 1
    }

    fn get_kind(&self) -> NodeKind<WebAsmNode> {
        // println!("getting: {:?}", &self);

        let kind = match self.instruction() {
            Instruction::Nop => {
                NodeKind::Command { command: Command::Noop, next: self.next(0) }
            }
            // block type not really needed apart from validation purposes
            Instruction::If { bt: _ } => {
                NodeKind::Branch {
                    condition: Condition::Ne0 { size: 4, src: Ref::Stack(self.stack_size - 4) },
                    if_true: self.next(0),
                    if_false: {
                        let block_end = self.ctx.block_end(&self.inst);
                        // it ends up either with "else" or "block_end", regardless to which one on else we want to go into either of them
                        self.goto(InstructionIdx(block_end.0 + 1))
                    },
                }
            }
            Instruction::Else => { todo!() }
            Instruction::BlockEnd => {
                if self.last_instruction() {
                    NodeKind::Final
                } else {
                    NodeKind::Command { command: Command::Noop, next: self.next(0) }
                }
            }
            Instruction::Return => {
                self.ret()
            }
            Instruction::Call(id) => {
                let params_size = size_of(&self.ctx.source.func_type(id).params);
                let result_size = size_of(&self.ctx.source.func_type(id).results);
                NodeKind::Call {
                    offset: self.stack_size - params_size,
                    call: WebAsmNode::CallFunc(CallFuncNode { ctx: Rc::new(FuncContext { source: self.ctx.source.clone(), id: id.clone() }) }),
                    next: self.next((result_size as i32) - (params_size as i32))
                }
            }
            Instruction::LocalGet(id) => {
                self.push(self.local_size(id) as u32, self.local_ref(id))
            }
            Instruction::I32Const(value) => {
                self.push_const(value.to_le_bytes().to_vec())
            }
            Instruction::Eq(num_type) => {
                let size = InstructionNode::num_type_size(num_type) as u32;

                let next_node = |byte_to_write: u32| -> WebAsmNode {
                    let dst = Ref::Stack(self.stack_size - size * 2);
                    WebAsmNode::Intermediate(Box::new(NodeKind::Command {
                        command: Command::Set { dst, bytes: byte_to_write.to_le_bytes().to_vec() },
                        next: WebAsmNode::Intermediate(
                            Box::new(NodeKind::Command {
                                command: Command::PoisonFrom { dst: Ref::Stack(self.stack_size - size * 2 + 4) },
                                next: self.next(-((size * 2 - 4) as i32)),
                            })
                        ),
                    }))
                };

                NodeKind::Branch {
                    condition: Condition::Eq {
                        size,
                        op1: Ref::Stack(self.stack_size - size * 2),
                        op2: Ref::Stack(self.stack_size - size),
                    },
                    if_true: next_node(0),
                    if_false: next_node(1),
                }
            }
            Instruction::Add(num_type) => {
                let size = InstructionNode::num_type_size(num_type) as u32;
                NodeKind::Command {
                    command: Command::Add {
                        size,
                        dst: Ref::Stack(self.stack_size),
                        op1: Ref::Stack(self.stack_size - 2 * size),
                        op2: Ref::Stack(self.stack_size - size),
                    },
                    next: self.next(size as i32),
                }
            }
            Instruction::Sub(num_type) => {
                let size = InstructionNode::num_type_size(num_type) as u32;
                NodeKind::Command {
                    command: Command::Sub {
                        size,
                        dst: Ref::Stack(self.stack_size),
                        op1: Ref::Stack(self.stack_size - 2 * size),
                        op2: Ref::Stack(self.stack_size - size),
                    },
                    next: self.next(size as i32),
                }
            }
        };

        // println!("computed: {:?}", kind);
        kind
    }

    fn push_const(&self, bytes: Vec<u8>) -> NodeKind<WebAsmNode> {
        let delta = bytes.len() as i32;
        NodeKind::Command {
            command: Command::Set { dst: Ref::Stack(self.stack_size), bytes },
            next: self.next(delta),
        }
    }

    fn push(&self, size: u32, src: Ref) -> NodeKind<WebAsmNode> {
        NodeKind::Command {
            command: Command::Copy { size, dst: Ref::Stack(self.stack_size), op: src },
            next: self.next(size as i32),
        }
    }

    fn ret(&self) -> NodeKind<WebAsmNode> {
        let ret_size = self.ctx.func_type().results.iter().map(|tpe| InstructionNode::val_type_size(tpe) as u32).sum();
        NodeKind::Command {
            // copy result
            command: Command::Copy {
                dst: Ref::Stack(0),
                size: ret_size,
                op: Ref::Stack(self.stack_size - ret_size),
            },
            next: WebAsmNode::Intermediate(Box::new(NodeKind::Command {
                // clean stack
                command: Command::PoisonFrom { dst: Ref::Stack(ret_size) },
                next: WebAsmNode::Intermediate(Box::new(NodeKind::Final)),
            })),
        }
    }
}

impl WebAsmNode {
    pub fn exported_func(source: Rc<Source>, name: &str) -> WebAsmNode {
        WebAsmNode::CallFunc(CallFuncNode { ctx: Rc::new(FuncContext::exported(source, name)) })
    }
}

impl Hash for InstructionNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u128(self.ctx.source.uuid);
        state.write_u32(self.ctx.id.0);
        state.write_u32(self.inst.0);
    }
}

impl Hash for CallFuncNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // todo: def repeating with ctx
        state.write_u128(self.ctx.source.uuid);
        state.write_u32(self.ctx.id.0);
    }
}

impl Eq for InstructionNode {}
impl Eq for CallFuncNode {}

impl PartialEq<Self> for InstructionNode {
    fn eq(&self, other: &Self) -> bool {
        self.ctx.source.uuid == other.ctx.source.uuid &&
            self.ctx.id.0 == other.ctx.id.0 &&
            self.inst.0 == other.inst.0
    }
}

impl PartialEq<Self> for CallFuncNode {
    fn eq(&self, other: &Self) -> bool {
        self.ctx.source.uuid == other.ctx.source.uuid &&
            self.ctx.id.0 == other.ctx.id.0
    }
}

impl Node for WebAsmNode {
    fn get(&self) -> NodeKind<Self> {
        match self {
            WebAsmNode::CallFunc(ctx) => { ctx.get_kind() }
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

#[test]
fn test_node_pretty_print() {
    let name = String::from("data/fib.wasm");
    let mut file = File::open(&name).unwrap();
    let module = Module::read(&mut file).unwrap();
    hydrate_module(&module, &mut file);
    let source = Rc::new(Source { uuid: 0, name: name.clone(), file: RefCell::new(file), module });
    let fib = WebAsmNode::exported_func(source.clone(), "fib");
    pretty_print(fib);
}
