use crate::ir::{Tac, TacConst};
use crate::parser::Op;
use crate::runtime::vm::{ByteCode, Func, Local};
use crate::codegen::InterferenceGraph;

pub struct ByteCodeTranslator<'a> {
    graph: &'a InterferenceGraph,
    func: &'a mut Func,
}

impl<'a> ByteCodeTranslator<'a> {
    pub fn new(graph: &'a InterferenceGraph, func: &'a mut Func) -> Self {
        Self { graph, func }
    }

    pub fn translate_tac(&mut self, tac: &Tac) -> Option<ByteCode> {
        match tac {
            Tac::Noop => Some(ByteCode::Noop),
            Tac::Copy { dest, src } => Some(ByteCode::Copy { 
                dest: self.graph.get_reg(dest), 
                src: self.graph.get_reg(src) 
            }),
            Tac::Read { dest } => Some(ByteCode::Read { 
                dest: self.graph.get_reg(dest), 
            }),
            Tac::Print { src } => Some(ByteCode::Print { 
                src: self.graph.get_reg(src), 
            }),
            Tac::Call { dest, src } => Some(ByteCode::Call { 
                dest: self.graph.get_reg(dest), 
                src: self.graph.get_reg(src) 
            }),
            Tac::StoreArg { src } => Some(ByteCode::StoreArg { 
                src: self.graph.get_reg(src), 
            }),
            Tac::Return { src } => Some(ByteCode::Return { 
                src: self.graph.get_reg(src), 
            }),
            Tac::NewList { dest } => Some(ByteCode::NewList {
                dest: self.graph.get_reg(dest), 
            }),
            Tac::NewMap { dest } => Some(ByteCode::NewMap {
                dest: self.graph.get_reg(dest), 
            }),
            Tac::LoadGlobal { dest, sym } => Some(ByteCode::LoadGlobal { 
                dest: self.graph.get_reg(dest), 
                sym: self.graph.get_reg(sym), 
            }),
            Tac::StoreGlobal { src, sym } => Some(ByteCode::StoreGlobal {
                src: self.graph.get_reg(src), 
                sym: self.graph.get_reg(sym), 
            }),
            Tac::LoadUpvalue { dest, id } => Some(ByteCode::LoadUpvalue { 
                dest: self.graph.get_reg(dest), 
                id: *id 
            }),
            Tac::StoreUpvalue { func, src } => Some(ByteCode::StoreUpvalue {
                func: self.graph.get_reg(func), 
                src: self.graph.get_reg(src), 
            }),
            Tac::MemLoad { dest, store, key } => Some(ByteCode::MemLoad { 
                dest: self.graph.get_reg(dest), 
                store: self.graph.get_reg(store),
                key: self.graph.get_reg(key) 
            }),
            Tac::MemStore { store, key, src } => Some(ByteCode::MemStore { 
                store: self.graph.get_reg(store),
                key: self.graph.get_reg(key),
                src: self.graph.get_reg(src), 
            }),
            Tac::Binop { dest, op, lhs, rhs } => {
                Some(self.translate_binop(*dest, *op, *lhs, *rhs))
            },
            Tac::LoadConst { dest, src } => {
                Some(self.translate_load_const(*dest, src))
            },
            // These are handled specially in the main loop
            Tac::Label { .. } |
            Tac::Jnt { .. } |
            Tac::Jit { .. } |
            Tac::Jump { .. } => None,
        }
    }

    fn translate_binop(&self, dest: crate::ir::VReg, op: Op, lhs: crate::ir::VReg, rhs: crate::ir::VReg) -> ByteCode {
        let dest = self.graph.get_reg(&dest);
        let lhs = self.graph.get_reg(&lhs);
        let rhs = self.graph.get_reg(&rhs);

        match op {
            Op::Equal => ByteCode::Equality { dest, lhs, rhs },
            Op::NotEqual => ByteCode::Inequality { dest, lhs, rhs },
            Op::Lt => ByteCode::Lt { dest, lhs, rhs },
            Op::Gt => ByteCode::Gt { dest, lhs, rhs },
            Op::Plus => ByteCode::Add { dest, lhs, rhs },
            Op::Minus => ByteCode::Sub { dest, lhs, rhs },
            Op::Modulo => ByteCode::Modulo { dest, lhs, rhs },
            Op::Multiply => ByteCode::Mult { dest, lhs, rhs },
            Op::Divide => ByteCode::Div { dest, lhs, rhs },
            _ => ByteCode::Noop
        }
    }

    fn translate_load_const(&mut self, dest: crate::ir::VReg, src: &TacConst) -> ByteCode {
        let dest_reg = self.graph.get_reg(&dest);

        match src {
            TacConst::Func(id) => {
                let local = Local::FuncId(*id);
                let id = self.get_or_create_local(local);
                ByteCode::LoadLocal { dest: dest_reg, id }
            }
            TacConst::Null => ByteCode::LoadNull { dest: dest_reg },
            TacConst::Bool(b) => ByteCode::LoadBool { dest: dest_reg, val: *b },
            TacConst::String(s) => {
                let local = Local::String(s.clone());
                let id = self.get_or_create_local(local);
                ByteCode::LoadLocal { dest: dest_reg, id }
            }
            TacConst::Float(f) => {
                let local = Local::Float(*f);
                let id = self.get_or_create_local(local);
                ByteCode::LoadLocal { dest: dest_reg, id }
            }
            TacConst::Int(i) => {
                match i16::try_from(*i) {
                    Ok(immediate) => ByteCode::LoadInt { dest: dest_reg, val: immediate },
                    _ => {
                        let local = Local::Int(*i);
                        let id = self.get_or_create_local(local);
                        ByteCode::LoadLocal { dest: dest_reg, id }
                    }
                }
            }
            TacConst::Sym(i) => {
                match u16::try_from(*i) {
                    Ok(immediate) => ByteCode::LoadSym { dest: dest_reg, val: immediate },
                    _ => {
                        let local = Local::Sym(*i);
                        let id = self.get_or_create_local(local);
                        ByteCode::LoadLocal { dest: dest_reg, id }
                    }
                }
            }
        }
    }

    fn get_or_create_local(&mut self, local: Local) -> u16 {
        if let Some(local_id) = self.func.get_local(&local) {
            local_id
        } else {
            self.func.push_local(local)
        }
    }
}