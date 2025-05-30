use crate::ir::{Tac, TacConst};
use crate::parser::Op;
use crate::runtime::vm::{ByteCode, Func, Local};
use crate::codegen::InterferenceGraph;

pub fn translate_tac(tac: &Tac, graph: &InterferenceGraph, func: &mut Func) -> Option<ByteCode> {
    match tac {
        Tac::Noop => Some(ByteCode::Noop),
        Tac::Copy { dest, src } => Some(ByteCode::Copy { 
            dest: graph.get_reg(dest), 
            src: graph.get_reg(src) 
        }),
        Tac::Read { dest } => Some(ByteCode::Read { 
            dest: graph.get_reg(dest), 
        }),
        Tac::Print { src } => Some(ByteCode::Print { 
            src: graph.get_reg(src), 
        }),
        Tac::Call { dest, src } => Some(ByteCode::Call { 
            dest: graph.get_reg(dest), 
            src: graph.get_reg(src) 
        }),
        Tac::StoreArg { src } => Some(ByteCode::StoreArg { 
            src: graph.get_reg(src), 
        }),
        Tac::Return { src } => Some(ByteCode::Return { 
            src: graph.get_reg(src), 
        }),
        Tac::NewList { dest } => Some(ByteCode::NewList {
            dest: graph.get_reg(dest), 
        }),
        Tac::NewMap { dest } => Some(ByteCode::NewMap {
            dest: graph.get_reg(dest), 
        }),
        Tac::LoadGlobal { dest, sym } => Some(ByteCode::LoadGlobal { 
            dest: graph.get_reg(dest), 
            sym: graph.get_reg(sym), 
        }),
        Tac::StoreGlobal { src, sym } => Some(ByteCode::StoreGlobal {
            src: graph.get_reg(src), 
            sym: graph.get_reg(sym), 
        }),
        Tac::LoadUpvalue { dest, id } => Some(ByteCode::LoadUpvalue { 
            dest: graph.get_reg(dest), 
            id: *id 
        }),
        Tac::StoreUpvalue { func, src } => Some(ByteCode::StoreUpvalue {
            func: graph.get_reg(func), 
            src: graph.get_reg(src), 
        }),
        Tac::MemLoad { dest, store, key } => Some(ByteCode::MemLoad { 
            dest: graph.get_reg(dest), 
            store: graph.get_reg(store),
            key: graph.get_reg(key) 
        }),
        Tac::MemStore { store, key, src } => Some(ByteCode::MemStore { 
            store: graph.get_reg(store),
            key: graph.get_reg(key),
            src: graph.get_reg(src), 
        }),
        Tac::Binop { dest, op, lhs, rhs } => {
            Some(translate_binop(*dest, *op, *lhs, *rhs, graph))
        },
        Tac::LoadConst { dest, src } => {
            Some(translate_load_const(*dest, src, graph, func))
        },
        // These are handled specially in the main loop
        Tac::Label { .. } |
        Tac::Jnt { .. } |
        Tac::Jit { .. } |
        Tac::Jump { .. } => None,
    }
}

fn translate_binop(dest: crate::ir::VReg, op: Op, lhs: crate::ir::VReg, rhs: crate::ir::VReg, graph: &InterferenceGraph) -> ByteCode {
    let dest = graph.get_reg(&dest);
    let lhs = graph.get_reg(&lhs);
    let rhs = graph.get_reg(&rhs);

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

fn translate_load_const(dest: crate::ir::VReg, src: &TacConst, graph: &InterferenceGraph, func: &mut Func) -> ByteCode {
    let dest_reg = graph.get_reg(&dest);

    match src {
        TacConst::Func(id) => {
            let local = Local::FuncId(*id);
            let id = get_or_create_local(local, func);
            ByteCode::LoadLocal { dest: dest_reg, id }
        }
        TacConst::Null => ByteCode::LoadNull { dest: dest_reg },
        TacConst::Bool(b) => ByteCode::LoadBool { dest: dest_reg, val: *b },
        TacConst::String(s) => {
            let local = Local::String(s.clone());
            let id = get_or_create_local(local, func);
            ByteCode::LoadLocal { dest: dest_reg, id }
        }
        TacConst::Float(f) => {
            let local = Local::Float(*f);
            let id = get_or_create_local(local, func);
            ByteCode::LoadLocal { dest: dest_reg, id }
        }
        TacConst::Int(i) => {
            match i16::try_from(*i) {
                Ok(immediate) => ByteCode::LoadInt { dest: dest_reg, val: immediate },
                _ => {
                    let local = Local::Int(*i);
                    let id = get_or_create_local(local, func);
                    ByteCode::LoadLocal { dest: dest_reg, id }
                }
            }
        }
        TacConst::Sym(i) => {
            match u16::try_from(*i) {
                Ok(immediate) => ByteCode::LoadSym { dest: dest_reg, val: immediate },
                _ => {
                    let local = Local::Sym(*i);
                    let id = get_or_create_local(local, func);
                    ByteCode::LoadLocal { dest: dest_reg, id }
                }
            }
        }
    }
}

fn get_or_create_local(local: Local, func: &mut Func) -> u16 {
    if let Some(local_id) = func.get_local(&local) {
        local_id
    } else {
        func.push_local(local)
    }
}