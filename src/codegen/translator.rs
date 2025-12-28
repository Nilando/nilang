use crate::codegen::func::{Func, Local};
use crate::codegen::InterferenceGraph;
use crate::ir::{Tac, TacConst};
use crate::operators::{BinaryOp, UnaryOp};
use crate::runtime::ByteCode;

pub fn translate_tac(tac: &Tac, graph: &InterferenceGraph, func: &mut Func) -> Option<ByteCode> {
    match tac {
        Tac::Noop => Some(ByteCode::Noop),
        Tac::Copy { dest, src } => Some(ByteCode::Copy {
            dest: graph.get_reg(dest),
            src: graph.get_reg(src),
        }),
        Tac::Read { dest } => Some(ByteCode::Read {
            dest: graph.get_reg(dest),
        }),
        Tac::Print { src } => Some(ByteCode::Print {
            src: graph.get_reg(src),
        }),
        Tac::Call { dest, src } => Some(ByteCode::Call {
            dest: graph.get_reg(dest),
            src: graph.get_reg(src),
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
            id: *id,
        }),
        Tac::StoreUpvalue { func, src } => Some(ByteCode::StoreUpvalue {
            func: graph.get_reg(func),
            src: graph.get_reg(src),
        }),
        Tac::MemLoad { dest, store, key } => Some(ByteCode::MemLoad {
            dest: graph.get_reg(dest),
            store: graph.get_reg(store),
            key: graph.get_reg(key),
        }),
        Tac::MemStore { store, key, src } => Some(ByteCode::MemStore {
            store: graph.get_reg(store),
            key: graph.get_reg(key),
            src: graph.get_reg(src),
        }),
        Tac::Import { dest, path } => Some(ByteCode::Import {
            dest: graph.get_reg(dest),
            path: graph.get_reg(path),
        }),
        Tac::Push { store, src } => Some(ByteCode::Push {
            store: graph.get_reg(store),
            src: graph.get_reg(src),
        }),
        Tac::Binop { dest, op, lhs, rhs } => Some(translate_binop(*dest, *op, *lhs, *rhs, graph)),
        Tac::Unaop { dest, op, src } => Some(translate_unaop(*dest, *op, *src, graph)),
        Tac::Pop { dest, src } => {
            let dest = graph.get_reg(dest);
            let src = graph.get_reg(src);

            Some(ByteCode::Pop { dest, src })
        }
        Tac::LoadConst { dest, src } => Some(translate_load_const(*dest, src, graph, func)),
        Tac::Type { dest, src } => {
            let dest = graph.get_reg(dest);
            let src = graph.get_reg(src);

            Some(ByteCode::Type { dest, src })
        }
        Tac::Clone { dest, src } => {
            let dest = graph.get_reg(dest);
            let src = graph.get_reg(src);

            Some(ByteCode::Clone { dest, src })
        }
        Tac::Bind { dest, func, arg } => {
            let dest = graph.get_reg(dest);
            let func = graph.get_reg(func);
            let arg = graph.get_reg(arg);

            Some(ByteCode::Bind { dest, func, arg })
        }
        Tac::Delete { dest, store, key } => {
            let dest = graph.get_reg(dest);
            let store = graph.get_reg(store);
            let key = graph.get_reg(key);

            Some(ByteCode::Delete { dest, store, key })
        }
        // These are handled specially in the main loop
        Tac::Label { .. } | Tac::Jnt { .. } | Tac::Jit { .. } | Tac::Jump { .. } => None,
    }
}

fn translate_unaop(
    dest: crate::ir::VReg,
    op: UnaryOp,
    src: crate::ir::VReg,
    graph: &InterferenceGraph,
) -> ByteCode {
    let dest = graph.get_reg(&dest);
    let src = graph.get_reg(&src);

    match op {
        UnaryOp::Len => {
            ByteCode::Len { dest, src }
        }
        UnaryOp::BitFlip => {
            ByteCode::BitFlip { dest, src }
        }
        _ => panic!()
    }
}

fn translate_binop(
    dest: crate::ir::VReg,
    op: BinaryOp,
    lhs: crate::ir::VReg,
    rhs: crate::ir::VReg,
    graph: &InterferenceGraph,
) -> ByteCode {
    let dest = graph.get_reg(&dest);
    let lhs = graph.get_reg(&lhs);
    let rhs = graph.get_reg(&rhs);

    match op {
        BinaryOp::Equal => ByteCode::Equality { dest, lhs, rhs },
        BinaryOp::NotEqual => ByteCode::Inequality { dest, lhs, rhs },
        BinaryOp::Lt => ByteCode::Lt { dest, lhs, rhs },
        BinaryOp::Gt => ByteCode::Gt { dest, lhs, rhs },
        BinaryOp::Plus => ByteCode::Add { dest, lhs, rhs },
        BinaryOp::Minus => ByteCode::Sub { dest, lhs, rhs },
        BinaryOp::Modulo => ByteCode::Modulo { dest, lhs, rhs },
        BinaryOp::Multiply => ByteCode::Mult { dest, lhs, rhs },
        BinaryOp::Divide => ByteCode::Div { dest, lhs, rhs },
        BinaryOp::Lte => ByteCode::Lte { dest, lhs, rhs },
        BinaryOp::Gte => ByteCode::Gte { dest, lhs, rhs },
        BinaryOp::BitOr => ByteCode::BitOr { dest, lhs, rhs },
        BinaryOp::BitXor => ByteCode::BitXor { dest, lhs, rhs },
        BinaryOp::BitShift => ByteCode::BitShift { dest, lhs, rhs },
        BinaryOp::BitAnd => ByteCode::BitAnd { dest, lhs, rhs },
        _ => panic!(),
    }
}

fn translate_load_const(
    dest: crate::ir::VReg,
    src: &TacConst,
    graph: &InterferenceGraph,
    func: &mut Func,
) -> ByteCode {
    let dest_reg = graph.get_reg(&dest);

    match src {
        TacConst::Func(id) => {
            let local = Local::FuncId(*id);
            let id = get_or_create_local(local, func);
            ByteCode::LoadLocal { dest: dest_reg, id }
        }
        TacConst::Null => ByteCode::LoadNull { dest: dest_reg },
        TacConst::Bool(b) => ByteCode::LoadBool {
            dest: dest_reg,
            val: *b,
        },
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
        TacConst::Int(i) => match i16::try_from(*i) {
            Ok(immediate) => ByteCode::LoadInt {
                dest: dest_reg,
                val: immediate,
            },
            _ => match i64::try_from(*i) {
                Ok(local) => {
                    let local = Local::Int(local);
                    let id = get_or_create_local(local, func);
                    ByteCode::LoadLocal { dest: dest_reg, id }
                }
                _ => {
                    let local = Local::Float(*i as f64);
                    let id = get_or_create_local(local, func);
                    ByteCode::LoadLocal { dest: dest_reg, id }
                }
            },
        },
        TacConst::Sym(i) => match u16::try_from(*i) {
            Ok(immediate) => ByteCode::LoadSym {
                dest: dest_reg,
                val: immediate,
            },
            _ => {
                let local = Local::Sym(*i);
                let id = get_or_create_local(local, func);
                ByteCode::LoadLocal { dest: dest_reg, id }
            }
        },
    }
}

fn get_or_create_local(local: Local, func: &mut Func) -> u16 {
    if let Some(local_id) = func.get_local(&local) {
        local_id
    } else {
        func.push_local(local)
    }
}
