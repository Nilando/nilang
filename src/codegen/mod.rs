mod interference_graph;
mod spilling;

pub use interference_graph::{InterferenceGraph, find_copy_edges};

use crate::ir::{Tac, TacConst, Func as IRFunc};
use crate::parser::Op;

use self::interference_graph::Reg;
use self::spilling::{find_regs_to_spill, spill_reg};

pub struct Func {
    // locals: Vec<Local>,
    instrs: Vec<ByteCode>
}

pub fn generate_func(mut ir_func: IRFunc) -> Func {
    let mut graph;

    loop {
        graph = InterferenceGraph::build(&ir_func);

        let max_clique = graph.find_max_clique();
        println!("MAX CLIQUE: {}", max_clique.len());
        if max_clique.len() <= 256 {
            break;
        }

        let regs = find_regs_to_spill(&ir_func, max_clique, &graph);
        for reg in regs.iter() {
            spill_reg(&mut ir_func, *reg);
        }
    }

    graph.color(&ir_func);

    let copies = find_copy_edges(&ir_func);
    
    graph.best_effort_coalescence(copies);

    //println!("{:#?}", graph);

    generate_bytecode(&ir_func, &graph)
}

fn generate_bytecode(ir_func: &IRFunc, graph: &InterferenceGraph) -> Func {
    let mut instrs = vec![];

    for block in ir_func.get_blocks() {
        for instr in block.get_instrs() {
            let bytecode = 
            match instr {
                Tac::Copy { dest, src } => {
                    ByteCode::Copy { 
                        dest: graph.get_reg(dest), 
                        src: graph.get_reg(src) 
                    }
                }
                Tac::Read { dest } => {
                    ByteCode::Read { 
                        dest: graph.get_reg(dest), 
                    }
                }
                Tac::Print { src } => {
                    ByteCode::Print { 
                        src: graph.get_reg(src), 
                    }
                }
                Tac::Call { dest, src } => {
                    ByteCode::Call { 
                        dest: graph.get_reg(dest), 
                        src: graph.get_reg(src) 
                    }
                }
                Tac::StoreArg { src } => {
                    ByteCode::StoreArg { 
                        src: graph.get_reg(src), 
                    }
                }
                Tac::Return { src } => {
                    ByteCode::Return { 
                        src: graph.get_reg(src), 
                    }
                }
                Tac::NewList { dest } => {
                    ByteCode::NewList {
                        dest: graph.get_reg(dest), 
                    }
                }
                Tac::NewMap { dest } => {
                    ByteCode::NewMap {
                        dest: graph.get_reg(dest), 
                    }
                }
                Tac::LoadGlobal { dest, sym } => {
                    ByteCode::LoadGlobal { 
                        dest: graph.get_reg(dest), 
                        sym: graph.get_reg(sym), 
                    }
                }
                Tac::StoreGlobal { src, sym } => {
                    ByteCode::StoreGlobal {
                        src: graph.get_reg(src), 
                        sym: graph.get_reg(sym), 
                    }
                }
                Tac::LoadUpvalue { dest, id } => {
                    // need a way to convert sym to id
                    todo!()
                }
                Tac::StoreUpvalue { func, src } => {
                    ByteCode::StoreUpvalue {
                        func: graph.get_reg(func), 
                        src: graph.get_reg(src), 
                    }
                }
                Tac::MemLoad { dest, store, key } => {
                    ByteCode::MemLoad { 
                        dest: graph.get_reg(dest), 
                        store: graph.get_reg(store),
                        key: graph.get_reg(key) 
                    }
                }
                Tac::MemStore { store, key, src } => {
                    ByteCode::MemStore { 
                        store: graph.get_reg(store),
                        key: graph.get_reg(key),
                        src: graph.get_reg(src), 
                    }
                }
                Tac::Binop { dest, op, lhs, rhs } => {
                    let dest = graph.get_reg(dest);
                    let lhs = graph.get_reg(lhs);
                    let rhs = graph.get_reg(rhs);
                    match op {
                        Op::Equal => {
                            ByteCode::Equality { 
                                dest,
                                lhs,
                                rhs
                            }
                        }
                        Op::NotEqual => {
                            ByteCode::Inequality {
                                dest,
                                lhs,
                                rhs
                            }
                        }
                        Op::Lt => {
                            ByteCode::Lt { 
                                dest,
                                lhs,
                                rhs
                            }
                        }
                        Op::Plus => {
                            ByteCode::Add { 
                                dest,
                                lhs,
                                rhs
                            }
                        }
                        Op::Minus => {
                            ByteCode::Sub { 
                                dest,
                                lhs,
                                rhs
                            }
                        }
                        _ => ByteCode::Noop
                    }
                }
                Tac::Label { label } => continue,
                Tac::Jnt { src, label } => {
                    ByteCode::Jnt { 
                        src: graph.get_reg(src),
                        offset: 0
                    }
                }
                Tac::Jit { src, label } => {
                    ByteCode::Jit { 
                        src: graph.get_reg(src),
                        offset: 0
                    }
                }
                Tac::Jump { label } => {
                    ByteCode::Jump { 
                        offset: 0
                    }
                }
                Tac::LoadConst { dest, src } => {
                    // we have 2 chance 2 turn this into an "immediate" load
                    // either src is an int that fits into a i16
                    // or src is a symID that fits into a u16
                    match src {
                        TacConst::Null => ByteCode::LoadNull { 
                            dest: graph.get_reg(dest), 
                        },
                        TacConst::Bool(b) => ByteCode::LoadBool { 
                            dest: graph.get_reg(dest), 
                            val: *b
                        },
                        TacConst::Int(i) => { 
                            match i16::try_from(*i) {
                                Ok(immediate) => {
                                    ByteCode::LoadInt { 
                                        dest: graph.get_reg(dest), 
                                        val: immediate
                                    }
                                }
                                _ => {
                                    // check if i exists as a local already
                                    // if so load that local id
                                    // if not insert a new local and load that one
                                    todo!()
                                }
                            }
                        },
                        TacConst::Sym(i) => { 
                            match u16::try_from(*i) {
                                Ok(immediate) => {
                                    ByteCode::LoadSym { 
                                        dest: graph.get_reg(dest), 
                                        val: immediate
                                    }
                                }
                                _ => {
                                    // check if i exists as a local already
                                    // if so load that local id
                                    // if not insert a new local and load that one
                                    todo!()
                                }
                            }
                        },
                        _ => ByteCode::Noop
                    }
                }
                _ => ByteCode::Noop
            };

            instrs.push(bytecode)
        }

        // ==== SSA ELIMINATION ALGORITHM ====
    }
    println!("{:#?}", instrs);

    Func { instrs }
}


// ===== PREPPING THE CALL SITE ====
// idea on how to deal with "prepping the call site"
// when we have a "load arg" instr but the vm will need to know
// where to actually load that arg
// this is what the prep call site op should do
// it needs to be chosen such that it and every higher register is free
// we can find the answer to what the lowest register that satisfies that
// by looking at the interference graph and finding the lowest register of 
// the node that corresponds to the vreg being called
//
// there is a major problem here though
// say the very topmost register is in use...there would be no valid call site
//
// what we can do is insert a copy instruction to copy that register to a lower
// free register and we can be certain that such a register exists since we
// are past the spilling phase
//
// if any arg or the calling function matches the moved register,
// make sure to use the new values location
//
// then we can insert a prep_call_site with the top most register
// then we can load all the arguments to the known callsite
// then we can call the function
//
// at which point the return value will be stored in the callsite
// so if we didn't have to free up the callsite manually here we would 
// just copy the callsite to the dest register
//
// if we did have to do a manual callsite freeing
// we would then insert a swap instruction to swap back the register we moved
// with the call site, and then we would copy the call site to the dest register
//
// what I didn't mention was how to deal with the Tac::PrepCallSite instruction
// when creating the interference graph. When we see this instructino during the
// graph creation process we want to add it into the graph as it represents a register
// that will need to be used, but when greedy coloring the graph we don't actually want to
// assign a register as the register will be selected in a different manner.
//
//

#[derive(Debug)]
enum ByteCode {
    Noop,
    Swap {
        r1: Reg,
        r2: Reg,
    },
    Copy {
        dest: Reg,
        src: Reg
    },
    Print {
        src: Reg,
    },
    Read {
        dest: Reg,
    },
    LoadGlobal {
        dest: Reg,
        sym: Reg,
    },
    StoreGlobal {
        src: Reg,
        sym: Reg,
    },
    MemLoad {
        dest: Reg,
        store: Reg,
        key: Reg,
    },
    MemStore {
        store: Reg,
        key: Reg,
        src: Reg,
    },
    NewList {
        dest: Reg,
    },
    NewMap {
        dest: Reg,
    },
    LoadBool {
        dest: Reg,
        val: bool
    },
    LoadNull {
        dest: Reg,
    },
    LoadInt {
        dest: Reg,
        val: i16
    },
    LoadSym {
        dest: Reg,
        val: u16
    },
    LoadLocal {
        dest: Reg,
        local_id: u16
    },
    LoadUpvalue {
        dest: Reg,
        upval_id: u16
    },
    StoreUpvalue {
        func: Reg,
        src: Reg
    },
    Equality {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Inequality {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Lt {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Add {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Sub {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    StoreArg {
        src: Reg,
    },
    Call {
        dest: Reg,
        src: Reg,
    },
    Return {
        src: Reg,
    },
    Jump {
        offset: i16
    },
    Jnt {
        src: Reg,
        offset: i16
    },
    Jit {
        src: Reg,
        offset: i16
    },
}
