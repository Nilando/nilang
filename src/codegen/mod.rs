mod interference_graph;
mod spilling;

use std::collections::HashMap;

pub use interference_graph::{InterferenceGraph, find_copy_edges};

use crate::ir::{Block, Func as IRFunc, LabelID, PhiArg, Tac, TacConst};
use crate::parser::Op;

use self::interference_graph::Reg;
use self::spilling::{find_regs_to_spill, spill_reg};


#[derive(PartialEq)]
enum Local {
    FuncId(u64),
    Int(i64),
    Float(f64),
    Sym(u64),
    String(String)
}

pub struct Func {
    id: u64,
    locals: Vec<Local>,
    instrs: Vec<ByteCode>
}

impl Func {
    pub fn new(id: u64) -> Self {
        Self { 
            id, 
            locals: vec![],
            instrs: vec![]
        }
    }

    fn len(&self) -> usize {
        self.instrs.len()
    }

    fn get_local(&self, local: &Local) -> Option<u16> {
        self.locals.iter().position(|l| l == local).map(|id|  {
            u16::try_from(id).unwrap()
        })
    }

    fn push_local(&mut self, local: Local) -> u16 {
        let id = self.locals.len();
        self.locals.push(local);
        u16::try_from(id).unwrap()
    }
}

pub fn generate_func(mut ir_func: IRFunc) -> Func {
    let mut spill_counter: u16 = 0;
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
            spill_reg(&mut ir_func, *reg, spill_counter);
            spill_counter += 1
        }
    }

    graph.color(&ir_func);

    let copies = find_copy_edges(&ir_func);
    
    graph.best_effort_coalescence(copies);

    // println!("{:#?}", graph);

    generate_bytecode(&ir_func, &graph)
}


// TODO:
//  generate reloads and spills during ssa elimination
//  - this requires keeping track of all the spilled vars
//  - the spilled vars may need to be passed to the interference graph 
//  or to the liveness analysis
//  - so we know that

fn generate_bytecode(ir_func: &IRFunc, graph: &InterferenceGraph) -> Func {
    let mut func = Func::new(ir_func.get_id());
    let mut jump_positions: HashMap<BackPatchLabel, Vec<usize>> = HashMap::new();
    let mut label_positions: HashMap<BackPatchLabel, usize> = HashMap::new();
    let mut temp_label_counter = 0;

    for block in ir_func.get_blocks() {
        for instr in block.get_instrs() {
            let bytecode = 
            match instr {
                Tac::PrepCallSite { src } => {
                    todo!()
                }
                Tac::ReloadVar { dest, slot } => {
                    ByteCode::Reload {
                        dest: graph.get_reg(dest),
                        slot: *slot
                    }
                }
                Tac::SpillVar { src, slot } => {
                    ByteCode::Spill { 
                        src: graph.get_reg(src),
                        slot: *slot
                    }
                },
                Tac::Noop => ByteCode::Noop,
                Tac::Label { label } => {
                    label_positions.insert(BackPatchLabel::Label(*label), func.len());
                    continue;
                }
                Tac::Jnt { src, label } |
                Tac::Jit { src, label } => {
                    let original_label = BackPatchLabel::Label(*label);
                    let instr =
                    if let Tac::Jnt { .. } = instr {
                        ByteCode::Jnt { 
                            src: graph.get_reg(src),
                            offset: 0
                        }
                    } else {
                        ByteCode::Jit { 
                            src: graph.get_reg(src),
                            offset: 0
                        }
                    };

                    let jump_block_id = ir_func.get_block_from_label(*label);
                    let jump_block = ir_func.get_block(jump_block_id);
                    let jump_label = 
                    if !jump_block.get_phi_nodes().is_empty() {
                        let bpl = BackPatchLabel::Temp(temp_label_counter);
                        temp_label_counter += 1;
                        bpl
                    } else {
                        original_label
                    };

                    push_generic_jump_instr(instr, &mut func.instrs, &mut jump_positions, jump_label);

                    let next_block = ir_func.get_block(block.get_id() + 1);
                    ssa_elimination(&mut func.instrs, next_block, block, graph);
                    let fall_through_label = BackPatchLabel::Temp(temp_label_counter);
                    temp_label_counter += 1;

                    if !jump_block.get_phi_nodes().is_empty() {
                        label_positions.insert(jump_label, func.len());

                        push_jump_instr(&mut func.instrs, &mut jump_positions, fall_through_label);

                        ssa_elimination(&mut func.instrs, jump_block, block, graph);

                        push_jump_instr(&mut func.instrs, &mut jump_positions, original_label);

                        label_positions.insert(fall_through_label, func.len());
                    }

                    continue;
                }
                Tac::Jump { label } => {
                    let next_block_id = ir_func.get_block_from_label(*label);
                    let next_block = ir_func.get_block(next_block_id);
                    let original_label = BackPatchLabel::Label(*label);

                    ssa_elimination(&mut func.instrs, next_block, block, graph);

                    push_jump_instr(&mut func.instrs, &mut jump_positions, original_label);
                    continue;
                }
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
                    ByteCode::LoadUpvalue { 
                        dest: graph.get_reg(dest), 
                        id: *id 
                    }
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
                        Op::Modulo => {
                            ByteCode::Modulo { 
                                dest,
                                lhs,
                                rhs
                            }
                        }
                        _ => ByteCode::Noop
                    }
                }
                Tac::LoadConst { dest, src } => {
                    match src {
                        TacConst::Func(id) => {
                            let local = Local::FuncId(*id);
                            let id =
                            if let Some(local_id) = func.get_local(&local) {
                                local_id
                            } else {
                                func.push_local(local)
                            };

                            ByteCode::LoadLocal {
                                dest: graph.get_reg(dest), 
                                id
                            }
                        }
                        TacConst::Null => ByteCode::LoadNull { 
                            dest: graph.get_reg(dest), 
                        },
                        TacConst::Bool(b) => ByteCode::LoadBool { 
                            dest: graph.get_reg(dest), 
                            val: *b
                        },
                        TacConst::String(s) => { 
                            let local = Local::String(s.clone());
                            let id =
                            if let Some(local_id) = func.get_local(&local) {
                                local_id
                            } else {
                                func.push_local(local)
                            };

                            ByteCode::LoadLocal {
                                dest: graph.get_reg(dest), 
                                id
                            }
                        }
                        TacConst::Float(f) => { 
                            let local = Local::Float(*f);
                            let id =
                            if let Some(local_id) = func.get_local(&local) {
                                local_id
                            } else {
                                func.push_local(local)
                            };

                            ByteCode::LoadLocal {
                                dest: graph.get_reg(dest), 
                                id
                            }
                        }
                        TacConst::Int(i) => { 
                            match i16::try_from(*i) {
                                Ok(immediate) => {
                                    ByteCode::LoadInt { 
                                        dest: graph.get_reg(dest), 
                                        val: immediate
                                    }
                                }
                                _ => {
                                    let local = Local::Int(*i);
                                    let id =
                                    if let Some(local_id) = func.get_local(&local) {
                                        local_id
                                    } else {
                                        func.push_local(local)
                                    };

                                    ByteCode::LoadLocal {
                                        dest: graph.get_reg(dest), 
                                        id
                                    }
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
                                    let local = Local::Sym(*i);
                                    let id =
                                    if let Some(local_id) = func.get_local(&local) {
                                        local_id
                                    } else {
                                        func.push_local(local)
                                    };

                                    ByteCode::LoadLocal {
                                        dest: graph.get_reg(dest), 
                                        id
                                    }
                                }
                            }
                        },
                    }
                }

            };

            func.instrs.push(bytecode)
        }

        if let Some(id) = block.falls_through() {
            let next_block = ir_func.get_block(id);
            ssa_elimination(&mut func.instrs, next_block, block, graph);
        } 
    }

    back_patch_jump_instructions(&mut func.instrs, label_positions, jump_positions);

    print_bytecode(&func);

    func
}

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
enum BackPatchLabel {
    Label(LabelID),
    Temp(usize),
}

fn push_jump_instr(instrs: &mut Vec<ByteCode>, jump_positions: &mut HashMap<BackPatchLabel, Vec<usize>>, label: BackPatchLabel) {
    let instr = ByteCode::Jump { offset: 0 };

    push_generic_jump_instr(instr, instrs, jump_positions, label);
}

fn push_generic_jump_instr(instr: ByteCode, instrs: &mut Vec<ByteCode>, jump_positions: &mut HashMap<BackPatchLabel, Vec<usize>>, label: BackPatchLabel) {
    let position = instrs.len();

    instrs.push(instr);

    if let Some(positions) = jump_positions.get_mut(&label) {
        positions.push(position);
    } else {
        jump_positions.insert(label, vec![position]);
    }
}

fn back_patch_jump_instructions(
    instrs: &mut Vec<ByteCode>, 
    label_positions: HashMap<BackPatchLabel, usize>,
    jump_positions: HashMap<BackPatchLabel, Vec<usize>>,
) {
    for (label, positions) in jump_positions.iter() {
        let label_position = label_positions.get(label).unwrap();

        for jump_pos in positions.iter() {
            match &mut instrs[*jump_pos] {
                ByteCode::Jnt { offset, .. } |
                ByteCode::Jit { offset, .. } |
                ByteCode::Jump { offset } => {
                    let abs_diff: usize = label_position.abs_diff(*jump_pos);
                    let signed_offset: isize =
                    if *label_position < *jump_pos {
                        isize::try_from(abs_diff).unwrap() * -1
                    } else {
                        isize::try_from(abs_diff).unwrap()
                    };

                    *offset = i16::try_from(signed_offset).unwrap();
                }
                _ => panic!("CODEGEN ERROR during Back Patching")
            }
        }
    }
}

// if the src and dest are both spilled instead of reload instructions
// we could just ensure that they match to the same memory location
fn ssa_elimination(instrs: &mut Vec<ByteCode>, next_block: &Block, current_block: &Block, graph: &InterferenceGraph) {
    if next_block.get_phi_nodes().is_empty() {
        return;
    }

    let mut copy_pairs = vec![];
    let mut dests = vec![];
    let mut srcs = vec![];
    let mut free_regs = vec![];
    let mut reload_pairs = vec![];

    for node in next_block.get_phi_nodes() {
        let dest = graph.get_reg(&node.dest);
        let phi_arg = node.srcs.get(&current_block.get_id()).unwrap();

        dests.push(dest);

        match phi_arg  {
            PhiArg::Reg(vreg) => {
                let src = graph.get_reg(vreg);

                srcs.push(src);

                if src != dest {
                    copy_pairs.push((src, dest));
                }
            }
            PhiArg::Spill(slot) => {
                reload_pairs.push((*slot, dest))
            }
        } 
    }


    // STEP 1: INSERT COPY INSTRUCTIONS BY MAKING USE OF FREE REGISTERS
    for (_, d) in copy_pairs.iter() {
        if !srcs.contains(d) {
            free_regs.push(*d);
        }
    }

    while let Some(free_reg) = free_regs.pop() {
        let i = copy_pairs.iter().position(|(_, dest)| *dest == free_reg).unwrap();
        let (src, dest) = copy_pairs.remove(i);
        let copy_instr = ByteCode::Copy {
            dest,
            src,
        };

        instrs.push(copy_instr);

        if copy_pairs.iter().find(|(s, _)| *s == src).is_none() {
            if dests.iter().find(|d| src == **d).is_some() {
                free_regs.push(src);
            }
        }
    }

    // STEP 2: LOAD SPILLED ARGS
    // since reload pairs cannot be involved in cycles, we can be sure
    // that the previous step copied any needed values to their needed dests before we clobber them
    //  
    // The fact that reload pairs cannot be involved in cycles s b/c currently 
    // spill slots are statically assigned(not reused), which isn't very efficient.
    for (slot, dest) in reload_pairs.into_iter() {
        let reload_instr = ByteCode::Reload {
            dest,
            slot,
        };

        instrs.push(reload_instr);
    }

    // STEP 3: INSERT SWAP INSTRUCTIONS TO BREAK CYCLES
    while let Some((dest, src)) = copy_pairs.pop() {
        if dest == src {
            continue;
        }

        let swap_instr = ByteCode::Swap {
            r1: dest,
            r2: src,
        };
        instrs.push(swap_instr);

        for (s, _) in copy_pairs.iter_mut() {
            if *s == dest {
                *s = src;
            }
        }
    }
}

// things to implement:
// 4. spill instruction insertion
// 3. callsite prepping

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
        id: u16
    },
    LoadUpvalue {
        dest: Reg,
        id: u16
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
    Modulo {
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
    Spill {
        src: Reg,
        slot: u16
    },
    Reload {
        dest: Reg,
        slot: u16
    },
}

fn print_bytecode(func: &Func) {
    println!("=== FN {} START ===", func.id);
    for instr in func.instrs.iter() {
        match instr {
            ByteCode::Jump { offset } =>               println!("JMP  {offset}"),
            ByteCode::Jnt { src, offset } =>           println!("JNT  {offset}, {src}"),
            ByteCode::Jit { src, offset } =>           println!("JIT  {offset}, {src}"),
            ByteCode::StoreArg { src } =>              println!("ARG  {src }"),
            ByteCode::Call { dest, src } =>            println!("CALL {dest}, {src }"),
            ByteCode::Return { src } =>                println!("RTN  {src }"),
            ByteCode::LoadBool { dest, val } =>        println!("BOOL {dest}, {val}"),
            ByteCode::LoadInt { dest, val } =>         println!("INT  {dest}, {val}"),
            ByteCode::LoadSym { dest, val } =>         println!("SYM  {dest}, #{val}"),
            ByteCode::LoadLocal { dest, id } =>        println!("LOC  {dest}, {id}"),
            ByteCode::LoadNull { dest } =>             println!("LDN  {dest}"),
            ByteCode::LoadUpvalue { dest, id } =>      println!("LDUV {dest}, {id}"),
            ByteCode::StoreUpvalue { func, src } =>    println!("STUV {func}, {src}"),
            ByteCode::Spill { src, slot } =>             println!("SPIL {src}, {slot}"),
            ByteCode::Reload { dest, slot } =>           println!("RELD {dest}, {slot}"),
            ByteCode::Print { src } =>                 println!("OUT  {src }"),
            ByteCode::Read { dest } =>                 println!("READ {dest}"),
            ByteCode::Lt { dest, lhs, rhs } =>         println!("LT   {dest}, {lhs}, {rhs}"),
            ByteCode::Inequality { dest, lhs, rhs } => println!("NEQ  {dest}, {lhs}, {rhs}"),
            ByteCode::Equality { dest, lhs, rhs } =>   println!("EQ   {dest}, {lhs}, {rhs}"),
            ByteCode::Add { dest, lhs, rhs } =>        println!("ADD  {dest}, {lhs}, {rhs}"),
            ByteCode::Sub { dest, lhs, rhs } =>        println!("SUB  {dest}, {lhs}, {rhs}"),
            ByteCode::Modulo { dest, lhs, rhs } =>     println!("MOD  {dest}, {lhs}, {rhs}"),
            ByteCode::Copy { dest, src } =>            println!("COPY {dest}, {src}"),
            ByteCode::Swap { r1, r2 } =>               println!("SWAP {r1  }, {r2 }"),
            ByteCode::NewList { dest } =>              println!("LIST {dest}"),
            ByteCode::NewMap { dest } =>               println!("MAP  {dest}"),
            ByteCode::MemLoad { dest, store, key } =>  println!("MEML {dest}, {store}[{key}]"),
            ByteCode::MemStore { store, key, src } =>  println!("MEMS {store}[{key}], {src}"),
            ByteCode::LoadGlobal { dest, sym } =>      println!("LDGB {dest}, #{sym}"),
            ByteCode::StoreGlobal { sym, src } =>      println!("STGB {src}, #{sym}"),
            ByteCode::Noop => println!("NOOP"),
        }
    }
    println!("=== END ===");
}
