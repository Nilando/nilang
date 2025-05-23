mod interference_graph;

#[cfg(test)]
mod tests;

use std::collections::HashMap;

pub use interference_graph::{InterferenceGraph, find_copy_edges};

use crate::ir::{Block, Func as IRFunc, LabelID, Tac, TacConst};
use crate::parser::{Op, PackedSpans};

use self::interference_graph::Reg;


#[derive(Debug, PartialEq)]
enum Local {
    FuncId(u64),
    Int(i64),
    Float(f64),
    Sym(u64),
    String(String)
}

#[derive(Debug)]
pub struct Func {
    id: u64,
    max_clique: usize,
    locals: Vec<Local>,
    instrs: Vec<ByteCode>,
    spans: PackedSpans
}

impl Func {
    pub fn new(id: u64, max_clique: usize) -> Self {
        Self { 
            id, 
            max_clique,
            locals: vec![],
            instrs: vec![],
            spans: PackedSpans::new()
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

pub fn generate_func(ir_func: IRFunc) -> Func {
    let mut graph = InterferenceGraph::build(&ir_func);
    let (max_clique, seo) = graph.find_max_clique(); // simplical elimination ordering aka SEO

    if max_clique > 256 {
        // just fail saying function requires too many reigsters
        panic!("function requires too many registers");
    }

    graph.color(&ir_func, seo, max_clique);

    let copies = find_copy_edges(&ir_func);
    graph.best_effort_coalescence(&ir_func, copies);

    let func = generate_bytecode(&ir_func, &graph, max_clique);

    func
}

fn generate_bytecode(ir_func: &IRFunc, graph: &InterferenceGraph, max_clique: usize) -> Func {
    let mut func = Func::new(ir_func.get_id(), max_clique);
    let mut jump_positions: HashMap<BackPatchLabel, Vec<usize>> = HashMap::new();
    let mut label_positions: HashMap<BackPatchLabel, usize> = HashMap::new();
    let mut temp_label_counter = 0;

    for block in ir_func.get_blocks() {
        let spans = block.get_spans();
        for (idx, instr) in block.get_instrs().iter().enumerate() {
            let span = spans.get(idx);
            let bytecode = 
            match instr {
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
                        Op::Gt => {
                            ByteCode::Gt { 
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
                        Op::Multiply => {
                            ByteCode::Mult { 
                                dest,
                                lhs,
                                rhs
                            }
                        }
                        Op::Divide => {
                            ByteCode::Div { 
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

            let i = func.instrs.len();
            func.instrs.push(bytecode);

            if let Some(s) = span {
                func.spans.push(*s, i);
            }
        }

        if let Some(id) = block.falls_through() {
            let next_block = ir_func.get_block(id);
            ssa_elimination(&mut func.instrs, next_block, block, graph);
        } 
    }

    back_patch_jump_instructions(&mut func.instrs, label_positions, jump_positions);

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

fn ssa_elimination(instrs: &mut Vec<ByteCode>, next_block: &Block, current_block: &Block, graph: &InterferenceGraph) {
    if next_block.get_phi_nodes().is_empty() {
        return;
    }

    let mut copy_pairs = vec![];
    let mut srcs = vec![];
    let mut free_regs = vec![];

    for node in next_block.get_phi_nodes() {
        let dest = graph.get_reg(&node.dest);
        let phi_arg = node.srcs.get(&current_block.get_id()).unwrap();
        let src = graph.get_reg(phi_arg);

        srcs.push(src);

        if src != dest {
            copy_pairs.push((src, dest));
        }
    }

    // STEP 1: INSERT COPY INSTRUCTIONS BY MAKING USE OF FREE REGISTERS
    // this should take care of everything that except values stuck in a cycles
    // and values that are spilled
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
            if copy_pairs.iter().find(|(_, d)| src == *d).is_some() {
                free_regs.push(src);
            }
        }
    }

    // STEP 2: INSERT SWAP INSTRUCTIONS (break cycles)
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
    Gt {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Lt {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Div {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Mult {
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
}

pub fn func_to_string(func: &Func) -> String {
    let mut result = String::new();

    result.push_str(&format!("=== FN {} START ===\n", func.id));
    for instr in func.instrs.iter() {
        let s =
        match instr {
            ByteCode::Jump { offset } =>               format!("JMP  {offset}"),
            ByteCode::Jnt { src, offset } =>           format!("JNT  {offset}, {src}"),
            ByteCode::Jit { src, offset } =>           format!("JIT  {offset}, {src}"),
            ByteCode::StoreArg { src } =>              format!("ARG  {src }"),
            ByteCode::Call { dest, src } =>            format!("CALL {dest}, {src }"),
            ByteCode::Return { src } =>                format!("RTN  {src }"),
            ByteCode::LoadBool { dest, val } =>        format!("BOOL {dest}, {val}"),
            ByteCode::LoadInt { dest, val } =>         format!("INT  {dest}, {val}"),
            ByteCode::LoadSym { dest, val } =>         format!("SYM  {dest}, #{val}"),
            ByteCode::LoadLocal { dest, id } =>        format!("LOC  {dest}, {id}"),
            ByteCode::LoadNull { dest } =>             format!("LDN  {dest}"),
            ByteCode::LoadUpvalue { dest, id } =>      format!("LDUV {dest}, {id}"),
            ByteCode::StoreUpvalue { func, src } =>    format!("STUV {func}, {src}"),
            ByteCode::Print { src } =>                 format!("OUT  {src }"),
            ByteCode::Read { dest } =>                 format!("READ {dest}"),
            ByteCode::Lt { dest, lhs, rhs } =>         format!("LT   {dest}, {lhs}, {rhs}"),
            ByteCode::Gt { dest, lhs, rhs } =>         format!("GT   {dest}, {lhs}, {rhs}"),
            ByteCode::Inequality { dest, lhs, rhs } => format!("NEQ  {dest}, {lhs}, {rhs}"),
            ByteCode::Equality { dest, lhs, rhs } =>   format!("EQ   {dest}, {lhs}, {rhs}"),
            ByteCode::Mult { dest, lhs, rhs } =>       format!("MULT {dest}, {lhs}, {rhs}"),
            ByteCode::Div { dest, lhs, rhs } =>        format!("DIV  {dest}, {lhs}, {rhs}"),
            ByteCode::Add { dest, lhs, rhs } =>        format!("ADD  {dest}, {lhs}, {rhs}"),
            ByteCode::Sub { dest, lhs, rhs } =>        format!("SUB  {dest}, {lhs}, {rhs}"),
            ByteCode::Modulo { dest, lhs, rhs } =>     format!("MOD  {dest}, {lhs}, {rhs}"),
            ByteCode::Copy { dest, src } =>            format!("COPY {dest}, {src}"),
            ByteCode::Swap { r1, r2 } =>               format!("SWAP {r1  }, {r2 }"),
            ByteCode::NewList { dest } =>              format!("LIST {dest}"),
            ByteCode::NewMap { dest } =>               format!("MAP  {dest}"),
            ByteCode::MemLoad { dest, store, key } =>  format!("MEML {dest}, {store}[{key}]"),
            ByteCode::MemStore { store, key, src } =>  format!("MEMS {store}[{key}], {src}"),
            ByteCode::LoadGlobal { dest, sym } =>      format!("LDGB {dest}, #{sym}"),
            ByteCode::StoreGlobal { sym, src } =>      format!("STGB {src}, #{sym}"),
            ByteCode::Noop => format!("NOOP"),
        };

        result.push_str(&s);
        result.push('\n');
    }

    result.push_str(&format!("=== END ===\n"));

    result
}
