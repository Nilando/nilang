use crate::parser::Op;
use crate::symbol_map::{SymbolMap, SymID};
use super::func::Func;
use super::block::{Block, BlockId};
use super::tac::{VReg, Tac, TacConst};
use super::lowering::MAIN_FUNC_ID;
use std::collections::HashMap;

#[derive(Debug)]
pub struct VRegMap {
    vars: HashMap<SymID, Vec<VReg>>,
    regs: HashMap<VReg, SymID>
}

impl VRegMap {
    pub fn new(vreg_map: HashMap<SymID, VReg>) -> Self {
        let mut vars = HashMap::new();
        let mut regs = HashMap::new();

        for (var, reg) in vreg_map.iter() {
            vars.insert(*var, vec![*reg]);
            regs.insert(*reg, *var);
        }

        Self {
            vars,
            regs
        }
    }

    pub fn map(&mut self, old: VReg, new: VReg) {
        if let Some(v) = self.regs.get(&old) {
            let var = *v;
            self.regs.insert(new, var);
            self.vars.get_mut(&var).unwrap().push(new);
        }
    }

    fn reg_to_var(&self, reg: &VReg) -> Option<(SymID, usize)> {
        if let Some(var) = self.regs.get(reg) {
            let ver = self.vars.get(var).unwrap().iter().position(|r| r == reg).unwrap();

            Some((*var, ver))
        } else {
            None
        }
    }
}

struct FuncPrinter<'a> {
    func: &'a Func,
    syms: &'a mut SymbolMap,
    result: String
}

pub fn func_to_string(func: &Func, syms: &mut SymbolMap) -> String {
    let stringifier = FuncPrinter {
        func,
        syms,
        result: String::new()
    };

    stringifier.stringify()
}

impl<'a> FuncPrinter<'a> {
    fn stringify(mut self) -> String {
        self.push_first_line();

        for block in self.func.get_blocks().iter() {
            self.stringify_block(block);
        }

        self.push_last_line();

        self.result
    }

    fn stringify_block(&mut self, block: &Block) {
        self.push_block_header(block);

        for tac in block.get_instrs().iter() {
            if let Tac::Label { .. } = tac {
                continue;
            }

            self.result.push_str("  ");

            match tac {
                Tac::Copy { dest, src } => {
                    self.push_var(dest);
                    self.result.push_str(" = ");
                    self.push_var(src);
                }
                Tac::NewList { dest } => {
                    self.push_var(dest);
                    self.result.push_str(" = NEW_LIST");
                }
                Tac::NewMap { dest } => {
                    self.push_var(dest);
                    self.result.push_str(" = NEW_MAP");
                }
                Tac::MemLoad { dest, store, key } => {
                    self.push_var(dest);
                    self.result.push_str(" = ");
                    self.push_var(store);
                    self.push_key_access(key);
                }
                Tac::MemStore { store, key, src } => {
                    self.push_var(store);
                    self.push_key_access(key);
                    self.result.push_str(" = ");
                    self.push_var(src);
                }
                Tac::LoadConst { dest, src } => {
                    self.push_var(dest);
                    self.result.push_str(" = ");
                    self.push_const(src);
                }
                Tac::StoreArg { src } => {
                    self.result.push_str("STORE_ARG ");
                    self.push_var(src);
                }
                Tac::Binop { dest, op, lhs, rhs } => {
                    self.push_var(dest);
                    self.result.push_str(" = ");
                    self.push_var(lhs);
                    self.push_op(op);
                    self.push_var(rhs);
                }
                Tac::Call { dest, src } => {
                    self.push_var(dest);
                    self.result.push_str(" = CALL ");
                    self.push_var(src);
                }
                Tac::Print { src } => {
                    self.result.push_str("PRINT ");
                    self.push_var(src);
                }
                Tac::Read { dest } => {
                    self.result.push_str("READ ");
                    self.push_var(dest);
                }
                Tac::Jump { label } => {
                    let succ_id = self.func.get_block_from_label(*label);
                    self.result.push_str(&format!("jump block{succ_id}"));

                    self.print_block_args(block.get_id(), succ_id);
                }
                Tac::Jnt { label, src } => {
                    self.result.push_str("jnt ");
                    self.push_var(src);

                    let succ_id = self.func.get_block_from_label(*label);
                    self.result.push_str(&format!(" block{succ_id}"));

                    self.print_block_args(block.get_id(), succ_id);
                }
                Tac::Jit { label, src } => {
                    self.result.push_str("jit ");
                    self.push_var(src);

                    let succ_id = self.func.get_block_from_label(*label);
                    self.result.push_str(&format!(" block{succ_id}"));

                    self.print_block_args(block.get_id(), succ_id);
                }
                Tac::Return { src } => {
                    self.result.push_str("RETURN ");
                    self.push_var(src);
                }
                Tac::Label { .. } => {
                    continue;
                }
                Tac::Noop => {
                    self.result.push_str("NOOP");
                }
                Tac::StoreGlobal { src, sym } => {
                    self.result.push_str("STORE_GLB ");
                    self.push_var(sym);
                    self.result.push_str(", ");
                    self.push_var(src);
                }
                Tac::LoadGlobal { dest, sym } => {
                    self.result.push_str("LOAD_GLB ");
                    self.push_var(dest);
                    self.result.push_str(", ");
                    self.push_var(sym);
                }
                Tac::StoreUpvalue { func, src } => {
                    self.result.push_str("STORE_UPVAL ");
                    self.push_var(func);
                    self.result.push_str(", ");
                    self.push_var(src);
                }
                Tac::LoadUpvalue { dest, .. } => {
                    self.result.push_str("LOAD_UPVAL ");
                    self.push_var(dest);
                }
            }
            self.result.push_str("\n");
        }

        if block.continues() {
            // TODO: this logic is wrong, the next block may not have id + 1 
            let id = block.get_id();
            if self.func.get_blocks().len() > id + 1 {
                self.result.push_str(&format!("  next block{}", id + 1));
                self.print_block_args(id, id + 1);
                self.result.push_str("\n");
            }
        }

        self.result.push_str("\n");
    }

    fn print_block_args(&mut self, caller_id: BlockId, calle_id: BlockId) {
        let phi_nodes = self.func.get_block(calle_id).get_phi_nodes();

        if !phi_nodes.is_empty() {
            self.result.push_str(&format!("("));
            for (i, node) in phi_nodes.iter().enumerate() {
                let vreg = node.srcs.get(&caller_id).unwrap();

                self.push_var(vreg);

                if  i + 1 < phi_nodes.len() {
                    self.result.push_str(", ");
                }
            }
            self.result.push_str(&format!(")"));
        }
    }

    fn push_op(&mut self, op: &Op) {
        let s = match op {
            Op::Lt => format!("<"),
            Op::Lte => format!("<="),
            Op::Gt => format!(">"),
            Op::Gte => format!(">="),
            Op::Multiply => format!("*"),
            Op::Equal => format!("=="),
            Op::NotEqual => format!("!="),
            Op::And => format!("&&"),
            Op::Or => format!("||"),
            Op::Modulo => format!("%"),
            Op::Plus => format!("+"),
            Op::Minus => format!("-"),
            Op::Divide => format!("/"),
        };

        self.result.push_str(&format!(" {} ", s));
    }

    fn push_const(&mut self, val: &TacConst) {
        let s = match val {
            TacConst::Int(i) => format!("{i}"),
            TacConst::String(s) => format!("{:?}", s),
            TacConst::Bool(b) => format!("{}", b),
            TacConst::Null => format!("null"),
            TacConst::Func(func_id) => format!("fn({func_id})"),
            TacConst::Float(f) => format!("{}", f),
            TacConst::Sym(s) => format!("#{}", self.syms.get_str(*s)),
        };

        self.result.push_str(&s);
    }

    fn push_key_access(&mut self, reg: &VReg) {
        let s = 
        if let Some(vrm) = self.func.get_vreg_map() {
            if let Some((sym, ver)) = vrm.reg_to_var(reg) {
                format!("{}_{}", self.syms.get_str(sym), ver)
            } else {
                format!("t{reg}")
            }
        } else {
            self.vreg_str(reg)
        };

        self.result.push_str(&format!("[{}]", s));
    }

    fn push_block_header(&mut self, block: &Block) {
        let phi_nodes = block.get_phi_nodes();

        if phi_nodes.is_empty() {
            let block_def = format!("block{}:\n", block.get_id());
            self.result.push_str(&block_def);
        } else {
            let block_def = format!("block{}(", block.get_id(), );
            self.result.push_str(&block_def);

            for (idx, node) in phi_nodes.iter().enumerate() {
                self.push_var(&node.dest);

                if idx + 1 < phi_nodes.len() {
                    self.result.push_str(", ");
                }
            }

            self.result.push_str("):\n");
        }
    }

    fn push_var(&mut self, reg: &VReg) {
        let s = 
        if let Some(vrm) = self.func.get_vreg_map() {
            if let Some((sym, ver)) = vrm.reg_to_var(reg) {
                format!("{}_{}", self.syms.get_str(sym), ver)
            } else {
                format!("t{reg}")
            }
        } else {
            self.vreg_str(reg)
        };


        self.result.push_str(&s)
    }

    fn vreg_str(&mut self, var: &VReg) -> String {
        format!("%{}", var)
    }

    fn push_first_line(&mut self) {
        if self.func.get_id() == MAIN_FUNC_ID {
            self.result.push_str("MAIN (");
        } else {
            self.result.push_str(&format!("fn{} (", self.func.get_id()));
        }

        for (idx, reg) in self.func.get_args().iter().enumerate() {
            self.push_var(reg);

            if idx + 1 < self.func.get_args().len() {
                self.result.push_str(", ");
            }
        }

        self.result.push_str(&format!(") {{\n"));
    }

    fn push_last_line(&mut self) {
        self.result.push_str("}\n");
    }
}
