use crate::parser::Op;
use crate::symbol_map::SymbolMap;
use super::func::Func;
use super::block::{Block, BlockId};
use super::tac::{VReg, Var, Tac, TacConst};
use super::lowering::MAIN_FUNC_ID;
use std::collections::HashMap;

#[derive(Debug)]
pub struct VRegMap {
    vars: HashMap<Var, Vec<VReg>>,
    regs: HashMap<VReg, Var>
}

impl VRegMap {
    pub fn new(var_reg_map: HashMap<Var, VReg>) -> Self {
        let mut vars = HashMap::new();
        let mut regs = HashMap::new();

        for (var, reg) in var_reg_map.iter() {
            vars.insert(*var, vec![*reg]);
            regs.insert(*reg, *var);
        }

        Self {
            vars,
            regs
        }
    }

    pub fn map(&mut self, old: VReg, new: VReg) {
        let var = *self.regs.get(&old).unwrap();

        self.regs.insert(new, var);
        self.vars.get_mut(&var).unwrap().push(new);
    }

    fn reg_to_var(&self, reg: &VReg) -> (Var, Option<usize>) {
        let var = self.regs.get(reg).unwrap();
        let ver = self.vars.get(var).unwrap().iter().position(|r| r == reg);

        (*var, ver)
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
                    self.result.push_str(" = new_list");
                }
                Tac::NewMap { dest } => {
                    self.push_var(dest);
                    self.result.push_str(" = new_map");
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
                Tac::LoadArg { src } => {
                    self.result.push_str("load_arg ");
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
                    self.result.push_str(" = call ");
                    self.push_var(src);
                }
                Tac::Print { src } => {
                    self.result.push_str("print ");
                    self.push_var(src);
                }
                Tac::Read { dest } => {
                    self.push_var(dest);
                    self.result.push_str(" = read");
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
                    self.result.push_str("return ");
                    self.push_var(src);
                }
                Tac::StoreUpvalue { dest, src } => {
                    self.push_var(dest);
                    self.result.push_str(" UPVAL ");
                    self.push_var(src);
                }
                Tac::Label { .. } => {
                    continue;
                }
                Tac::Noop => {
                    self.result.push_str("NOOP");
                }
                Tac::SpillVar { src } => {
                    self.result.push_str("SPILL ");
                    self.push_var(src);
                }
                Tac::ReloadVar { dest, src } => {
                    self.result.push_str("RELOAD ");
                    self.push_var(dest);
                    self.push_var(src);
                }
                Tac::StoreGlobal { src, sym } => {
                    self.result.push_str("STORE_GLB ");
                    self.push_var(sym);
                    self.result.push_str(" , ");
                    self.push_var(src);
                }
                Tac::LoadGlobal { dest, sym } => {
                    self.result.push_str("LOAD_GLB ");
                    self.push_var(dest);
                    self.result.push_str(" , ");
                    self.push_var(sym);
                }
                Tac::LoadUpvalue { dest, id } => {
                    self.result.push_str("LOAD_UPVAL ");
                    self.push_var(dest);
                    self.result.push_str(" , ");
                    self.push_const(&TacConst::Sym(*id));
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
                self.push_var(node.srcs.get(&caller_id).unwrap());

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
            let (var, ver) = vrm.reg_to_var(reg);

            self.var_str(&var, ver)
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
            let (var, ver) = vrm.reg_to_var(reg);

            self.var_str(&var, ver)
        } else {
            self.vreg_str(reg)
        };


        self.result.push_str(&s)
    }

    fn vreg_str(&mut self, var: &VReg) -> String {
        format!("%{}", var)
    }

    fn var_str(&mut self, var: &Var, ver: Option<usize>) -> String {
        match var {
            Var::Local(id) => format!("{}_{}", self.syms.get_str(*id), ver.unwrap()),
            Var::Temp(id) => format!("t{id}"),
            Var::UpVal(id) => format!("^{:?}", id),
        }
    }

    fn push_first_line(&mut self) {
        if self.func.get_id() == MAIN_FUNC_ID {
            self.result.push_str("MAIN (");
        } else {
            self.result.push_str(&format!("fn{} (", self.func.get_id()));
        }

        for (idx, sym) in self.func.get_args().iter().enumerate() {
            self.result.push_str(&format!("{}", self.syms.get_str(*sym)));

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
