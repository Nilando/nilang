use crate::parser::Op;
use crate::symbol_map::SymbolMap;
use crate::cfg::{CFG, BasicBlock, BlockID};
use crate::tac::{Key, VarID, Var, Tac, TacConst, MAIN_FUNC_ID};

struct CFGPrinter<'a> {
    cfg: &'a CFG,
    syms: &'a mut SymbolMap,
    result: String
}

pub fn cfg_to_string(cfg: &CFG, syms: &mut SymbolMap) -> String {
    let stringifier = CFGPrinter {
        cfg,
        syms,
        result: String::new()
    };

    stringifier.stringify()
}

impl<'a> CFGPrinter<'a> {
    fn stringify(mut self) -> String {
        self.push_first_line();

        for block in self.cfg.blocks.iter() {
            self.stringify_block(block);
        }

        self.push_last_line();

        self.result
    }

    fn stringify_block(&mut self, block: &BasicBlock) {
        self.push_block_header(block);

        for tac in block.code.iter() {
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
                Tac::KeyLoad { dest, store, key } => {
                    self.push_var(dest);
                    self.result.push_str(" = ");
                    self.push_var(store);
                    self.push_key_access(key);
                }
                Tac::KeyStore { store, key, src } => {
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
                    let succ_id = self.cfg.get_block_from_label(*label);
                    self.result.push_str(&format!("jump block{succ_id}"));

                    self.print_block_args(block.id, succ_id);
                }
                Tac::Jnt { label, src } => {
                    self.result.push_str("jnt ");
                    self.push_var(src);

                    let succ_id = self.cfg.get_block_from_label(*label);
                    self.result.push_str(&format!(" block{succ_id}"));

                    self.print_block_args(block.id, succ_id);
                }
                Tac::Jit { label, src } => {
                    self.result.push_str("jit ");
                    self.push_var(src);

                    let succ_id = self.cfg.get_block_from_label(*label);
                    self.result.push_str(&format!(" block{succ_id}"));

                    self.print_block_args(block.id, succ_id);
                }
                Tac::Return { src } => {
                    self.result.push_str("return ");
                    self.push_var(src);
                }
                Tac::UpvalueStore { store, src } => {
                    self.push_var(store);
                    self.result.push_str(".upvalues << ");
                    self.result.push_str(self.syms.get_str(*src));
                }
                Tac::Label { .. } => {
                    panic!("CFG contained label TAC! Labels should have been removed during CFG building process")
                }
            }
            self.result.push_str("\n");
        }

        if block.continues() {
            if self.cfg.blocks.len() > block.id + 1 {
                self.result.push_str(&format!("  next block{}", block.id + 1));
                self.print_block_args(block.id, block.id + 1);
                self.result.push_str("\n");
            }
        }

        self.result.push_str("\n");
    }

    fn print_block_args(&mut self, caller_id: BlockID, calle_id: BlockID) {
        if !self.cfg[calle_id].phi_nodes.is_empty() {
            self.result.push_str(&format!("("));
            for (i, node) in self.cfg[calle_id].phi_nodes.iter().enumerate() {
                self.push_var(node.srcs.get(&caller_id).unwrap());

                if  i + 1 < self.cfg[calle_id].phi_nodes.len() {
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
        };

        self.result.push_str(&s);
    }

    fn push_key_access(&mut self, key: &Key) {
        let s = match key {
            Key::Sym(id) => format!(".{}", self.syms.get_str(*id)),
            Key::Var(var) => format!("[{}]", self.var_str(var)),
        };

        self.result.push_str(&s);
    }

    fn push_block_header(&mut self, block: &BasicBlock) {
        if block.phi_nodes.is_empty() {
            let block_def = format!("block{}:\n", block.id);
            self.result.push_str(&block_def);
        } else {
            let block_def = format!("block{}(", block.id, );
            self.result.push_str(&block_def);

            for (idx, node) in block.phi_nodes.iter().enumerate() {
                self.push_var(&node.dest);

                if idx + 1 < block.phi_nodes.len() {
                    self.result.push_str(", ");
                }
            }

            self.result.push_str("):\n");
        }
    }

    fn push_var(&mut self, var: &Var) {
        let s = self.var_str(var);

        self.result.push_str(&s)
    }

    fn var_str(&mut self, var: &Var) -> String {
        match var.id {
            VarID::Local(id) => format!("{}_{}", self.syms.get_str(id), var.ver.unwrap()),
            VarID::Temp(id) => format!("t{id}"),
            VarID::LongTemp(id) => format!("l{id}_{}", var.ver.unwrap()),
            VarID::Global(id) => format!("@{}_{}", self.syms.get_str(id), var.ver.unwrap()),
            VarID::Upvalue(id) => format!("^{}", self.syms.get_str(id)),
        }
    }

    fn push_first_line(&mut self) {
        if self.cfg.func_id == MAIN_FUNC_ID {
            self.result.push_str("MAIN (");
        } else {
            self.result.push_str(&format!("fn{} (", self.cfg.func_id));
        }

        for (idx, sym) in self.cfg.entry_arguments.iter().enumerate() {
            self.result.push_str(&format!("{}", self.syms.get_str(*sym)));

            if idx + 1 < self.cfg.entry_arguments.len() {
                self.result.push_str(", ");
            }
        }

        self.result.push_str(&format!(") {{\n"));
    }

    fn push_last_line(&mut self) {
        self.result.push_str("}");
    }
}
