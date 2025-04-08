use crate::parser::Op;
use crate::symbol_map::SymbolMap;
use crate::cfg::{CFG, BasicBlock};
use crate::tac::{Key, VarID, Var, Tac, TacConst};

struct CFGStringifier<'a> {
    cfg: &'a CFG,
    syms: &'a mut SymbolMap,
    result: String
}

pub fn cfg_to_string(cfg: &CFG, syms: &mut SymbolMap) -> String {
    let stringifier = CFGStringifier {
        cfg,
        syms,
        result: String::new()
    };

    stringifier.stringify()
}

impl<'a> CFGStringifier<'a> {
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
                Tac::Jnt { label, src } => {
                    self.result.push_str("jnt ");
                    self.push_var(src);
                    // get the successor block
                    // self.result.push_str(format!(" block{}", block_id));
                    // does the block we are jumping to have phi nodes?
                    // if yes what versions are we passing for those nodes?
                    // this would require some calculating...
                }
                Tac::Jit { label, src } => {
                    self.result.push_str("jit ");
                    self.push_var(src);
                    // self.result.push_str(format!(" block{}", block_id));
                    // does the block we are jumping to have phi nodes?
                    // if yes what versions are we passing for those nodes?
                    // this would require some calculating...
                }
                Tac::Return { src } => {
                    self.result.push_str("return ");
                    self.push_var(src);
                }
                _ => {}
            }
            self.result.push_str("\n");
        }
        self.result.push_str("\n");
    }

    fn push_op(&mut self, op: &Op) {
        let s = match op {
            Op::Lt => format!("<"),
            Op::Equal => format!("=="),
            Op::And => format!("&&"),
            Op::Or => format!("||"),
            Op::Modulo => format!("%"),
            Op::Plus => format!("+"),
            _ => todo!()
        };

        self.result.push_str(&format!(" {} ", s));
    }

    fn push_const(&mut self, val: &TacConst) {
        let s = match val {
            TacConst::Int(i) => format!("{i}"),
            TacConst::String(s) => format!("{:?}", s),
            _ => todo!(),
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
            _ => "TODO".to_string(),
        }
    }

    fn push_first_line(&mut self) {
        let s = format!("fn ({}) {{\n", "n");
        
        self.result.push_str(&s);
    }

    fn push_last_line(&mut self) {
        self.result.push_str("}");
    }
}

fn comma_separated_args(cfg: &CFG, syms: &mut SymbolMap) -> String {
    "n".to_string()
}

/*
fn (n) {
    t1 = new_list
    result_0 = t1
    t2 = 0
    prev_0 = t2
    t3 = 1
    cur_0 = t3

block1(cur_1, prev_1):
    t4 = result_0.length
    t5 = t4 < n_0
    jnt t5 block5

block2:
    t6 = result_0.length
    t7 = 0
    t8 = t6 == t7
    jnt t8 block4

block3:
    t9 = 0
    t10 = result_0.push
    load_arg t9
    t11 = call t10
    jump block1(cur_1, prev_1)

block4:
    t12 = prev_1 + cur_1
    next_0 = t12
    t13 = result_0.push
    load_arg next_0
    t14 = call t13
    prev_2 = cur_1
    cur_2 = next_0
    jump block1(cur_2, prev_2)

block5:
    return result_0
}
*/
