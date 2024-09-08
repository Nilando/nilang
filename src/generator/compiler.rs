use super::block::Block;
use super::generator::RawFunc;
use super::ir::{IR, IRConst, Var, VarID, FuncID, LabelID, LocalID};
use std::collections::HashMap;

use crate::lexer::Op;
use crate::bytecode::{ByteCode, Reg};
use crate::parser::Span;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Location {
    Reg(Reg),
    GlobalStore,
}

pub struct FuncCompiler {
    code: Vec<ByteCode>,
    locals: HashMap<LocalID, IRConst>,
    var_data: HashMap<VarID, Vec<Location>>,
    reg_data: [Vec<Var>; 256],
    label_positions: HashMap<LabelID, usize>,
    label_backpatches: HashMap<LabelID, Vec<usize>>
}

impl FuncCompiler {
    pub fn new() -> Self {
        Self {
            code: vec![],
            locals: HashMap::new(),
            var_data: HashMap::new(),
            reg_data: [const { vec![] }; 256],
            label_positions: HashMap::new(),
            label_backpatches: HashMap::new()
        }
    }

    pub fn compile(mut self, id: FuncID, ir_code: Vec<Span<IR>>) -> RawFunc {
        let blocks = self.create_blocks(ir_code);

        for block in blocks.into_iter() {
            for ir in block.as_code().into_iter() {
                match ir.val {
                    IR::Binop { dest, op, lhs, rhs } => {
                        let op_reg1 = self.load_var(lhs);
                        let op_reg2 = self.load_var(rhs);
                        let dest_reg = self.get_dest_reg(dest);

                        self.push_binop(dest_reg, op_reg1, op_reg2, op);
                    }
                    IR::ObjLoad { obj, key, dest } => {
                        let obj = self.load_var(obj);
                        let key = self.load_var(key);
                        let dest = self.get_dest_reg(dest);

                        self.code.push(ByteCode::LoadObj { obj, key, dest });
                    }
                    IR::NewList { dest } => {
                        let dest = self.get_dest_reg(dest);
                        // TODO: dynamically pick cap! or maybe load lists in an entirely different
                        // way
                        self.code.push(ByteCode::LoadList { dest, cap: 8 });
                    }
                    IR::NewMap { dest } => {
                        let dest = self.get_dest_reg(dest);
                        // TODO: dynamically pick cap! or maybe load lists in an entirely different
                        // way
                        self.code.push(ByteCode::LoadMap { dest, cap: 8 });
                    }
                    IR::Log { src } => {
                        let src = self.load_var(src);

                        self.code.push(ByteCode::Log { src });
                    }
                    IR::ObjStore { obj, key, val } => {
                        let obj = self.load_var(obj);
                        let key = self.load_var(key);
                        let val = self.load_var(val);

                        self.code.push(ByteCode::StoreObj { obj, key, val });
                    }
                    IR::Copy { dest, src } => {
                        let src = self.load_var(src);
                        // update reg_data at src to hold dest too
                        // update var_data so that its only location is src
                    }
                    // call jump return Jnt Label
                    _ => todo!() 
                }
            }
        }

        RawFunc::new(id, self.code, self.locals)
    }

    fn push_binop(&mut self, dest: Reg, op1: Reg, op2: Reg, op: Op) {
        let bytecode = 
            match op {
                Op::Plus => ByteCode::Add { dest, op1, op2 },
                Op::Minus => ByteCode::Sub { dest, op1, op2 },
                Op::Divide => ByteCode::Div { dest, op1, op2 },
                Op::Multiply => ByteCode::Mul { dest, op1, op2 },
                Op::Equal => ByteCode::Equal { dest, op1, op2 },
                Op::NotEqual => ByteCode::NotEqual { dest, op1, op2 },
                Op::Or => ByteCode::Or { dest, op1, op2 },
                Op::And => ByteCode::And { dest, op1, op2 },
            };

        self.code.push(bytecode);
    }

    fn load_var(&mut self, var: Var) -> Reg {
        if let Some(locations) = self.var_data.get(&var.id) {
            for loc in locations.iter() {
                if let Location::Reg(reg) = loc {
                    return *reg;
                }
            }
        }

        let reg = self.get_reg();

        match var.id {
            VarID::Global(id) => {
                self.code.push(ByteCode::LoadGlobal { dest: reg, sym: id });
                self.var_data.insert(var.id, vec![Location::GlobalStore, Location::Reg(reg)]);
                self.reg_data[reg as usize] = vec![var];
            }
            VarID::Temp(_) | VarID::Local(_) => {
                self.code.push(ByteCode::LoadNull { dest: reg });
            }
        };

        reg
    }

    // should handle creating a local ID if one is needed?
    fn load_const(&mut self, ir_const: IRConst) -> Reg {
        match ir_const {
            IRConst::Int(i) => {
                // if i fits in a i16 create a load int instr
                // else create a local and then create a load local instr
                let reg = self.get_reg();
                // could we update reg data so that reg holds a temp?
                // then after every instruction wipe the temps?
                //
                //
                // t1 = 1 + 2;
                // a = t1 + b;
                todo!()
            }
            IRConst::Float(i) => {
                // if i fits in a i16 create a load int instr
                // else create a local and then create a load local instr
                todo!()
            }
            IRConst::Bool(src) => {
                let dest = self.get_reg();

                self.code.push( ByteCode::LoadBool { dest, src });

                dest
            }
            IRConst::Func(func_id) => {
                // create a local value with the func id
                // create a local load instr
                todo!()
            }
            IRConst::Null => {
                let dest = self.get_reg();

                self.code.push( ByteCode::LoadNull { dest });

                dest
            }
            IRConst::String(str) => {
                // create a local value with the string
                // create a local load instr
                todo!()
            }
        }
    }

    fn get_reg(&mut self) -> Reg {
        for (reg, reg_vars) in self.reg_data.iter().enumerate() {
            // first look for an empty register
            if reg_vars.is_empty() {
                return reg as u8;
            }

            // look for a register when for every value in the register
            // the address descriptor for that value shows it being somewhere else
            let mut flag = false;

            for var in reg_vars.iter() { 
                flag = false;
                match self.var_data.get(&var.id) {
                    Some(var_locations) => {
                        if var_locations.len() < 2 {
                            break
                        }
                    }
                    None => break,
                }
                flag = true;
            }

            // flag being true means every value in this register is stored at 
            // least in one other location
            if flag {
                return reg as u8;
            }
            
           
            // if every value in the register has no next use we can use this register
            // and is not live
            for var in reg_vars.iter() { 
                if var.next_use.is_some() {

                }
            }
        }

        // TODO: unable to find a register! 
        // means function has too many local variables, or an expression was
        // too long
        todo!()
    }

    fn get_dest_reg(&mut self, val: Var) -> Reg {
        // update reg_data so that val isn't stored in any registers
        todo!()
    }

    fn create_blocks(&self, mut ir_code: Vec<Span<IR>>) -> Vec<Block> {
        let mut blocks = vec![];
        let mut current_block = Block::new(None, true);

        while let Some(mut ir) = ir_code.pop() {
            let i = ir_code.len();

            match ir.val {
                IR::Label { id } => {
                    current_block.set_label(Some(id));
                    blocks.push(current_block);
                    current_block = Block::new(None, true);
                    continue;
                }
                IR::Binop {
                    ref mut dest,
                    lhs: ref mut op1,
                    rhs: ref mut op2,
                    ..
                }
                | IR::ObjLoad {
                    ref mut dest,
                    obj: ref mut op1,
                    key: ref mut op2,
                } => {
                    current_block.update_dest_liveness(dest);
                    current_block.update_operand_liveness(op1, i);
                    current_block.update_operand_liveness(op2, i);
                }
                IR::Copy {
                    ref mut dest,
                    ref mut src,
                } => {
                    current_block.update_dest_liveness(dest);
                    current_block.update_operand_liveness(src, i);
                }
                IR::Log { ref mut src } => {
                    current_block.update_operand_liveness(src, i);
                }
                IR::NewList { ref mut dest } | IR::NewMap { ref mut dest } => {
                    current_block.update_dest_liveness(dest);
                }
                IR::ObjStore {
                    ref mut obj,
                    ref mut key,
                    ref mut val,
                } => {
                    current_block.update_operand_liveness(obj, i);
                    current_block.update_operand_liveness(key, i);
                    current_block.update_operand_liveness(val, i);
                }
                IR::Call {
                    ref mut dest,
                    ref mut calle,
                    ref mut input,
                } => {
                    if !current_block.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(None, true);
                    current_block.update_dest_liveness(dest); // TODO: SHOULD THIS BE HERE???
                    current_block.update_operand_liveness(calle, i);
                    current_block.update_operand_liveness(input, i);

                    // TODO: maybe here special case the dest to be live on exit
                    // even if it is a temp

                    current_block.push(ir);

                    blocks.push(current_block);
                    current_block = Block::new(None, true);

                    continue;
                }
                IR::Jump { label } => {
                    if !current_block.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(Some(label), false);
                }
                IR::Jnt { label, .. } => {
                    if !current_block.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(Some(label), true);
                }
                IR::Return { ref mut src } => {
                    if !current_block.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(None, false);

                    current_block.set_return(src.id);
                }
                IR::LoadConst { ref mut dest, .. } => {
                    current_block.update_dest_liveness(dest);
                }
            }

            current_block.push(ir);
        }

        blocks
    }
}
