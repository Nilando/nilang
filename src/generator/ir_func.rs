use super::block::Block;
use super::generator::{RawFunc, Var, VarID, FuncID, LabelID, LocalID};
use super::ir::IR;
use super::raw_value::RawValue;
use std::collections::HashMap;

use crate::lexer::Op;
use crate::bytecode::{ByteCode, Reg};
use crate::parser::Span;

enum Location {
    Reg(Reg),
    GlobalStore,
}

type ImmID = usize;

pub struct FuncCompiler {
    code: Vec<ByteCode>,
    locals: HashMap<LocalID, RawValue>,
    var_data: HashMap<VarID, Vec<Location>>,
    reg_data: [Vec<VarID>; 256],
    locked_regs: Vec<Reg>,
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
            locked_regs: vec![],
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
                        let op_reg1 = self.load_value(lhs);
                        let op_reg2 = self.load_value(rhs);
                        let dest_reg = self.get_dest_reg(dest);

                        self.push_binop(dest_reg, op_reg1, op_reg2, op);
                    }
                    IR::ObjLoad { obj, key, dest } => {
                        let obj = self.load_value(obj);
                        let key = self.load_value(key);
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
                        let src = self.load_value(src);

                        self.code.push(ByteCode::Log { src });
                    }
                    IR::ObjStore { obj, key, val } => {
                        let obj = self.load_value(obj);
                        let key = self.load_value(key);
                        let val = self.load_value(val);

                        self.code.push(ByteCode::StoreObj { obj, key, val });
                    }
                    IR::Load { dest, src } => {
                        let src = self.load_value(src);
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
                self.reg_data[reg as usize] = vec![var.id];
            }
            VarID::Temp(_) | VarID::Local(_) => {
                self.code.push(ByteCode::LoadNull { dest: reg });
            }
        };

        reg
    }

    // we need this raw value to live inside a register! no exceptions!
    // this should lock the register for the current instruction
    fn load_value(&mut self, val: RawValue) -> Reg {
        match val {
            RawValue::Var(var) => {
                self.load_var(var)
            }
            RawValue::Int(i) => {
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
            RawValue::Float(i) => {
                // if i fits in a i16 create a load int instr
                // else create a local and then create a load local instr
                todo!()
            }
            RawValue::Bool(src) => {
                let dest = self.get_reg();

                self.code.push( ByteCode::LoadBool { dest, src });

                dest
            }
            RawValue::Func(func_id) => {
                // create a local value with the func id
                // create a local load instr
                todo!()
            }
            RawValue::Null => {
                let dest = self.get_reg();

                self.code.push( ByteCode::LoadNull { dest });

                dest
            }
            RawValue::String(str) => {
                // create a local value with the string
                // create a local load instr
                todo!()
            }
        }
    }

    fn get_reg(&mut self) -> Reg {
        // first look for an empty register

        // 
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
                IR::Load {
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

                    if let RawValue::Var(var) = src {
                        current_block.set_return(var.id);
                    }
                }
            }

            current_block.push(ir);
        }

        blocks
    }
}
