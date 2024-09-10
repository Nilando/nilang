use super::block::Block;
use super::generator::RawFunc;
use super::ir::{IR, IRVar, IRConst, VarID, FuncID, LabelID, LocalID};
use std::collections::HashMap;

use crate::lexer::Op;
use crate::bytecode::{ByteCode, Reg};
use crate::parser::Span;
use crate::symbol_map::INPUT_SYM_ID;

pub struct FuncCompiler {
    code: Vec<ByteCode>,
    locals: Vec<IRConst>,
    var_data: HashMap<VarID, VarData>,
    reg_data: [Vec<VarID>; 256],
    loaded_regs: Vec<Reg>,
    label_positions: HashMap<LabelID, usize>,
    label_backpatches: HashMap<LabelID, Vec<usize>>,
}

#[derive(PartialEq)]
enum Location {
    Reg(Reg),
    GlobalStore,
}

struct VarData {
    next_use: Option<usize>,
    live: bool,
    locations: Vec<Location>,
}

impl VarData {
    pub fn new(ir_var: IRVar, locations: Vec<Location>) -> Self { 
        Self {
            live: ir_var.live,
            next_use: ir_var.next_use,
            locations,
        } 
    }
}

impl FuncCompiler {
    pub fn new() -> Self {
        let mut reg_data = [const { vec![] }; 256];
        let mut var_data = HashMap::new();

        // input var always starts in reg 0
        let input_var_id = VarID::Local(INPUT_SYM_ID);
        reg_data[0].push(input_var_id);
        var_data.insert(input_var_id, VarData::new(IRVar::new(input_var_id), vec![Location::Reg(0)]));

        Self {
            code: vec![],
            locals: vec![],
            var_data,
            reg_data,
            label_positions: HashMap::new(),
            label_backpatches: HashMap::new(),
            loaded_regs: vec![]
        }
    }

    pub fn compile_instr(&mut self, ir: Span<IR>) {
        match ir.val {
            IR::Binop { dest, op, lhs, rhs } => {
                let lhs_reg = self.load_var(lhs);
                let rhs_reg = self.load_var(rhs);
                let dest_reg = self.assign_var(dest);

                self.push_binop(dest_reg, lhs_reg, rhs_reg, op);
            }
            IR::ObjLoad { dest, obj, key } => {
                let obj = self.load_var(obj);
                let key = self.load_var(key);
                let dest = self.assign_var(dest);

                self.code.push(ByteCode::LoadObj { dest, obj, key });
            }
            IR::NewList { dest } => {
                let dest = self.assign_var(dest);

                self.code.push(ByteCode::LoadList { dest });
            }
            IR::NewMap { dest } => {
                let dest = self.assign_var(dest);

                self.code.push(ByteCode::LoadMap { dest });
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
                // this is a weird one, 
                // we only need to load src...
                // for dest we just update its var/reg data
                let reg = self.load_var(src);
                let var_data = VarData::new(dest, vec![Location::Reg(reg)]);

                self.reg_data[reg as usize].push(dest.id);
                self.var_data.insert(dest.id, var_data);
            }
            IR::LoadConst { dest, src } => {
                let dest = self.assign_var(dest);

                self.load_const(dest, src);
            }
            IR::Jnt { label, cond } => {
                let cond = self.load_var(cond);

                match self.label_positions.get(&label) {
                    None => {
                        let jump_instr_pos = self.code.len();

                        match self.label_backpatches.get_mut(&jump_instr_pos) {
                            None => { self.label_backpatches.insert(label, vec![jump_instr_pos]); }
                            Some(back_patches) => { back_patches.push(jump_instr_pos); }
                        }

                        self.code.push(ByteCode::Jnt { offset: 0, cond });
                    }
                    Some(label_pos) => {
                        let jump_instr_pos = self.code.len();
                        let offset: i16 = (label_pos - jump_instr_pos).try_into().expect("GENERATOR ERROR: JUMP OFFSET EXCEEDED MAXIMUM");

                        self.code.push(ByteCode::Jnt { offset, cond });
                    }
                }
            }
            IR::Jump { label } => {
                match self.label_positions.get(&label) {
                    None => {
                        let jump_instr_pos = self.code.len();

                        match self.label_backpatches.get_mut(&jump_instr_pos) {
                            None => { self.label_backpatches.insert(label, vec![jump_instr_pos]); }
                            Some(back_patches) => { back_patches.push(jump_instr_pos); }
                        }

                        self.code.push(ByteCode::Jump { offset: 0 });
                    }
                    Some(label_pos) => {
                        let jump_instr_pos = self.code.len();
                        let offset: i16 = (label_pos - jump_instr_pos).try_into().expect("GENERATOR ERROR: JUMP OFFSET EXCEEDED MAXIMUM");

                        self.code.push(ByteCode::Jump { offset });
                    }
                }
            }
            IR::Label { id } => {
                let label_idx = self.code.len();

                self.label_positions.insert(id, label_idx);
            }
            IR::Call { dest, calle, input } => {
                // calle just needs to be loaded doesn't really matter where
                let calle_reg = self.load_var(calle);

                // the input reg needs to be loaded into the call site
                // but since the return value will wipe the input
                let call_site = self.load_call_site(input);

                // load what we need before storing globals
                self.store_globals();

                self.assign_var_to_reg(dest, call_site);
                
                self.code.push(ByteCode::Call { call_site, func: calle_reg });
            }
            IR::Return { src } => {
                // reg 0 is special cased,
                // don't update any var data!
                if self.reg_data[0].contains(&src.id) {
                   // do nothing 
                } else {
                    let mut load_flag = true;
                    if let Some(var_data) = self.var_data.get(&src.id) {
                         for loc in var_data.locations.iter() {
                             if let Location::Reg(src) = loc {
                                load_flag = false;
                                self.code.push(ByteCode::Copy { dest: 0, src: *src  } );
                                break;
                             }
                         }
                    }

                    if load_flag {
                        match src.id {
                            VarID::Global(id) => {
                                // load global into reg 0
                                self.code.push(ByteCode::LoadGlobal { dest: 0, sym: id  } )
                            }
                            _ => {
                                // load null into reg 0
                                self.code.push(ByteCode::LoadNull { dest: 0 } )
                            }
                        }
                    }
                }

                self.store_globals();

                self.code.push(ByteCode::Return);
            }
        }
    }

    pub fn compile_func(mut self, id: FuncID, ir_code: Vec<Span<IR>>) -> RawFunc {
        let blocks = self.create_blocks(ir_code);

        for block in blocks.into_iter() {
            for ir in block.as_code().into_iter() {
                self.loaded_regs.clear();
                self.compile_instr(ir);
            }
        }

        self.backpatch_labels();

        RawFunc::new(id, self.code, self.locals)
    }

    fn store_globals(&mut self) {
        for (var_id, var_data) in self.var_data.iter_mut() {
            if let VarID::Global(sym) = var_id {
                let mut reg = None;
                let mut needs_store = true;
                // remove this var id from all regs it was stored in
                
                for loc in var_data.locations.iter() {
                    if let Location::Reg(r) = loc {
                        reg = Some(*r);
                        self.reg_data[*r as usize].retain(|id| {
                            id != var_id
                        });
                    } else {
                        needs_store = false;
                    }
                }

                var_data.locations = vec![Location::GlobalStore];

                // only generate a store instr if the value is stored in a reg,
                // and also is not found in the global store
                if needs_store {
                    if let Some(dest) = reg {
                        self.code.push(ByteCode::StoreGlobal { dest, sym: *sym } );
                    }
                }
            }
        }
    }

    fn backpatch_labels(&mut self) {
        for (label_id, backpatches) in self.label_backpatches.iter() {
            let label_pos = self.label_positions.get(&label_id).unwrap();

            for jump_pos in backpatches.iter() {
                let jump_instr = &mut self.code[*jump_pos];
                let new_offset: i16 = (label_pos - jump_pos).try_into().expect("GENERATOR ERROR: JUMP OFFSET EXCEEDED MAXIMUM");

                match jump_instr {
                    ByteCode::Jnt { ref mut offset, .. } 
                    | ByteCode::Jump { ref mut offset } => *offset = new_offset,
                    _ => panic!("GENERATOR ERROR: BAD LABEL BACKPATCH")
                }
            } 
        }
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

    fn load_call_site(&mut self, var: IRVar) -> Reg {
        let callsite = self.find_callsite();

        match self.var_data.get(&var.id) {
            Some(var_data) => {
                if var_data.locations.len() == 0 {
                    self.code.push(ByteCode::LoadNull { dest: callsite });
                    return callsite;
                } else if var_data.locations.len() == 1 {
                    if let VarID::Global(sym) = var.id {
                        if let Location::GlobalStore = var_data.locations[0] {
                            self.code.push(ByteCode::LoadGlobal { dest: callsite, sym });
                            return callsite;
                        }
                    }
                }

                for loc in var_data.locations.iter() {
                    if let Location::Reg(src) = loc {
                        self.code.push(ByteCode::Copy { dest: callsite, src: *src });
                        break;
                    }
                }
            }

            None => self.code.push(ByteCode::LoadNull { dest: callsite }),
        }
        
        callsite
    }

    fn find_callsite(&self) -> Reg {
        // TODO: make a fallback where if the final reg is used
        // we attempt to make space for a callsite
        for (reg, reg_vars) in self.reg_data.iter().enumerate().rev() {
            if !reg_vars.is_empty() {
                return (reg + 1).try_into().expect("GENERATOR ERROR: UNABLE TO ALLOCATE CALLSITE");
            }
        }

        panic!("GENERATOR ERROR: UNABLE TO ALLOCATE CALLSITE")
    }

    fn load_var(&mut self, var: IRVar) -> Reg {
        match self.var_data.get_mut(&var.id) {
            None => {
                let var_data = VarData::new(var, vec![]);
                self.var_data.insert(var.id, var_data);
            }
            Some(var_data) => {
                var_data.live = var.live;
                var_data.next_use = var.next_use;

                for loc in var_data.locations.iter() {
                    if let Location::Reg(reg) = loc {
                        return *reg;
                    }
                }
            }
        }

        // var wasn't loaded in a reg, if global create a load instr
        // if local, load null
        let reg = self.get_reg();

        match var.id {
            VarID::Global(id) => {
                self.code.push(ByteCode::LoadGlobal { dest: reg, sym: id });
                self.reg_data[reg as usize] = vec![var.id];

                let locations = &mut self.var_data.get_mut(&var.id).unwrap().locations;

                if locations.is_empty() {
                    locations.push(Location::GlobalStore);
                }

                locations.push(Location::Reg(reg));
            }
            VarID::Temp(_) | VarID::Local(_) => {
                self.code.push(ByteCode::LoadNull { dest: reg });
            }
        }

        reg
    }

    fn assign_var_to_reg(&mut self, var: IRVar, dest_reg: Reg) {
        // clear this reg from all other vars locations
        for var_id in self.reg_data[dest_reg as usize].iter() {
            if *var_id == var.id { continue; }

            if let Some(var_data) = self.var_data.get_mut(&var_id) {
                var_data.locations.retain(|reg| {
                    if let Location::Reg(reg) = reg {
                        *reg != dest_reg
                    } else {
                        true
                    }
                });
            }
        }

        // remove this var id from all regs it was stored in
        if let Some(var_data) = self.var_data.get_mut(&var.id) {
            for loc in var_data.locations.iter() {
                if let Location::Reg(reg) = loc {
                    self.reg_data[*reg as usize].retain(|var_id| {
                        *var_id != var.id
                    });
                }
            }
        }

        // set this var so that its only location is here
        let var_data = VarData::new(var, vec![Location::Reg(dest_reg)]);
        self.var_data.insert(var.id, var_data);

        // set the reg so that its only var is the one just loaded
        self.reg_data[dest_reg as usize] = vec![var.id];
    }

    fn assign_var(&mut self, var: IRVar) -> Reg {
        let dest_reg = self.get_reg();

        self.assign_var_to_reg(var, dest_reg);

        dest_reg
    }

    fn load_const(&mut self, dest: Reg, ir_const: IRConst) {
        match ir_const {
            IRConst::Int(i) => {
                match TryInto::<i16>::try_into(i) {
                    Ok(src) => {
                        self.code.push(ByteCode::LoadInt { dest, src });
                    }
                    Err(_) => {
                        let local_id = self.push_local(ir_const);
                        self.code.push(ByteCode::LoadLocal { dest, local_id });
                    }
                }
            }
            IRConst::Bool(src) => self.code.push(ByteCode::LoadBool { dest, src }),
            IRConst::Sym(src) => self.code.push(ByteCode::LoadSym { dest, src }),
            IRConst::Null => self.code.push(ByteCode::LoadNull { dest }),
            IRConst::Func(_) 
            | IRConst::String(_)
            | IRConst::Float(_) => {
                let local_id = self.push_local(ir_const);
                self.code.push(ByteCode::LoadLocal { dest, local_id });
            }
        }
    }

    fn push_local(&mut self, ir_const: IRConst) -> LocalID {
        self.locals.push(ir_const);
        self.locals.len().try_into().expect("GENERATOR ERROR: TOO MANY LOCALS IN FUNC")
    }

    fn get_reg(&mut self) -> Reg {
        for (reg, reg_vars) in self.reg_data.iter().enumerate() {
            let reg = reg as u8;

            // first look for an empty register
            if reg_vars.is_empty() {
                self.loaded_regs.push(reg);
                return reg;
            }

            // don't use loaded registers!
            // they may contain a value that has no next use, and is not live
            // but we still may not use 
            if self.loaded_regs.contains(&reg) {
                continue;
            }

            // register is 'usable' if every val stored in the reg
            // is stored somewhere else, or the value is not live and has no next use
            let mut usable_reg = true;
            for var in reg_vars.iter() { 
                let var_data = self.var_data.get(&var).unwrap();

                if var_data.locations.len() < 2 || var_data.next_use.is_some() || var_data.live { 
                    usable_reg = false;
                    break;
                }
            }

            if usable_reg {
                self.loaded_regs.push(reg);
                return reg;
            }
        }

        // TODO: return a generator error?
        panic!("GENERATOR UNABLE TO ALLOCATE REGISTER")
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

                    // TODO: I THINK THIS LOGIC IS WRONG
                    if !current_block.is_empty() {
                        blocks.push(current_block);
                        current_block = Block::new(None, true);
                    }

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
