use crate::lexer::Op;
use crate::parser::{Expr, Span, Stmt, Value as RawValue};

use std::collections::HashMap;

type TempID = usize;
type SymID = usize;
type FuncID = usize;
type LabelID = usize;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum VarID {
    Temp(TempID),
    Local(SymID),
    Global(SymID),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub(super) id: VarID,
    next_use: Option<usize>,
    live: bool,
}

impl Var {
    pub fn new(id: VarID) -> Self {
        match id {
            VarID::Temp(_) => Self {
                id,
                next_use: None,
                live: false,
            },
            _ => Self {
                id,
                next_use: None,
                live: true,
            },
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Var(Var),
    String(String),
    Float(f64),
    Int(isize),
    Bool(bool),
    Func(usize),
    Null,
}

#[derive(Debug)]
pub enum Instr {
    Binop {
        dest: Var,
        op: Op,
        lhs: Value,
        rhs: Value,
    },
    ObjStore {
        obj: Value,
        key: Value,
        val: Value,
    },
    ObjLoad {
        dest: Var,
        obj: Value,
        key: Value,
    },
    NewList {
        dest: Var,
    },
    NewMap {
        dest: Var,
    },
    Log {
        src: Value,
    },
    Load {
        dest: Var,
        src: Value,
    },

    // Control Flow Codes Below
    Call {
        dest: Var,
        calle: Value,
        input: Value,
    },
    Jump {
        label: LabelID,
    },
    Return {
        src: Value,
    },
    Jnt {
        label: LabelID,
        cond: Value,
    },
    Label {
        id: LabelID,
    },
}

#[derive(Debug)]
pub struct Func {
    pub(super) id: FuncID,
    pub(super) code: Vec<Span<Instr>>,
    label_counter: LabelID,
    label_stack: Vec<LabelID>,
}

impl Func {
    pub fn into_blocks(mut self) -> Vec<Block> {
        let mut blocks = vec![];
        let mut current_block = Block::new(None, true);

        while let Some(mut instr) = self.code.pop() {
            let i = self.code.len();

            match instr.val {
                Instr::Label { id } => {
                    current_block.label = Some(id);
                    blocks.push(current_block);
                    current_block = Block::new(None, true);
                    continue;
                }
                Instr::Binop {
                    ref mut dest,
                    lhs: ref mut op1,
                    rhs: ref mut op2,
                    ..
                }
                | Instr::ObjLoad {
                    ref mut dest,
                    obj: ref mut op1,
                    key: ref mut op2,
                } => {
                    current_block.update_dest_liveness(dest);
                    current_block.update_operand_liveness(op1, i);
                    current_block.update_operand_liveness(op2, i);
                }
                Instr::Load {
                    ref mut dest,
                    ref mut src,
                } => {
                    current_block.update_dest_liveness(dest);
                    current_block.update_operand_liveness(src, i);
                }
                Instr::Log { ref mut src } => {
                    current_block.update_operand_liveness(src, i);
                }
                Instr::NewList { ref mut dest } | Instr::NewMap { ref mut dest } => {
                    current_block.update_dest_liveness(dest);
                }
                Instr::ObjStore {
                    ref mut obj,
                    ref mut key,
                    ref mut val,
                } => {
                    current_block.update_operand_liveness(obj, i);
                    current_block.update_operand_liveness(key, i);
                    current_block.update_operand_liveness(val, i);
                }
                Instr::Call {
                    ref mut dest,
                    ref mut calle,
                    ref mut input,
                } => {
                    if !current_block.code.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(None, true);
                    current_block.update_operand_liveness(calle, i);
                    current_block.update_operand_liveness(input, i);

                    // TODO: maybe here special case the dest to be live on exit
                    // even if it is a temp

                    current_block.code.push(instr);

                    blocks.push(current_block);
                    current_block = Block::new(None, true);

                    continue;
                }
                Instr::Jump { label } => {
                    if !current_block.code.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(Some(label), false);
                }
                Instr::Jnt { label, .. } => {
                    if !current_block.code.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(Some(label), true);
                }
                Instr::Return { ref mut src } => {
                    if !current_block.code.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(None, false);

                    if let Value::Var(var) = src {
                        current_block.return_var = Some(var.id);
                    }
                }
            }

            current_block.code.push(instr);
        }

        blocks
    }
}

#[derive(Debug)]
pub struct Block {
    label: Option<usize>,
    code: Vec<Span<Instr>>,
    jump: Option<usize>,
    continues: bool,
    liveness: HashMap<Var, (Option<usize>, bool)>,
    return_var: Option<VarID>,
}

impl Block {
    fn new(jump: Option<LabelID>, continues: bool) -> Self {
        Self {
            label: None,
            code: vec![],
            return_var: None,
            liveness: HashMap::new(),
            jump,
            continues,
        }
    }

    fn update_operand_liveness(&mut self, val: &mut Value, i: usize) {
        if let Value::Var(var) = val {
            self.attach_liveness(var);
            self.liveness.insert(*var, (Some(i), true));
        }
    }

    fn update_dest_liveness(&mut self, var: &mut Var) {
        self.attach_liveness(var);
        self.liveness.insert(*var, (None, false));
    }

    fn attach_liveness(&mut self, var: &mut Var) {
        if let Some((next_use, live)) = self.liveness.get(&var) {
            var.next_use = *next_use;
            var.live = *live;
        }
    }
}

#[derive(Debug)]
pub struct Generator {
    func_stack: Vec<Func>,
    func_counter: usize,
    temp_counter: usize,
    pub funcs: HashMap<usize, Func>,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            func_stack: vec![],
            funcs: HashMap::new(),
            temp_counter: 0,
            func_counter: 0,
        }
    }

    pub fn gen_program(&mut self, stmts: Vec<Stmt>) {
        self.push_new_func();
        self.generate(stmts);
        self.pop_func();

        // convert each function into blocks
    }

    pub fn generate(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts.into_iter() {
            match stmt {
                Stmt::Expr(expr) => {
                    self.generate_expr(*expr);
                    self.temp_counter = 0;
                }
                Stmt::Return(expr) => {
                    let var = self.generate_expr(*expr);

                    self.push_instr(Instr::Return { src: var.val }, var.span);
                    self.temp_counter = 0;
                }
                Stmt::Log(expr) => {
                    let var = self.generate_expr(*expr);
                    let instr = Instr::Log { src: var.val };

                    self.push_instr(instr, var.span);
                    self.temp_counter = 0;
                }
                Stmt::Assign { dest, src } => match dest.val {
                    Expr::Value(value) => {
                        if let Value::Var(dest) = self.generate_value(value, dest.span) {
                            let src = self.generate_expr(*src);
                            let instr = Instr::Load { dest, src: src.val };

                            self.push_instr(instr, src.span);
                            self.temp_counter = 0;
                        }
                    }
                    Expr::Access { store, key } => {
                        let span = store.span;
                        let obj = self.generate_expr(*store).val;
                        let key = Value::Var(Var::new(VarID::Local(key)));
                        let val = self.generate_expr(*src).val;
                        let instr = Instr::ObjStore { obj, key, val };

                        self.push_instr(instr, span);
                        self.temp_counter = 0;
                    }
                    Expr::Index { store, key } => {
                        let span = store.span;
                        let obj = self.generate_expr(*store).val;
                        let key = self.generate_expr(*key).val;
                        let val = self.generate_expr(*src).val;
                        let instr = Instr::ObjStore { obj, key, val };

                        self.push_instr(instr, span);
                        self.temp_counter = 0;
                    }
                    _ => panic!("GENERATOR ERROR: assigning to non lvalue"),
                },
                Stmt::While { cond, stmts } => {
                    let label_start = self.get_new_label();
                    let label_end = self.get_new_label();
                    let cond = self.generate_expr(*cond);

                    self.push_instr(Instr::Label { id: label_start }, (0, 0));
                    self.push_instr(
                        Instr::Jnt {
                            cond: cond.val,
                            label: label_end,
                        },
                        cond.span,
                    );
                    self.temp_counter = 0;
                    self.push_label(label_start);
                    self.generate(stmts);
                    self.push_instr(Instr::Jump { label: label_start }, (0, 0));
                    self.push_instr(Instr::Label { id: label_end }, (0, 0));
                    self.pop_label();
                }
                Stmt::If { cond, stmts } => {
                    let cond = self.generate_expr(*cond);
                    let label = self.get_new_label();

                    self.push_instr(
                        Instr::Jnt {
                            cond: cond.val,
                            label,
                        },
                        cond.span,
                    );
                    self.temp_counter = 0;
                    self.generate(stmts);
                    self.push_instr(Instr::Label { id: label }, (0, 0));
                }
                Stmt::IfElse {
                    cond,
                    stmts,
                    else_stmts,
                } => {
                    let cond = self.generate_expr(*cond);
                    let else_start = self.get_new_label();
                    let else_end = self.get_new_label();

                    self.push_instr(
                        Instr::Jnt {
                            cond: cond.val,
                            label: else_start,
                        },
                        cond.span,
                    );
                    self.temp_counter = 0;
                    self.generate(stmts);
                    self.push_instr(Instr::Jump { label: else_end }, (0, 0));
                    self.push_instr(Instr::Label { id: else_start }, (0, 0));
                    self.generate(else_stmts);
                    self.push_instr(Instr::Label { id: else_end }, (0, 0));
                }
                Stmt::Continue => {
                    let func = self.func_stack.last_mut().unwrap();
                    let label = *func.label_stack.last().unwrap();
                    let jmp = Instr::Jump { label };

                    func.code.push(Span::new(jmp, (0, 0)));
                }
                Stmt::Break => {
                    let func = self.func_stack.last_mut().unwrap();
                    let label = 1 + *func.label_stack.last().unwrap();
                    let jmp = Instr::Jump { label };

                    func.code.push(Span::new(jmp, (0, 0)));
                }
            }
        }
    }

    fn generate_expr(&mut self, spanned_expr: Span<Expr>) -> Span<Value> {
        let expr = spanned_expr.val;
        let span = spanned_expr.span;

        match expr {
            Expr::Value(value) => Span::new(self.generate_value(value, span), span),
            Expr::Binop { lhs, op, rhs } => {
                let lhs = self.generate_expr(*lhs).val;
                let rhs = self.generate_expr(*rhs).val;
                let dest = self.get_temp();
                let binop = Instr::Binop {
                    dest,
                    lhs,
                    rhs,
                    op: op.clone(),
                };

                self.push_instr(binop, span);

                Span::new(Value::Var(dest), span)
            }
            Expr::Access { store, key } => {
                let obj = self.generate_expr(*store).val;
                let key = Value::Var(Var::new(VarID::Local(key)));
                let dest = self.get_temp();
                let obj_load = Instr::ObjLoad { dest, obj, key };

                self.push_instr(obj_load, span);

                Span::new(Value::Var(dest), span)
            }
            Expr::Index { store, key } => {
                let obj = self.generate_expr(*store).val;
                let key = self.generate_expr(*key).val;
                let dest = self.get_temp();
                let obj_load = Instr::ObjLoad { dest, obj, key };

                self.push_instr(obj_load, span);

                Span::new(Value::Var(dest), span)
            }
            Expr::Call { calle, input } => {
                let dest = self.get_temp();
                let calle = self.generate_expr(*calle).val;
                let input = if input.is_none() {
                    Value::Null
                } else {
                    self.generate_expr(*input.unwrap()).val
                };
                let instr = Instr::Call { dest, calle, input };

                self.push_instr(instr, span);

                Span::new(Value::Var(dest), span)
            }
        }
    }

    fn generate_value(&mut self, value: RawValue, span: (usize, usize)) -> Value {
        match value {
            RawValue::Null => Value::Null,
            RawValue::Int(i) => Value::Int(i),
            RawValue::Float(f) => Value::Float(f),
            RawValue::Ident(id) => Value::Var(Var::new(VarID::Local(id))),
            RawValue::Global(id) => Value::Var(Var::new(VarID::Global(id))),
            RawValue::Bool(b) => Value::Bool(b),
            RawValue::String(s) => Value::String(s.clone()),
            RawValue::List(list) => {
                let dest = self.get_temp();
                let instr = Instr::NewList { dest };

                self.push_instr(instr, span);

                for (i, expr) in list.into_iter().enumerate() {
                    let item = self.generate_expr(expr);
                    let instr = Instr::ObjStore {
                        obj: Value::Var(dest),
                        key: Value::Int(i as isize),
                        val: item.val,
                    };
                    self.push_instr(instr, span);
                }

                Value::Var(dest)
            }
            RawValue::Map(map) => {
                let dest = self.get_temp();
                let instr = Instr::NewMap { dest };

                self.push_instr(instr, span);

                for (key, val) in map.into_iter() {
                    let key = self.generate_expr(key).val;
                    let val = self.generate_expr(val).val;
                    let instr = Instr::ObjStore {
                        obj: Value::Var(dest),
                        key,
                        val,
                    };

                    self.push_instr(instr, span);
                }

                Value::Var(dest)
            }
            RawValue::Func { stmts } => {
                let temp_counter = self.temp_counter;
                self.temp_counter = 0;

                self.push_new_func();
                self.generate(stmts);

                self.temp_counter = temp_counter;

                let func_val = self.pop_func();
                let dest = self.get_temp();
                let load = Instr::Load {
                    dest,
                    src: func_val,
                };

                self.push_instr(load, span);

                Value::Var(dest)
            }
        }
    }

    fn pop_func(&mut self) -> Value {
        let mut func = self.func_stack.pop().unwrap();
        let end_return = Instr::Return { src: Value::Null };
        let func_val = Value::Func(func.id);

        func.code.push(Span::new(end_return, (0, 0)));
        self.funcs.insert(func.id, func);

        func_val
    }

    fn push_new_func(&mut self) {
        let func = Func {
            id: self.func_counter,
            code: vec![],
            label_stack: vec![],
            label_counter: 0,
        };

        self.func_counter += 1;
        self.func_stack.push(func);
    }

    fn push_label(&mut self, label: usize) {
        self.get_func().label_stack.push(label);
    }

    fn pop_label(&mut self) {
        self.get_func().label_stack.pop();
    }

    fn push_instr(&mut self, instr: Instr, span: (usize, usize)) {
        self.get_func().code.push(Span::new(instr, span));
    }

    fn get_func(&mut self) -> &mut Func {
        self.func_stack.last_mut().unwrap()
    }

    fn get_new_label(&mut self) -> usize {
        let func = self.get_func();
        let label_id = func.label_counter;

        func.label_counter += 1;
        label_id
    }

    fn get_temp(&mut self) -> Var {
        let temp = Var::new(VarID::Temp(self.temp_counter));

        self.temp_counter += 1;
        temp
    }
}
