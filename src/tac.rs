use crate::parser::{Span, Spanned, Stmt, Expr, Value, Op, MapKey, LhsExpr};
use crate::symbol_map::SymID;
use std::collections::HashSet;

const LABEL_BACKPATCH: usize = 0;
pub type LabelID = usize;
pub type UpvalueID = usize;
pub type TempID = usize;
pub type FuncID = usize;

#[derive(Copy, Clone)]
pub enum TacVar {
    Temp(TempID),
    Local(SymID),
    Upvalue(SymID),
    Global(SymID),
}

pub enum TacConst {
    String(String),
    Int(isize),
    Float(f64),
    Bool(bool),
    Func(FuncID),
    Null,
}

#[derive(Copy, Clone)]
pub enum Key {
    Var(TacVar),
    Sym(SymID)
}

pub enum Tac {
    Binop { 
        dest: TacVar,
        lhs: TacVar,
        op: Op,
        rhs: TacVar,
    },
    NewList {
        dest: TacVar,
        items: Vec<TacVar>,
    },
    NewMap {
        dest: TacVar,
        entries: Vec<(Key, TacVar)>,
    },
    Print {
        dest: TacVar,
    },
    Read {
        dest: TacVar,
    },
    Copy {
        dest: TacVar,
        src: TacVar,
    },
    KeyStore {
        store: TacVar,
        key: Key,
        value: TacVar,
    },
    KeyLoad {
        dest: TacVar,
        store: TacVar,
        key: Key,
    },
    LoadConst {
        dest: TacVar,
        val: TacConst,
    },
    UpvalueStore {
        dest: TacVar,
        val: TacVar,
    },
    UpvalueLoad {
        dest: TacVar,
        id: UpvalueID,
    },
    Call {
        dest: TacVar,
        calle: TacVar,
        args: Vec<TacVar>,
    },
    Return {
        src: TacVar,
    },
    Jump {
        label: LabelID,
    },
    Jnt {
        label: LabelID,
        cond: TacVar,
    },
    Label {
        label: LabelID,
    },
}

pub struct TacFunc {
    id: FuncID,
    inputs: HashSet<SymID>,
    tac: Vec<Tac>,
    spans: Vec<(usize, Span)>,
    upvalues: HashSet<SymID>,
}

struct LoopCtx {
    start: LabelID,
    end: LabelID,
}

impl TacFunc {
    fn new(id: FuncID, inputs: HashSet<SymID>) -> Self {
        Self {
            id,
            inputs,
            tac: vec![],
            spans: vec![],
            upvalues: HashSet::new(),
        }
    }
}

struct TacFuncGenerator {
    func: TacFunc,
    temp_counter: usize,
    label_counter: usize,
    defined_variables: HashSet<SymID>,
    loop_ctxs: Vec<LoopCtx>
}

impl TacFuncGenerator {
    fn new(id: FuncID, inputs: HashSet<SymID>) -> Self {
        Self {
            func: TacFunc::new(id, inputs),
            temp_counter: 0,
            label_counter: 0,
            defined_variables: HashSet::new(),
            loop_ctxs: vec![],
        }
    }
}

struct TacGenCtx<'a> {
    func_counter: usize,
    generators: Vec<TacFuncGenerator>,
    streaming_callback: Box<dyn FnMut(TacFunc) + 'a>
}

impl<'a> TacGenCtx<'a> {
    pub fn new(callback: impl FnMut(TacFunc) + 'a) -> Self {
        let main_func = TacFuncGenerator::new(0, HashSet::new());

        Self {
            func_counter: 0,
            generators: vec![main_func],
            streaming_callback: Box::new(callback)
        }
    } 

    fn generate_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr) => { self.generate_expr(expr); }
            Stmt::Return(return_expr) => {
                let var =
                if let Some(expr) = return_expr {
                    self.generate_expr(expr)
                } else {
                    self.new_temp()
                };

                self.generate_return(var);
            }
            Stmt::Break => {
                self.generate_break();
            }
            Stmt::Continue => {
                self.generate_continue();
            }
            Stmt::While { cond, stmts } => {
                self.generate_while_block(cond, stmts);
            }
            Stmt::If { cond, stmts } => {
                self.generate_if_block(cond, stmts);
            }
            Stmt::IfElse { cond, stmts, else_stmts } => {
                self.generate_if_else_block(cond, stmts, else_stmts);
            }
            Stmt::FuncDecl { ident, inputs, stmts } => {
                self.define_var(ident);
                let func = self.generate_func(inputs, stmts);

                self.emit(
                    Tac::Copy {
                        dest: TacVar::Local(ident),
                        src: func
                    }
                )
            }
            Stmt::Assign { dest, src } => {
                let src = self.generate_expr(src);

                self.generate_assign(dest, src);
            }
        }
    }

    fn generate_assign(&mut self, lhs_expr: Spanned<LhsExpr>, src: TacVar) {
        match lhs_expr.item {
            LhsExpr::Index { store, key } => {
                let store = self.generate_expr(*store);
                let key = self.generate_expr(*key);

                self.generate_key_store(store, Key::Var(key), src);
            }
            LhsExpr::Access { store, key } => {
                let store = self.generate_expr(*store);

                self.generate_key_store(store, Key::Sym(key), src);
            }
            LhsExpr::Local(sym_id) => {
                self.define_var(sym_id);
                self.emit(
                    Tac::Copy {
                        dest: TacVar::Local(sym_id),
                        src
                    }
                )
            }
            LhsExpr::Global(sym_id) => {
                self.emit(
                    Tac::Copy {
                        dest: TacVar::Global(sym_id),
                        src
                    }
                )
            }
        }
    }

    fn generate_expr(&mut self, spanned_expr: Spanned<Expr>) -> TacVar {
        match spanned_expr.item {
            Expr::Value(value) => self.generate_value(value),
            Expr::Read => self.generate_read(),
            Expr::Print(expr) => {
                let var = self.generate_expr(*expr);

                self.generate_print(var)
            }
            Expr::Binop { lhs, op, rhs } => {
                let v1 = self.generate_expr(*lhs);
                let v2 = self.generate_expr(*rhs);

                self.generate_binop(v1, op, v2)
            }
            Expr::Access { store, key } => {
                let var = self.generate_expr(*store);

                self.generate_key_load(var, Key::Sym(key))
            }
            Expr::Index { store, key } => {
                let store_val = self.generate_expr(*store);
                let key_val = self.generate_expr(*key);

                self.generate_key_load(store_val, Key::Var(key_val))
            }
            Expr::Call { calle, args } => {
                let arg_vals = args.into_iter().map(|a| self.generate_expr(a)).collect();
                let calle_val = self.generate_expr(*calle);

                self.generate_call(calle_val, arg_vals)
            }
        }
    }

    fn generate_value(&mut self, value: Value) -> TacVar {
        match value {
            Value::Int(i) => self.load_const(TacConst::Int(i)),
            Value::Null => self.load_const(TacConst::Null),
            Value::Float(f) => self.load_const(TacConst::Float(f)),
            Value::Bool(b) => self.load_const(TacConst::Bool(b)),
            Value::String(s) => self.load_const(TacConst::String(s)),
            Value::Global(sym_id) => TacVar::Global(sym_id),
            Value::Ident(sym_id) => self.generate_ident(sym_id),
            Value::Map(map) => self.generate_map(map),
            Value::List(list) => self.generate_list(list),
            Value::InlineFunc { inputs, stmts } => self.generate_func(inputs, stmts),
        }
    }

    fn generate_call(&mut self, calle: TacVar, args: Vec<TacVar>) -> TacVar {
        let temp = self.new_temp();

        self.emit(
            Tac::Call {
                dest: temp,
                calle,
                args,
            }
        );

        temp
    }

    fn generate_key_store(&mut self, store: TacVar, key: Key, value: TacVar) {
        self.emit(
            Tac::KeyStore {
                store,
                key,
                value
            }
        );
    }

    fn generate_key_load(&mut self, store: TacVar, key: Key) -> TacVar {
        let temp = self.new_temp();

        self.emit(
            Tac::KeyLoad {
                dest: temp,
                store,
                key,
            }
        );

        temp
    }

    fn generate_return(&mut self, val: TacVar) {
        self.emit(Tac::Return { src: val });
    }

    fn generate_break(&mut self) {
        let label = self.get_loop_end();

        self.emit_jump(label);
    }

    fn generate_continue(&mut self) {
        let label = self.get_loop_start();

        self.emit_jump(label);
    }

    fn generate_func(&mut self, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) -> TacVar {
        let temp = self.new_temp();
        let func_id = self.new_func(inputs, stmts);

        self.emit(
            Tac::LoadConst {
                dest: temp,
                val: TacConst::Func(func_id),
            }
        );

        temp
    }

    fn new_func(&mut self, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) -> FuncID {
        let func_id = self.new_func_id();
        let generator = TacFuncGenerator::new(func_id, HashSet::from_iter(inputs.item));

        self.generators.push(generator);

        self.generate_stmts(stmts);

        func_id
    }

    fn generate_if_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>) {
        let label = self.new_label();
        let cond = self.generate_expr(cond);

        self.emit(
            Tac::Jnt {
                cond,
                label,
            }
        );
        self.generate_stmts(stmts);
        self.emit_label(label);
    }

    fn generate_if_else_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>, else_stmts: Vec<Stmt>) {
        let else_end = self.new_label();
        let else_start = self.new_label();
        let cond = self.generate_expr(cond);

        self.emit(
            Tac::Jnt {
                cond,
                label: else_start,
            }
        );
        self.generate_stmts(stmts);
        self.emit_jump(else_end);
        self.emit_label(else_start);
        self.generate_stmts(else_stmts);
        self.emit_label(else_end);
    }

    fn generate_while_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>) {
        let start = self.new_label();
        let end = self.new_label();

        self.push_loop_ctx(LoopCtx { start, end });

        let cond = self.generate_expr(cond);

        self.emit_label(start);
        self.emit(
            Tac::Jnt {
                cond,
                label: end,
            }
        );
        self.generate_stmts(stmts);
        self.emit_jump(start);
        self.emit_label(end);
    }

    fn generate_ident(&mut self, sym_id: SymID) -> TacVar {
        if self.defined_local(sym_id) {
            TacVar::Local(sym_id)
        } else if self.defined_upvalue(sym_id) {
            self.set_upvalue(sym_id);

            TacVar::Upvalue(sym_id)
        } else {
            self.new_temp()
        }
    }

    fn generate_map(&mut self, pairs: Vec<(MapKey, Spanned<Expr>)>) -> TacVar {
        let temp = self.new_temp();
        let entries = pairs.into_iter().map(|(k, value)| {
            let key = 
            match k {
                MapKey::Sym(sym) => Key::Sym(sym),
                MapKey::Expr(expr) => Key::Var(self.generate_expr(expr))
            };

            let value = self.generate_expr(value);

            (key, value)
        }).collect();

        self.emit(Tac::NewMap { dest: temp , entries});

        temp
    }

    fn generate_list(&mut self, exprs: Vec<Spanned<Expr>>) -> TacVar {
        let temp = self.new_temp();
        let items = exprs.into_iter().map(|e| self.generate_expr(e)).collect();

        self.emit(Tac::NewList { dest: temp , items});

        temp
    }

    fn generate_binop(&mut self, lhs: TacVar, op: Op, rhs: TacVar) -> TacVar {
        let temp = self.new_temp();

        self.emit(
            Tac::Binop {
                dest: temp,
                lhs,
                op,
                rhs
            }
        );

        temp
    }

    fn generate_print(&mut self, dest: TacVar) -> TacVar {
        self.emit(
            Tac::Print {
                dest,
            }
        );

        self.new_temp()
    }

    fn generate_read(&mut self) -> TacVar {
        let temp = self.new_temp();

        self.emit(
            Tac::Read {
                dest: temp,
            }
        );

        temp
    }

    fn load_const(&mut self, tac_const: TacConst) -> TacVar {
        let temp = self.new_temp();

        self.emit(
            Tac::LoadConst {
                dest: temp,
                val: tac_const
            }
        );

        temp
    }

    fn emit_label(&mut self, label: LabelID) {
        self.emit_spanned(Tac::Label { label }, None)
    }

    fn emit_jump(&mut self, label: LabelID) {
        self.emit_spanned(Tac::Jump { label }, None)
    }

    fn emit(&mut self, tac: Tac) {
        self.emit_spanned(tac, None)
    }

    fn emit_spanned(&mut self, tac: Tac, span: Option<Span>) {
        self.generators.last_mut().as_mut().unwrap().func.tac.push(tac);
    }

    fn push_loop_ctx(&mut self, ctx: LoopCtx) {
        self.generators.last_mut().as_mut().unwrap().loop_ctxs.push(ctx);
    }

    fn new_func_id(&mut self) -> FuncID {
        self.func_counter += 1;

        self.func_counter
    }

    fn new_temp(&mut self) -> TacVar {
        self.generators.last_mut().as_mut().unwrap().temp_counter += 1;

        TacVar::Temp(self.generators.last().unwrap().temp_counter)
    }

    fn new_label(&mut self) -> LabelID {
        self.generators.last_mut().as_mut().unwrap().label_counter += 1;

        self.generators.last().unwrap().label_counter
    }

    fn get_loop_start(&mut self) -> LabelID {
        self.generators.last_mut().as_mut().unwrap().loop_ctxs.last().unwrap().start
    }

    fn get_loop_end(&mut self) -> LabelID {
        self.generators.last_mut().as_mut().unwrap().loop_ctxs.last().unwrap().end
    }

    fn define_var(&mut self, sym: SymID) {
        self.generators.last_mut().as_mut().unwrap().defined_variables.insert(sym);
    }

    fn set_upvalue(&mut self, sym: SymID) {
        self.generators.last_mut().as_mut().unwrap().func.upvalues.insert(sym);
    }

    fn defined_local(&self, sym: SymID) -> bool {
        self.generators.last().unwrap().defined_variables.contains(&sym)
    }

    fn defined_upvalue(&self, sym: SymID) -> bool {
        for i in 0..(self.generators.len() - 1) {
            if self.generators[i].defined_variables.contains(&sym) {
                return true;
            }
        }

        return false;
    }

    fn generate_stmts(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts.into_iter() {
            self.generate_stmt(stmt);
        }
    }
}

pub fn stream_tac_from_stmts(stmts: Vec<Stmt>, callback: impl FnMut(TacFunc)) {
    let mut ctx = TacGenCtx::new(callback);

    ctx.generate_stmts(stmts)
}
