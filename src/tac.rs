use crate::parser::{Span, Spanned, Stmt, Expr, Value, Op, MapKey, LhsExpr};
use crate::symbol_map::SymID;
use std::collections::HashSet;

const MAIN_FUNC_ID: usize = 0;
pub type LabelID = usize;
pub type UpvalueID = usize;
pub type TempID = usize;
pub type FuncID = usize;
pub type VersionID = usize;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum VarID {
    // only assigned to once
    Temp(TempID), 
    Upvalue(SymID),

    // can be assgined to multiple times
    Local {
        id: SymID,
        version: Option<VersionID>
    },
    Global {
        id: SymID,
        version: Option<VersionID>
    },
}

impl VarID {
    fn local(id: SymID) -> Self {
        Self::Local {
            id,
            version: None
        }
    }

    fn global(id: SymID) -> Self {
        Self::Local {
            id,
            version: None
        }
    }
}

// rules for processing globals
// whenever we store to a global add a global store instruction
// however whenever we use a global we have a choice
//  1. load the global from the store which will always be the most up to date value
//      - we may have to do this if the global's value is not found in a register
//  2. try to reuse the latest computed version of a global locally

/*
main {
    @a.1 = 1;
    jnt cond .l1
    @a.2 = 2;
    call func // this increments all live global variables meaning this is like we just declared
              // @a.3
    .l1
    @a = phi ( a.1 , a.3 ) // here we would see we don't have any value a.3 so we would need to
                           // create load global instruction
    print @a;
}
*/

#[derive(Debug, PartialEq)]
pub enum TacConst {
    String(String),
    Int(isize),
    Float(f64),
    Bool(bool),
    Func(FuncID),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum Key {
    Var(VarID),
    Sym(SymID)
}

#[derive(Debug, PartialEq)]
pub enum Tac {
    Binop { 
        dest: VarID,
        lhs: VarID,
        op: Op,
        rhs: VarID,
    },
    NewList {
        dest: VarID,
        items: Vec<VarID>,
    },
    NewMap {
        dest: VarID,
        entries: Vec<(Key, VarID)>,
    },
    Print {
        dest: VarID,
    },
    Read {
        dest: VarID,
    },
    Copy {
        dest: VarID,
        src: VarID,
    },
    KeyStore {
        store: VarID,
        key: Key,
        value: VarID,
    },
    KeyLoad {
        dest: VarID,
        store: VarID,
        key: Key,
    },
    LoadConst {
        dest: VarID,
        val: TacConst,
    },
    UpvalueStore {
        dest: VarID,
        val: VarID, // this can only be a temp or a local
        upvalue: UpvalueID
    },
    Call {
        dest: VarID,
        calle: VarID,
        args: Vec<VarID>,
    },
    Return {
        src: VarID,
    },
    Jump {
        label: LabelID,
    },
    Jnt {
        label: LabelID,
        cond: VarID,
    },
    Label {
        label: LabelID,
    },
}

#[derive(Debug)]
pub struct TacFunc {
    id: FuncID,
    inputs: HashSet<SymID>,
    pub tac: Vec<Tac>,
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
            func: TacFunc::new(id, inputs.clone()),
            temp_counter: 0,
            label_counter: 0,
            defined_variables: inputs,
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
        let main_func = TacFuncGenerator::new(MAIN_FUNC_ID, HashSet::new());

        Self {
            func_counter: 0,
            generators: vec![main_func],
            streaming_callback: Box::new(callback)
        }
    } 

    fn generate_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr) => { self.generate_expr(expr); }
            Stmt::Return(expr) => self.generate_return(expr),
            Stmt::Break => self.generate_break(),
            Stmt::Continue => self.generate_continue(),
            Stmt::While { cond, stmts } => self.generate_while_block(cond, stmts),
            Stmt::If { cond, stmts } => self.generate_if_block(cond, stmts),
            Stmt::IfElse { cond, stmts, else_stmts } => self.generate_if_else_block(cond, stmts, else_stmts),
            Stmt::FuncDecl { ident, inputs, stmts } => self.generate_func_decl(ident, inputs, stmts),
            Stmt::Assign { dest, src } => self.generate_assign(dest, src),
        };
    }

    fn generate_return(&mut self, return_expr: Option<Spanned<Expr>>) {
        let var =
        if let Some(expr) = return_expr {
            self.generate_expr(expr)
        } else {
            self.new_temp()
        };

        self.emit(Tac::Return { src: var });
    }

    fn generate_assign(&mut self, lhs_expr: Spanned<LhsExpr>, src: Spanned<Expr>) {
        let src = self.generate_expr(src);
        let span = lhs_expr.get_span();

        match lhs_expr.item {
            LhsExpr::Index { store, key } => {
                let store = self.generate_expr(*store);
                let key = self.generate_expr(*key);

                self.generate_key_store(store, Key::Var(key), src, span);
            }
            LhsExpr::Access { store, key } => {
                let store = self.generate_expr(*store);

                self.generate_key_store(store, Key::Sym(key), src, span);
            }
            LhsExpr::Local(sym_id) => {
                self.define_var(sym_id);
                self.emit(
                    Tac::Copy {
                        dest: VarID::local(sym_id),
                        src
                    }
                )
            }
            LhsExpr::Global(sym_id) => {
                self.emit(
                    Tac::Copy {
                        dest: VarID::global(sym_id),
                        src
                    }
                )
            }
        }
    }

    fn generate_expr(&mut self, spanned_expr: Spanned<Expr>) -> VarID {
        let span = spanned_expr.get_span();
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

                self.generate_binop(v1, op, v2, span)
            }
            Expr::Access { store, key } => {
                let var = self.generate_expr(*store);

                self.generate_key_load(var, Key::Sym(key), span)
            }
            Expr::Index { store, key } => {
                let store_val = self.generate_expr(*store);
                let key_val = self.generate_expr(*key);

                self.generate_key_load(store_val, Key::Var(key_val), span)
            }
            Expr::Call { calle, args } => {
                let arg_vals = args.into_iter().map(|a| self.generate_expr(a)).collect();
                let calle_val = self.generate_expr(*calle);

                self.generate_call(calle_val, arg_vals, span)
            }
        }
    }

    fn generate_value(&mut self, value: Value) -> VarID {
        match value {
            Value::Int(i) => self.load_const(TacConst::Int(i)),
            Value::Null => self.load_const(TacConst::Null),
            Value::Float(f) => self.load_const(TacConst::Float(f)),
            Value::Bool(b) => self.load_const(TacConst::Bool(b)),
            Value::String(s) => self.load_const(TacConst::String(s)),
            Value::Global(sym_id) => VarID::global(sym_id),
            Value::Ident(sym_id) => self.generate_ident(sym_id),
            Value::Map(map) => self.generate_map(map),
            Value::List(list) => self.generate_list(list),
            Value::InlineFunc { inputs, stmts } => self.generate_func(inputs, stmts),
        }
    }

    fn generate_call(&mut self, calle: VarID, args: Vec<VarID>, span: Span) -> VarID {
        let temp = self.new_temp();

        self.emit_spanned(
            Tac::Call {
                dest: temp,
                calle,
                args,
            },
            span
        );

        temp
    }

    fn generate_key_store(&mut self, store: VarID, key: Key, value: VarID, span: Span) {
        self.emit_spanned(
            Tac::KeyStore {
                store,
                key,
                value
            },
            span
        );
    }

    fn generate_key_load(&mut self, store: VarID, key: Key, span: Span) -> VarID {
        let temp = self.new_temp();

        self.emit_spanned(
            Tac::KeyLoad {
                dest: temp,
                store,
                key,
            },
            span
        );

        temp
    }

    fn generate_break(&mut self) {
        let label = self.get_loop_end();

        self.emit_jump(label);
    }

    fn generate_continue(&mut self) {
        let label = self.get_loop_start();

        self.emit_jump(label);
    }

    fn generate_func_decl(&mut self, ident: SymID, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) {
        self.define_var(ident);

        let func = self.generate_func(inputs, stmts);

        self.emit(
            Tac::Copy {
                dest: VarID::local(ident),
                src: func
            }
        );
    }

    fn generate_func(&mut self, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) -> VarID {
        let temp = self.new_temp();

        // TODO: here we need to create store upvalue instructions

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

        self.stream_current_func();

        func_id
    }

    fn stream_current_func(&mut self) {
        let func = self.process_top_func();

        (self.streaming_callback)(func);
    }

    fn process_top_func(&mut self) -> TacFunc {
        let mut func = self.generators.pop().unwrap().func;

        // ... do we need to do any processing?
        // insert upvalue load instructions

        func
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

    fn generate_ident(&mut self, sym_id: SymID) -> VarID {
        if self.defined_local(sym_id) {
            VarID::local(sym_id)
        } else if self.defined_upvalue(sym_id) {
            self.set_upvalue(sym_id);

            VarID::Upvalue(sym_id)
        } else {
            self.new_temp()
        }
    }

    fn generate_map(&mut self, pairs: Vec<(MapKey, Spanned<Expr>)>) -> VarID {
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

    fn generate_list(&mut self, exprs: Vec<Spanned<Expr>>) -> VarID {
        let temp = self.new_temp();
        let items = exprs.into_iter().map(|e| self.generate_expr(e)).collect();

        self.emit(Tac::NewList { dest: temp , items});

        temp
    }

    fn generate_binop(&mut self, lhs: VarID, op: Op, rhs: VarID, span: Span) -> VarID {
        let temp = self.new_temp();

        self.emit_spanned(
            Tac::Binop {
                dest: temp,
                lhs,
                op,
                rhs
            },
            span
        );

        temp
    }

    fn generate_print(&mut self, dest: VarID) -> VarID {
        self.emit(
            Tac::Print {
                dest,
            }
        );

        self.new_temp()
    }

    fn generate_read(&mut self) -> VarID {
        let temp = self.new_temp();

        self.emit(
            Tac::Read {
                dest: temp,
            }
        );

        temp
    }

    fn load_const(&mut self, tac_const: TacConst) -> VarID {
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
        self.emit(Tac::Label { label })
    }

    fn emit_jump(&mut self, label: LabelID) {
        self.emit(Tac::Jump { label })
    }

    fn emit(&mut self, tac: Tac) {
        self.generators.last_mut().as_mut().unwrap().func.tac.push(tac);
    }

    fn emit_spanned(&mut self, tac: Tac, span: Span) {
        let func = &mut self.generators.last_mut().unwrap().func;
        let index = func.tac.len();

        func.tac.push(tac);

        if let Some(prev_span) = func.spans.last() {
            if prev_span.1 != span {
                func.spans.push((index, span))
            }
        } else {
            func.spans.push((index, span))
        }
    }

    fn push_loop_ctx(&mut self, ctx: LoopCtx) {
        self.generators.last_mut().as_mut().unwrap().loop_ctxs.push(ctx);
    }

    fn new_func_id(&mut self) -> FuncID {
        self.func_counter += 1;

        self.func_counter
    }

    fn new_temp(&mut self) -> VarID {
        self.generators.last_mut().as_mut().unwrap().temp_counter += 1;

        VarID::Temp(self.generators.last().unwrap().temp_counter)
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

    ctx.generate_stmts(stmts);

    ctx.stream_current_func();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_program;
    use crate::symbol_map::SymbolMap;

    fn expect_tac(input: &str, expected_code: Vec<Vec<Tac>>) {
        let mut syms = SymbolMap::new();

        expect_tac_with_syms(input, expected_code, &mut syms);
    }

    fn expect_tac_with_syms(input: &str, mut expected_code: Vec<Vec<Tac>>, syms: &mut SymbolMap) {
        let parse_result = parse_program(input, syms);
        assert!(parse_result.errors.is_empty());

        stream_tac_from_stmts(parse_result.value.unwrap(), |tac_func| {
            assert_eq!(tac_func.tac, expected_code.pop().unwrap())
        });
    }

    #[test]
    fn load_int_value() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_value(Value::Int(69));

        assert!(ctx.generators[0].func.tac[0] == Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::Int(69) });
        assert!(ctx.generators[0].func.spans.is_empty());
    }

    #[test]
    fn load_string_value() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_value(Value::String("test".to_string()));

        assert!(ctx.generators[0].func.tac[0] == Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::String("test".to_string()) });
    }

    #[test]
    fn load_float_value() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_value(Value::Float(612357.234532));

        assert!(ctx.generators[0].func.tac[0] == Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::Float(612357.234532) })
    }

    #[test]
    fn emit_read() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_expr(Spanned::new(Expr::Read, (0, 1)));

        assert!(ctx.generators[0].func.tac[0] == Tac::Read { dest: VarID::Temp(1) });
        assert!(ctx.generators[0].func.spans.is_empty());
    }

    #[test]
    fn expr_stmt_tac() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::Int(1) }
            ]
        ];
        let input = "1;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_expr_tac() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::Int(1) },
                Tac::LoadConst { dest: VarID::Temp(2), val: TacConst::Int(1) },
                Tac::LoadConst { dest: VarID::Temp(3), val: TacConst::Int(1) },
                Tac::Binop { dest: VarID::Temp(4), lhs: VarID::Temp(2), op: Op::Multiply, rhs: VarID::Temp(3) },
                Tac::Binop { dest: VarID::Temp(5), lhs: VarID::Temp(1), op: Op::Multiply, rhs: VarID::Temp(4) }
            ]
        ];
        let input = "1 * 1 * 1;";

        expect_tac(input, tac);
    }

    #[test]
    fn index_assignment_tac() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::Int(1) },
                Tac::LoadConst { dest: VarID::Temp(3), val: TacConst::Int(1) },
                Tac::NewList { dest: VarID::Temp(2), items: vec![VarID::Temp(3)] },
                Tac::LoadConst { dest: VarID::Temp(4), val: TacConst::Int(0) },
                Tac::KeyStore { store: VarID::Temp(2), key: Key::Var(VarID::Temp(4)), value: VarID::Temp(1) },
            ]
        ];
        let input = "[1][0] = 1;";

        expect_tac(input, tac);
    }

    #[test]
    fn access_assignment_tac() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::Int(1) },
                Tac::KeyStore { store: VarID::Temp(2), key: Key::Sym(syms.get_id("b")), value: VarID::Temp(1) },
            ]
        ];
        let input = "a.b = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn assign_local() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::Int(1) },
                Tac::Copy { dest: VarID::local(syms.get_id("a")), src: VarID::Temp(1) },
            ]
        ];
        let input = "a = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn assign_global() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::Int(1) },
                Tac::Copy { dest: VarID::global(syms.get_id("a")), src: VarID::Temp(1) },
            ]
        ];
        let input = "@a = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn generates_return_with_value() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::Bool(true) },
                Tac::Return { src: VarID::Temp(1) },
            ]
        ];
        let input = "return true;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_return_with_no_value() {
        let tac = vec![
            vec![
                Tac::Return { src: VarID::Temp(1) },
            ]
        ];
        let input = "return;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_print_expr() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::String("Hello World".to_string()) },
                Tac::Print { dest: VarID::Temp(1) },
            ]
        ];
        let input = "print \"Hello World\";";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_break_and_continue_stmts() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: VarID::Temp(1), val: TacConst::Bool(true) },
                Tac::Label { label: 1 },
                Tac::Jnt { cond: VarID::Temp(1), label: 2 },
                Tac::Jump { label: 1 },
                Tac::Jump { label: 2 },
                Tac::Jump { label: 1 },
                Tac::Label { label: 2 },
            ]
        ];
        let input = "while true { continue; break; }";

        expect_tac(input, tac);
    }
}
