use crate::parser::{Span, Spanned, Stmt, Expr, Value, Op, MapKey, LhsExpr};
use std::hash::Hash;
use crate::symbol_map::SymID;
use std::collections::HashSet;

const MAIN_FUNC_ID: usize = 0;
pub type LabelID = usize;
pub type UpvalueID = usize;
pub type TempID = usize;
pub type FuncID = usize;
pub type VersionID = usize;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum VarID {
    Temp(TempID), 
    Upvalue(SymID),
    Local(SymID),
    Global(SymID),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Var {
    pub id: VarID,
    ver: Option<usize>,
    //last_use: Option<usize>,
    //live: Option<bool>
}

impl Hash for Var {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.ver.hash(state);
        // don't use last use and liveness to hash
    }
}

impl Var {
    fn local(id: SymID) -> Self {
        Self::new(VarID::Local(id))
    }

    fn global(id: SymID) -> Self {
        Self::new(VarID::Global(id))
    }

    fn temp(id: usize) -> Self {
        Self::new(VarID::Temp(id))
    }

    fn upvalue(id: SymID) -> Self {
        Self::new(VarID::Upvalue(id))
    }

    fn new(id: VarID) -> Self {
        Self {
            id,
            ver: None,
            //last_use: None,
            //live: None,
        }
    }
}

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
    Var(Var),
    Sym(SymID),
}

#[derive(Debug, PartialEq)]
pub enum Tac {
    Binop { 
        dest: Var,
        op: Op,
        lhs: Var,
        rhs: Var,
    },
    NewList {
        dest: Var,
    },
    NewMap {
        dest: Var,
    },
    Print {
        src: Var,
    },
    Read {
        dest: Var,
    },
    Copy {
        dest: Var,
        src: Var,
    },
    KeyStore {
        store: Var,
        src: Var,
        key: Key,
    },
    KeyLoad {
        dest: Var,
        store: Var,
        key: Key,
    },
    LoadConst {
        dest: Var,
        src: TacConst,
    },
    UpvalueStore {
        store: Var,
        src: SymID,
    },
    LoadArg {
        src: Var
    },
    Call {
        dest: Var,
        src: Var,
    },
    Return {
        src: Var,
    },
    Jump {
        label: LabelID,
    },
    Jnt {
        label: LabelID,
        src: Var,
    },
    Label {
        label: LabelID,
    },
}

impl Tac {
    pub fn used_vars(&self) -> (Option<&Var>, Option<&Var>, Option<&Var>) {
        match self {
            Tac::Binop { lhs, rhs, .. } => (Some(lhs), Some(rhs), None),

            Tac::Copy { src, .. } |
            Tac::Print { src, .. } |
            Tac::LoadArg { src, .. } |
            Tac::Call { src, .. } |
            Tac::Return { src, .. } |
            Tac::Jnt { src, .. } 
                => (Some(src), None, None),

            Tac::KeyLoad { store, key, .. } => {
                if let Key::Var(var) = key {
                    (Some(store), Some(var), None)
                } else {
                    (Some(store), None, None)
                }
            }

            Tac::KeyStore { store, key, src } => {
                if let Key::Var(var) = key {
                    (Some(store), Some(src), Some(var))
                } else {
                    (Some(store), Some(src), None)
                }
            }

            _ => (None, None, None)
        }
    }

    pub fn dest_var(&self) -> Option<&Var> {
        match self {
            Tac::Call { dest, .. } |
            Tac::LoadConst { dest, .. } |
            Tac::KeyLoad { dest, .. } |
            Tac::Copy { dest, .. } |
            Tac::Read { dest, .. } |
            Tac::NewMap { dest, .. } |
            Tac::NewList { dest, .. } |
            Tac::Binop { dest, .. }
            => Some(dest),
            _ => None
        }
    }
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
                        dest: Var::local(sym_id),
                        src
                    }
                )
            }
            LhsExpr::Global(sym_id) => {
                self.emit(
                    Tac::Copy {
                        dest: Var::global(sym_id),
                        src
                    }
                )
            }
        }
    }

    fn generate_expr(&mut self, spanned_expr: Spanned<Expr>) -> Var {
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

    fn generate_value(&mut self, value: Value) -> Var {
        match value {
            Value::Int(i) => self.load_const(TacConst::Int(i)),
            Value::Null => self.load_const(TacConst::Null),
            Value::Float(f) => self.load_const(TacConst::Float(f)),
            Value::Bool(b) => self.load_const(TacConst::Bool(b)),
            Value::String(s) => self.load_const(TacConst::String(s)),
            Value::Global(sym_id) => Var::global(sym_id),
            Value::Ident(sym_id) => self.generate_ident(sym_id),
            Value::Map(map) => self.generate_map(map),
            Value::List(list) => self.generate_list(list),
            Value::InlineFunc { inputs, stmts } => self.generate_func(inputs, stmts),
        }
    }

    fn generate_call(&mut self, calle: Var, args: Vec<Var>, span: Span) -> Var {
        for src in args {
            self.emit(
                Tac::LoadArg {
                    src
                }
            )
        }

        let temp = self.new_temp();

        self.emit_spanned(
            Tac::Call {
                dest: temp,
                src: calle,
            },
            span
        );

        temp
    }

    fn generate_key_store(&mut self, store: Var, key: Key, src: Var, span: Span) {
        self.emit_spanned(
            Tac::KeyStore {
                store,
                key,
                src
            },
            span
        );
    }

    fn generate_key_load(&mut self, store: Var, key: Key, span: Span) -> Var {
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
                dest: Var::local(ident),
                src: func
            }
        );
    }

    fn generate_func(&mut self, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) -> Var {
        let temp = self.new_temp();


        let (func_id, upvalues) = self.new_func(inputs, stmts);

        self.emit(
            Tac::LoadConst {
                dest: temp,
                src: TacConst::Func(func_id),
            }
        );


        for upvalue in upvalues.iter() {
            self.emit(
                Tac::UpvalueStore {
                    store: temp,
                    src: *upvalue
                }
            );
        }

        temp
    }

    fn new_func(&mut self, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) -> (FuncID, HashSet<SymID>) {
        let func_id = self.new_func_id();
        let generator = TacFuncGenerator::new(func_id, HashSet::from_iter(inputs.item));

        self.generators.push(generator);

        self.generate_stmts(stmts);

        let upvalues = self.generators.last().unwrap().func.upvalues.clone();

        self.stream_current_func();

        (func_id, upvalues)
    }

    fn stream_current_func(&mut self) {
        let func = self.generators.pop().unwrap().func;

        (self.streaming_callback)(func);
    }

    fn generate_if_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>) {
        let label = self.new_label();
        let cond = self.generate_expr(cond);

        self.emit(
            Tac::Jnt {
                src: cond,
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
                src: cond,
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
                src: cond,
                label: end,
            }
        );
        self.generate_stmts(stmts);
        self.emit_jump(start);
        self.emit_label(end);
    }

    fn generate_ident(&mut self, sym_id: SymID) -> Var {
        if self.defined_local(sym_id) {
            Var::local(sym_id)
        } else if self.defined_upvalue(sym_id) {
            self.set_upvalue(sym_id);

            Var::upvalue(sym_id)
        } else {
            // this local will be dead, probably can optimize here to keep this info
            Var::local(sym_id)
        }
    }

    fn generate_map(&mut self, pairs: Vec<(MapKey, Spanned<Expr>)>) -> Var {
        let store = self.new_temp();

        self.emit(Tac::NewMap { dest: store });

        for (k, value) in pairs.into_iter() {
            let key = 
            match k {
                MapKey::Sym(sym) => Key::Sym(sym),
                MapKey::Expr(expr) => Key::Var(self.generate_expr(expr))
            };

            let src = self.generate_expr(value);

            // this doesn't need to be spanned since we are sure we are storing into a map this
            // can't fail so we don't need spanning info
            self.emit(
                Tac::KeyStore {
                    store,
                    key,
                    src
                },
            );
        }

        store
    }

    fn generate_list(&mut self, exprs: Vec<Spanned<Expr>>) -> Var {
        let store = self.new_temp();

        self.emit(Tac::NewList { dest: store });

        for (i, e) in exprs.into_iter().enumerate() {
            let src = self.generate_expr(e);

            let key = self.new_temp();
            self.emit(Tac::LoadConst { dest: key, src: TacConst::Int(i as isize)});

            self.emit(
                Tac::KeyStore {
                    store,
                    src,
                    key: Key::Var(key),
                }
            );
        }

        store
    }

    fn generate_binop(&mut self, lhs: Var, op: Op, rhs: Var, span: Span) -> Var {
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

    fn generate_print(&mut self, src: Var) -> Var {
        self.emit(
            Tac::Print {
                src,
            }
        );

        self.new_temp()
    }

    fn generate_read(&mut self) -> Var {
        let temp = self.new_temp();

        self.emit(
            Tac::Read {
                dest: temp,
            }
        );

        temp
    }

    fn load_const(&mut self, tac_const: TacConst) -> Var {
        let temp = self.new_temp();

        self.emit(
            Tac::LoadConst {
                dest: temp,
                src: tac_const
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

    fn new_temp(&mut self) -> Var {
        self.generators.last_mut().as_mut().unwrap().temp_counter += 1;

        Var::temp(self.generators.last().unwrap().temp_counter)
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

        assert!(ctx.generators[0].func.tac[0] == Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(69) });
        assert!(ctx.generators[0].func.spans.is_empty());
    }

    #[test]
    fn load_string_value() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_value(Value::String("test".to_string()));

        assert!(ctx.generators[0].func.tac[0] == Tac::LoadConst { dest: Var::temp(1), src: TacConst::String("test".to_string()) });
    }

    #[test]
    fn load_float_value() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_value(Value::Float(612357.234532));

        assert!(ctx.generators[0].func.tac[0] == Tac::LoadConst { dest: Var::temp(1), src: TacConst::Float(612357.234532) })
    }

    #[test]
    fn emit_read() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_expr(Spanned::new(Expr::Read, (0, 1)));

        assert!(ctx.generators[0].func.tac[0] == Tac::Read { dest: Var::temp(1) });
        assert!(ctx.generators[0].func.spans.is_empty());
    }

    #[test]
    fn expr_stmt_tac() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) }
            ]
        ];
        let input = "1;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_expr_tac() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) },
                Tac::LoadConst { dest: Var::temp(2), src: TacConst::Int(1) },
                Tac::LoadConst { dest: Var::temp(3), src: TacConst::Int(1) },
                Tac::Binop { dest: Var::temp(4), lhs: Var::temp(2), op: Op::Multiply, rhs: Var::temp(3) },
                Tac::Binop { dest: Var::temp(5), lhs: Var::temp(1), op: Op::Multiply, rhs: Var::temp(4) }
            ]
        ];
        let input = "1 * 1 * 1;";

        expect_tac(input, tac);
    }

    #[test]
    fn index_assignment_tac() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) },
                Tac::NewList { dest: Var::temp(2) },
                Tac::LoadConst { dest: Var::temp(3), src: TacConst::Int(1) },
                Tac::LoadConst { dest: Var::temp(4), src: TacConst::Int(0) },
                Tac::KeyStore { store: Var::temp(2), src: Var::temp(3), key: Key::Var(Var::temp(4)) },
                Tac::LoadConst { dest: Var::temp(5), src: TacConst::Int(0) },
                Tac::KeyStore { store: Var::temp(2), key: Key::Var(Var::temp(5)), src: Var::temp(1) },
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
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) },
                Tac::KeyStore { store: Var::local(syms.get_id("a")), key: Key::Sym(syms.get_id("b")), src: Var::temp(1) },
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
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) },
                Tac::Copy { dest: Var::local(syms.get_id("a")), src: Var::temp(1) },
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
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) },
                Tac::Copy { dest: Var::global(syms.get_id("a")), src: Var::temp(1) },
            ]
        ];
        let input = "@a = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn generates_return_with_value() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Bool(true) },
                Tac::Return { src: Var::temp(1) },
            ]
        ];
        let input = "return true;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_return_with_no_value() {
        let tac = vec![
            vec![
                Tac::Return { src: Var::temp(1) },
            ]
        ];
        let input = "return;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_print_expr() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::String("Hello World".to_string()) },
                Tac::Print { src: Var::temp(1) },
            ]
        ];
        let input = "print \"Hello World\";";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_break_and_continue_stmts() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Bool(true) },
                Tac::Label { label: 1 },
                Tac::Jnt { src: Var::temp(1), label: 2 },
                Tac::Jump { label: 1 },
                Tac::Jump { label: 2 },
                Tac::Jump { label: 1 },
                Tac::Label { label: 2 },
            ]
        ];
        let input = "while true { continue; break; }";

        expect_tac(input, tac);
    }

    #[test]
    fn call_multiple_args() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(0) },
                Tac::LoadConst { dest: Var::temp(2), src: TacConst::Int(1) },
                Tac::LoadConst { dest: Var::temp(3), src: TacConst::Int(2) },
                Tac::LoadArg { src: Var::temp(1), },
                Tac::LoadArg { src: Var::temp(2), },
                Tac::LoadArg { src: Var::temp(3), },
                Tac::Call { dest: Var::temp(4), src: Var::local(syms.get_id("a")), },
            ]
        ];
        let input = "a(0, 1, 2);";

        expect_tac_with_syms(input, tac, &mut syms);
    }
}
