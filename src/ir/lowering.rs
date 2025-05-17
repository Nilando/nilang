use crate::parser::{Span, Spanned, Stmt, Expr, Value, Op, MapKey, LhsExpr};
use crate::symbol_map::SymID;
use std::collections::{BTreeSet, HashSet};
use super::func::Func;
use super::func_builder::FuncBuilder;
use super::tac::{
    LabelID,
    Tac,
    TacConst,
    FuncID,
};
use super::VReg;

pub const MAIN_FUNC_ID: usize = 0;

struct LoopCtx {
    start: LabelID,
    end: LabelID,
}

struct FuncLoweringCtx {
    label_counter: usize,
    defined_variables: BTreeSet<SymID>,
    loop_ctxs: Vec<LoopCtx>,
    builder: FuncBuilder,
}

impl FuncLoweringCtx {
    fn new(id: FuncID, defined_variables: BTreeSet<SymID>, pretty_ir: bool) -> Self {
        Self {
            builder: FuncBuilder::new(id, &defined_variables, pretty_ir),
            label_counter: 0,
            defined_variables,
            loop_ctxs: vec![],
        }
    }
}

struct LoweringCtx {
    func_counter: usize,
    funcs: Vec<FuncLoweringCtx>,
    lowered_funcs: Vec<Func>,
    pretty_ir: bool,
}

impl LoweringCtx {
    pub fn new(pretty_ir: bool) -> Self {
        let main_func = FuncLoweringCtx::new(MAIN_FUNC_ID, BTreeSet::new(), pretty_ir);

        Self {
            func_counter: 0,
            funcs: vec![main_func],
            lowered_funcs: vec![],
            pretty_ir
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
        let src =
        if let Some(expr) = return_expr {
            self.generate_expr(expr)
        } else {
            self.new_temp()
        };

        self.emit(Tac::Return { src });
    }

    fn generate_assign(&mut self, lhs_expr: Spanned<LhsExpr>, src: Spanned<Expr>) {
        let copy_flag = 
        if let Expr::Value(Value::Ident(_)) = src.item {
            true
        } else {
            false
        };
        let src = self.generate_expr(src);
        let span = lhs_expr.get_span();

        match lhs_expr.item {
            LhsExpr::Index { store, key } => {
                let store = self.generate_expr(*store);
                let key = self.generate_expr(*key);

                self.generate_key_store(store, key, src, span);
            }
            LhsExpr::Access { store, key } => {
                let store = self.generate_expr(*store);
                let key = self.load_const(TacConst::Sym(key));

                self.generate_key_store(store, key, src, span);
            }
            LhsExpr::Local(sym_id) => {
                self.define_var(sym_id);

                if !copy_flag && self.last_instr_has_dest() {
                  self.update_prev_dest(sym_id);
                } else {
                    let dest = self.sym_to_reg(&sym_id);

                    self.emit(
                        Tac::Copy {
                            dest,
                            src
                        }
                    )
                }
            }
            LhsExpr::Global(sym_id) => {
                let sym = self.load_const(TacConst::Sym(sym_id));

                self.emit(
                    Tac::StoreGlobal { src, sym }
                )
            }
        }
    }

    fn generate_expr(&mut self, spanned_expr: Spanned<Expr>) -> VReg {
        let span = spanned_expr.get_span();
        match spanned_expr.item {
            Expr::Value(value) => self.generate_value(value),
            Expr::Read => self.generate_read(),
            Expr::Print(expr) => {
                let var = self.generate_expr(*expr);

                self.generate_print(var)
            }
            Expr::Binop { lhs, op, rhs } => {
                match op {
                    Op::And | Op::Or => self.generate_shortcircuit(*lhs, op, *rhs),
                    _ => {
                        let v1 = self.generate_expr(*lhs);
                        let v2 = self.generate_expr(*rhs);

                        self.generate_binop(v1, op, v2, span)
                    }
                }
            }
            Expr::Access { store, key } => {
                let var = self.generate_expr(*store);
                let key = self.load_const(TacConst::Sym(key));

                self.generate_key_load(var, key, span)
            }
            Expr::Index { store, key } => {
                let store = self.generate_expr(*store);
                let key = self.generate_expr(*key);

                self.generate_key_load(store, key, span)
            }
            Expr::Call { calle, args } => {
                let args = args.into_iter().map(|a| self.generate_expr(a)).collect();
                let calle = self.generate_expr(*calle);

                self.generate_call(calle, args, span)
            }
        }
    }

    fn generate_shortcircuit(&mut self, lhs: Spanned<Expr>, op: Op, rhs: Spanned<Expr>) -> VReg {
        let label = self.new_label();
        let dest = self.new_temp();
        let src = self.generate_expr(lhs);

        self.emit(
            Tac::Copy {
                dest,
                src
            }
        );

        match op {
            Op::And => 
                self.emit(
                    Tac::Jnt {
                        src,
                        label,
                    }
                ),
            Op::Or => 
                self.emit(
                    Tac::Jit {
                        src,
                        label,
                    }
                ),
            _ => panic!("tried to generated shortcircuit for wrong op"),
        }

        let src = self.generate_expr(rhs);

        self.emit(
            Tac::Copy {
                dest,
                src
            }
        );

        self.emit_label(label);

        dest
    }

    fn generate_value(&mut self, value: Value) -> VReg {
        match value {
            Value::Int(i) => self.load_const(TacConst::Int(i)),
            Value::Null => self.load_const(TacConst::Null),
            Value::Float(f) => self.load_const(TacConst::Float(f)),
            Value::Bool(b) => self.load_const(TacConst::Bool(b)),
            Value::String(s) => self.load_const(TacConst::String(s)),
            Value::Global(sym_id) => self.load_global(sym_id),
            Value::Ident(sym_id) => self.generate_ident(sym_id),
            Value::Map(map) => self.generate_map(map),
            Value::List(list) => self.generate_list(list),
            Value::InlineFunc { inputs, stmts } => self.generate_func(inputs, stmts, None),
        }
    }

    fn load_global(&mut self, sym_id: SymID) -> VReg {
        let sym = self.load_const(TacConst::Sym(sym_id));
        let dest = self.new_temp();

        self.emit(
            Tac::LoadGlobal {
                dest,
                sym,
            }
        );

        dest
    }

    fn generate_call(&mut self, calle: VReg, args: Vec<VReg>, span: Span) -> VReg {
        let callsite = self.new_temp();
        self.emit(
            Tac::PrepCallSite { 
                src: callsite
            }
        );

        for src in args.into_iter() {
            self.emit(
                Tac::StoreArg {
                    src
                }
            )
        }

        let dest = self.new_temp();

        self.emit_spanned(
            Tac::Call {
                dest,
                src: calle,
            },
            span
        );

        dest
    }

    fn generate_key_store(&mut self, store: VReg, key: VReg, src: VReg, span: Span) {
        self.emit_spanned(
            Tac::MemStore {
                store,
                key, 
                src
            },
            span
        );
    }

    fn generate_key_load(&mut self, store: VReg, key: VReg, span: Span) -> VReg {
        let dest = self.new_temp();

        self.emit_spanned(
            Tac::MemLoad {
                dest,
                store,
                key,
            },
            span
        );

        dest
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

        self.generate_func(inputs, stmts, Some(ident));
    }

    fn generate_func(&mut self, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>, ident: Option<SymID>) -> VReg {
        let func = 
        if let Some(sym) = ident {
            self.sym_to_reg(&sym)
        } else {
            self.new_temp()
        };

        let (func_id, upvalues) = self.new_func(inputs, stmts);

        self.emit(
            Tac::LoadConst {
                dest: func,
                src: TacConst::Func(func_id),
            }
        );

        for sym_id in upvalues.iter() {
            let src = self.generate_ident(*sym_id);

            self.emit(
                Tac::StoreUpvalue {
                    func,
                    src 
                }
            );
        }

        func
    }

    fn new_func(&mut self, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) -> (FuncID, HashSet<SymID>) {
        let func_id = self.new_func_id();
        let generator = FuncLoweringCtx::new(func_id, BTreeSet::from_iter(inputs.item), self.pretty_ir);

        self.funcs.push(generator);

        self.generate_stmts(stmts);

        let func_builder = &self.funcs.last().unwrap().builder;
        let upvalues = func_builder.upvalues.clone();

        self.push_current_func();

        (func_id, upvalues)
    }

    fn push_current_func(&mut self) {
        let builder = self.funcs.pop().unwrap().builder;
        let func = builder.build();

        self.lowered_funcs.push(func);
    }

    fn generate_if_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>) {
        let label = self.new_label();
        let src = self.generate_expr(cond);

        self.emit(
            Tac::Jnt {
                src,
                label,
            }
        );
        self.generate_stmts(stmts);
        self.emit_label(label);
    }

    fn generate_if_else_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>, else_stmts: Vec<Stmt>) {
        let else_end = self.new_label();
        let else_start = self.new_label();
        let src = self.generate_expr(cond);

        self.emit(
            Tac::Jnt {
                src,
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

        self.emit_label(start);

        let src = self.generate_expr(cond);

        self.emit(
            Tac::Jnt {
                src,
                label: end,
            }
        );
        self.generate_stmts(stmts);
        self.emit_jump(start);
        self.emit_label(end);
    }


    fn generate_ident(&mut self, sym_id: SymID) -> VReg {
        if self.defined_local(sym_id) {
            self.sym_to_reg(&sym_id)
        } else if self.defined_upvalue(sym_id) {
            let dest = self.sym_to_reg(&sym_id);

            self.set_upvalue(sym_id);

            let upvalue = self.get_current_func_mut().builder.upvalues
                .iter()
                .position(|uv| {
                    *uv == sym_id
            }).unwrap();

            self.emit(Tac::LoadUpvalue { dest, id: u16::try_from(upvalue).unwrap() });
            
            dest
        } else {
            // this is an uninitialized variable
            let dest = self.sym_to_reg(&sym_id);

            self.emit(Tac::LoadConst { dest, src: TacConst::Null });

            dest
        }
    }

    fn generate_map(&mut self, pairs: Vec<(MapKey, Spanned<Expr>)>) -> VReg {
        let store = self.new_temp();

        self.emit(Tac::NewMap { dest: store });

        for (k, value) in pairs.into_iter() {
            let key = 
            match k {
                MapKey::Sym(sym) => self.load_const(TacConst::Sym(sym)),
                MapKey::Expr(expr) => self.generate_expr(expr)
            };
            let src = self.generate_expr(value);

            // this doesn't need to be spanned since we are sure we are storing into a map this
            // can't fail so we don't need spanning info
            self.emit(
                Tac::MemStore {
                    store,
                    key,
                    src,
                },
            );
        }

        store
    }

    fn generate_list(&mut self, exprs: Vec<Spanned<Expr>>) -> VReg {
        let store = self.new_temp();

        self.emit(Tac::NewList { dest: store });

        for (i, e) in exprs.into_iter().enumerate() {
            let src = self.generate_expr(e);
            let key = self.new_temp();

            self.emit(Tac::LoadConst { dest: key, src: TacConst::Int(i as isize)});

            self.emit(
                Tac::MemStore {
                    store,
                    key,
                    src,
                }
            );
        }

        store
    }

    fn generate_binop(&mut self, lhs: VReg, op: Op, rhs: VReg, span: Span) -> VReg {
        let dest = self.new_temp();

        self.emit_spanned(
            Tac::Binop {
                dest,
                lhs,
                op,
                rhs
            },
            span
        );

        dest
    }

    fn generate_print(&mut self, src: VReg) -> VReg {
        self.emit(
            Tac::Print {
                src,
            }
        );

        self.new_temp()
    }

    fn generate_read(&mut self) -> VReg {
        let dest = self.new_temp();

        self.emit(
            Tac::Read {
                dest
            }
        );

        dest
    }

    fn load_const(&mut self, tac_const: TacConst) -> VReg {
        let dest = self.new_temp();

        self.emit(
            Tac::LoadConst {
                dest,
                src: tac_const
            }
        );

        dest
    }

    fn emit_label(&mut self, label: LabelID) {
        self.emit(Tac::Label { label })
    }

    fn emit_jump(&mut self, label: LabelID) {
        self.emit(Tac::Jump { label })
    }

    fn last_instr_has_dest(&self) -> bool {
        self.get_current_func().builder.last_instr().unwrap().dest_reg().is_some()
    }

    fn update_prev_dest(&mut self, sym: SymID) {
        let reg = self.sym_to_reg(&sym);
        let f = self.get_current_func_mut();
        let dest = f.builder.last_instr_mut().unwrap().dest_reg_mut().unwrap();

        *dest = reg;
    }

    fn sym_to_reg(&mut self, sym: &SymID) -> VReg {
        self.funcs.last_mut().unwrap().builder.sym_to_reg(sym)
    }

    fn emit(&mut self, instr: Tac) {
        self.funcs.last_mut().unwrap().builder.push_instr(instr, None);
    }

    fn emit_spanned(&mut self, instr: Tac, span: Span) {
        self.funcs.last_mut().unwrap().builder.push_instr(instr, Some(span));
    }

    fn push_loop_ctx(&mut self, ctx: LoopCtx) {
        self.get_current_func_mut().loop_ctxs.push(ctx);
    }

    fn new_func_id(&mut self) -> FuncID {
        self.func_counter += 1;
        self.func_counter
    }

    fn new_temp(&mut self) -> VReg {
        let f = self.get_current_func_mut();
        f.builder.new_reg()
    }

    fn new_label(&mut self) -> LabelID {
        let f = self.get_current_func_mut();

        f.label_counter += 1;
        f.label_counter
    }

    fn get_loop_start(&mut self) -> LabelID {
        self.get_current_func_mut().loop_ctxs.last().unwrap().start
    }

    fn get_loop_end(&mut self) -> LabelID {
        self.get_current_func_mut().loop_ctxs.last().unwrap().end
    }

    fn define_var(&mut self, sym: SymID) {
        self.get_current_func_mut().defined_variables.insert(sym);
    }

    fn defined_local(&self, sym: SymID) -> bool {
        self.get_current_func().defined_variables.contains(&sym)
    }

    fn defined_upvalue(&self, sym: SymID) -> bool {
        for i in 0..(self.funcs.len() - 1) {
            if self.funcs[i].defined_variables.contains(&sym) {
                return true;
            }
        }

        return false;
    }

    fn set_upvalue(&mut self, sym: SymID) {
        self.get_current_func_mut().builder.upvalues.insert(sym);

    }

    fn generate_stmts(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts.into_iter() {
            self.generate_stmt(stmt);
        }
    }

    fn get_current_func_mut(&mut self) -> &mut FuncLoweringCtx {
        self.funcs.last_mut().unwrap()
    }

    fn get_current_func(&self) -> &FuncLoweringCtx {
        self.funcs.last().unwrap()
    }
}

pub fn lower_ast(stmts: Vec<Stmt>, pretty_ir: bool) -> Vec<Func> {
    let mut ctx = LoweringCtx::new(pretty_ir);

    ctx.generate_stmts(stmts);

    ctx.push_current_func();

    ctx.lowered_funcs
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::parser::parse_program;
    use crate::symbol_map::SymbolMap;
    use super::super::func_builder::tests::instrs_to_func;
    use super::super::func_to_string;
    use pretty_assertions::assert_eq;
    
    fn expect_tac(input: &str, expected_code: Vec<Tac>) {
        let mut syms = SymbolMap::new();

        expect_tac_with_syms(input, expected_code, &mut syms);
    }

    fn expect_tac_with_syms(input: &str, expected_code: Vec<Tac>, syms: &mut SymbolMap) {
        let parse_result = parse_program(input, syms);
        let expected_func = instrs_to_func(expected_code);
        let ast = parse_result.value.unwrap();
        let top_level_func = lower_ast(ast, false).pop().unwrap();

        assert_eq!(func_to_string(&expected_func, syms), func_to_string(&top_level_func, syms));
    }

    #[test]
    fn expr_stmt_tac() {
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Int(1) }
        ];
        let input = "1;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_expr_tac() {
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Int(1) },
            Tac::LoadConst { dest: 1, src: TacConst::Int(1) },
            Tac::LoadConst { dest: 2, src: TacConst::Int(1) },
            Tac::Binop { dest: 3, lhs: 1, op: Op::Multiply, rhs: 2 },
            Tac::Binop { dest: 4, lhs: 0, op: Op::Multiply, rhs: 3 }
        ];
        let input = "1 * 1 * 1;";

        expect_tac(input, tac);
    }

    #[test]
    fn index_assignment_tac() {
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Int(1) },
            Tac::NewList { dest: 1 },
            Tac::LoadConst { dest: 2, src: TacConst::Int(1) },
            Tac::LoadConst { dest: 3, src: TacConst::Int(0) },
            Tac::MemStore { src: 2, store: 1, key: 3 }, 
            Tac::LoadConst { dest: 4, src: TacConst::Int(0) },
            Tac::MemStore { src: 0, store: 1, key: 4 }, 
        ];
        let input = "[1][0] = 1;";

        expect_tac(input, tac);
    }

    #[test]
    fn access_assignment_tac() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Int(1) },
            Tac::LoadConst { dest: 1, src: TacConst::Null },
            Tac::LoadConst { dest: 2, src: TacConst::Sym(syms.get_id("b")) },
            Tac::MemStore { src: 0, store: 1, key: 2 }, 
        ];
        let input = "a.b = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn assign_local() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst { dest: 1, src: TacConst::Int(1) },
        ];
        let input = "a = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn assign_global() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Int(1) },
            Tac::LoadConst { dest: 1, src: TacConst::Sym(syms.get_id("a")) },
            Tac::StoreGlobal { src: 0, sym:1  }
        ];
        let input = "@a = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn generates_return_with_value() {
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Bool(true) },
            Tac::Return { src: 0 },
        ];
        let input = "return true;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_return_with_no_value() {
        let tac = vec![
            Tac::Return { src: 0 },
        ];
        let input = "return;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_print_expr() {
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::String("Hello World".to_string()) },
            Tac::Print { src: 0 },
        ];
        let input = "print(\"Hello World\");";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_break_and_continue_stmts() {
        let tac = vec![
            Tac::Label { label: 1 },
            Tac::LoadConst { dest: 0, src: TacConst::Bool(true) },
            Tac::Jnt { src: 0, label: 2 },
            Tac::Jump { label: 1 },
            Tac::Jump { label: 2 },
            Tac::Jump { label: 1 },
            Tac::Label { label: 2 },
        ];
        let input = "while true { continue; break; }";

        expect_tac(input, tac);
    }

    #[test]
    fn call_multiple_args() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Int(0) },
            Tac::LoadConst { dest: 1, src: TacConst::Int(1) },
            Tac::LoadConst { dest: 2, src: TacConst::Int(2) },
            Tac::LoadConst { dest: 3, src: TacConst::Null },
            Tac::StoreArg { src: 0, },
            Tac::StoreArg { src: 1, },
            Tac::StoreArg { src: 2, },
            Tac::Call { dest: 4, src: 3, },
        ];
        let input = "a(0, 1, 2);";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn use_defined_local() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst { dest: 1, src: TacConst::Int(1) },
            Tac::Copy { dest: 2, src: 1 },
        ];
        let input = "a = 1; b = a;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn simple_if_stmt() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Bool(true) }, 
            Tac::Jnt { label: 1, src: 0 }, 
            Tac::LoadConst { dest: 1, src: TacConst::String(String::from("test")) }, 
            Tac::Print { src: 1 }, 
            Tac::Label { label: 1 }
        ];
        let input = "if true { print(\"test\"); }";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn load_null() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst { dest: 1, src: TacConst::Null }, 
        ];
        let input = "a = null;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn sym_key_load() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Null },
            Tac::LoadConst { dest: 1, src: TacConst::Sym(syms.get_id("c")) }, 
            Tac::MemLoad { dest: 3, store: 0, key: 1 }, 
        ];
        let input = "a = b.c;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn val_key_load() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Null },
            Tac::LoadConst { dest: 1, src: TacConst::Int(0) }, 
            Tac::MemLoad { dest: 3, store: 0, key: 1 }, 
        ];
        let input = "a = b[0];";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn load_global() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst { dest: 0, src: TacConst::Sym(syms.get_id("b")) },
            Tac::LoadGlobal { dest: 2, sym: 0 }
        ];
        let input = "a = @b;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn map_assignment() {
        let mut syms = SymbolMap::new();
        let tac = vec![
              Tac::NewMap { dest: 0 },
              Tac::LoadConst { dest: 1, src: TacConst::Sym(syms.get_id("b")) },
              Tac::LoadConst { dest: 2, src: TacConst::Int(0) },
              Tac::MemStore { store: 0, key: 1, src: 2 },
              Tac::Copy { dest: 3, src: 0 },
        ];
        let input = "a = { b: 0 };";

        expect_tac_with_syms(input, tac, &mut syms);
    }
}
