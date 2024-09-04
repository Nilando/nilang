use super::generator::{Var, VarID};
use super::raw_value::RawValue;
use super::ir::IR;
use super::ir_func::IRFunc;
use std::collections::HashMap;

impl RawValue {
    fn display(&self, symbol_map: &HashMap<String, usize>) -> String {
        match self {
            Self::String(s) => format!("\"{}\"", s),
            Self::Int(i) => format!("{}", i),
            Self::Bool(b) => format!("{}", b),
            Self::Float(n) => format!("{}", n),
            Self::Null => format!("null"),
            Self::Func(i) => format!("fn({})", i),
            Self::Var(v) => v.display(symbol_map),
        }
    }
}

impl Var {
    fn display(&self, symbol_map: &HashMap<String, usize>) -> String {
        match self.id {
            VarID::Temp(i) => format!("T{}", i),
            VarID::Local(i) => {
                for (k, v) in symbol_map.iter() {
                    if *v == i {
                        return format!("{}", k);
                    }
                }
                panic!("unreachable")
            }
            VarID::Global(i) => {
                for (k, v) in symbol_map.iter() {
                    if *v == i {
                        return format!("{}", k);
                    }
                }
                panic!("unreachable")
            }
        }
    }
}

impl IR {
    pub fn display(&self, symbol_map: &HashMap<String, usize>) -> String {
        match self {
            Self::Load { dest, src } => format!(
                "\t{} = {}",
                dest.display(symbol_map),
                src.display(symbol_map)
            ),
            Self::Return { src } => format!("\treturn {}", src.display(symbol_map)),
            Self::Log { src } => format!("\tlog {}", src.display(symbol_map)),
            Self::Label { id } => format!("LABEL {}", id),
            Self::Jump { label } => format!("\tJump {}", label),
            Self::Jnt { cond, label } => format!(
                "\tJNT {} {}",
                label,
                format!("{}", cond.display(symbol_map))
            ),
            Self::ObjLoad { dest, obj, key } => format!(
                "\t{} = {}[{}]",
                dest.display(symbol_map),
                obj.display(symbol_map),
                key.display(symbol_map)
            ),
            Self::ObjStore { obj, key, val } => format!(
                "\t{}[{}] = {}",
                obj.display(symbol_map),
                key.display(symbol_map),
                val.display(symbol_map)
            ),
            Self::Call { dest, calle, input } => format!(
                "\t{} = {}({})",
                dest.display(symbol_map),
                calle.display(symbol_map),
                input.display(symbol_map)
            ),
            Self::Binop { dest, lhs, op, rhs } => format!(
                "\t{} = {} {} {}",
                dest.display(symbol_map),
                lhs.display(symbol_map),
                op,
                rhs.display(symbol_map)
            ),
            Self::NewList { dest } => format!("\t{} = []", dest.display(symbol_map)),
            Self::NewMap { dest } => format!("\t{} = {{}}", dest.display(symbol_map)),
        }
    }
}

impl IRFunc {
    pub fn display(&self, symbol_map: &HashMap<String, usize>) {
        if self.id == 0 {
            println!("======= MAIN START =======");
        } else {
            println!("======= FUNC {} =======", self.id);
        }

        for (i, stmt) in self.code.iter().enumerate() {
            println!(
                "{}\t{}",
                format!("{:04x} {:04x} {:04x}", i, stmt.span.0, stmt.span.1),
                stmt.val.display(symbol_map)
            );
        }

        if self.id == 0 {
            println!("======= MAIN END =======\n");
        } else {
            println!("======= FUNC {} END =======\n", self.id);
        }
    }
}
