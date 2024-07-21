use std::collections::HashMap;
use super::generator::{IRValue, IRVar, IRCode, IRFunc};

impl IRValue {
    fn display(&self, symbol_map: &HashMap<String, usize>) -> String {
        match self {
            Self::String(s) => format!("\"{}\"", s),
            Self::Int(i)    => format!("{}", i),
            Self::Bool(b)   => format!("{}", b),
            Self::Float(n)  => format!("{}", n),
            Self::Null      => format!("null"),
            Self::Func(i)   => format!("fn({})", i),
            Self::Var(v)    => v.display(symbol_map),
        }
    }
}

impl IRVar {
    fn display(&self, symbol_map: &HashMap<String, usize>) -> String {
        match self {
            IRVar::Temp(i)   => format!("T{}", i),
            IRVar::Ident(i)  => {
                for (k, v) in symbol_map.iter() {
                    if v == i {
                        return format!("{}", k);
                    }
                }
                panic!("unreachable")
            }
            IRVar::Global(i)  => {
                for (k, v) in symbol_map.iter() {
                    if v == i {
                        return format!("{}", k);
                    }
                }
                panic!("unreachable")
            }
        }
    }
}

impl IRCode {
    pub fn display(&self, symbol_map: &HashMap<String, usize>) -> String {
        match self {
            Self::Load { dest, src }  => format!("\t{} = {}"  , dest.display(symbol_map), src.display(symbol_map)),
            Self::Return { dest }     => format!("\treturn {}", dest.display(symbol_map)),
            Self::Log { dest }     => format!("\tlog {}"      ,dest.display(symbol_map)),
            Self::Label { id }        => format!("LABEL {}"   , id),
            Self::Jump { label }      => format!("\tJump {}"  , label),
            Self::Jnt { cond, label } => format!("\tJNT {} {}", label, format!("{}", cond.display(symbol_map))),
            Self::ObjLoad { dest, store, key } => format!("\t{} = {}[{}]"  , dest.display(symbol_map) , store.display(symbol_map), key.display(symbol_map)),
            Self::ObjStore { store, key, src } => format!("\t{}[{}] = {}"  , store.display(symbol_map), key.display(symbol_map)  , src.display(symbol_map)),
            Self::Call { dest, src, input }    => format!("\t{} = {}({})"  , dest.display(symbol_map) , src.display(symbol_map)  , input.display(symbol_map)),
            Self::Binop { dest, lhs, op, rhs } => format!("\t{} = {} {} {}", dest.display(symbol_map) , lhs.display(symbol_map)  , op, rhs.display(symbol_map)),
            Self::NewList { dest, items } => {
                let mut body = String::new();
                for item in items.iter() {
                    body.push_str(&format!("\t\t\t\t{},\n", item.display(symbol_map)));
                }

                format!("\t{} = [\n{}\t\t\t]", dest.display(symbol_map), body)
            }
            Self::NewMap { dest, items } => {
                let mut body = String::new();
                for (key, value) in items.iter() {
                    body.push_str(&format!("\t\t\t\t{}: {},\n", key.display(symbol_map), value.display(symbol_map)));
                }

                format!("\t{} = {{\n{}\t\t\t}}", dest.display(symbol_map), body)
            }
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
            println!("{}\t{}", format!("{:04x} {:04x} {:04x}", i, stmt.span.0, stmt.span.1), stmt.val.display(symbol_map));
        }

        if self.id == 0 {
            println!("======= MAIN END =======\n");
        } else {
            println!("======= FUNC {} END =======\n", self.id);
        }
    }
}
