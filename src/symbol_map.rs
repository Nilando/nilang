use std::collections::HashMap;
use sandpit::TraceLeaf;
use serde::Serialize;

#[derive(Copy, Clone, PartialEq, Eq, TraceLeaf, Hash, Serialize)]
pub struct SymID {
    value: usize,
}

impl std::fmt::Debug for SymID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", usize::from(*self))
    }
}

impl From<usize> for SymID {
    fn from(value: usize) -> Self {
        Self {
            value
        }
    }
}

impl From<SymID> for usize {
    fn from(sym: SymID) -> Self {
        sym.value
    }
}

pub struct SymbolMap {
    map: HashMap<String, SymID>,
}

pub const INPUT_SYM_ID: SymID = SymID { value: 0 };
pub const SELF_SYM_ID: SymID = SymID { value: 1 };

impl SymbolMap {
    pub fn new() -> Self {
        let mut map = HashMap::new();

        map.insert("@".to_string(), INPUT_SYM_ID);
        map.insert("self".to_string(), SELF_SYM_ID);

        Self {
            map,
        }
    }

    pub fn get_id(&mut self, str: &str) -> SymID {
        match self.map.get(str) {
            Some(id) => *id,
            None => {
                let id = SymID::from(self.map.len());

                self.map.insert(str.to_string(), id);
                id
            }
        }
    }
}
