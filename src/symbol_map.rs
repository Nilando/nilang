use std::collections::HashMap;
use sandpit::TraceLeaf;
use serde::Serialize;

#[derive(Debug, Copy, Clone, PartialEq, Eq, TraceLeaf, Hash, Serialize)]
pub struct SymID(u64);
#[derive(Debug, Copy, Clone, PartialEq, Eq, TraceLeaf, Hash, Serialize)]
pub struct SmallSymID(u16);

pub struct SymbolMap<'a> {
    map: HashMap<&'a str, SymID>,
}

pub const INPUT_SYM_ID: SymID = SymID(0);
pub const SELF_SYM_ID: SymID = SymID(1);

impl<'a> SymbolMap<'a> {
    pub fn new() -> Self {
        let mut map = HashMap::new();

        map.insert("@", INPUT_SYM_ID);
        map.insert("self", SELF_SYM_ID);

        Self {
            map,
        }
    }

    pub fn get_id(&mut self, str: &'a str) -> SymID {
        match self.map.get(str) {
            Some(id) => *id,
            None => {
                let id = SymID(self.map.len().try_into().unwrap());

                self.map.insert(str, id);

                id
            }
        }
    }
}
