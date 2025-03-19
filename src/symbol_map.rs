use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SymID(u64);
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SmallSymID(u16);

pub struct SymbolMap {
    map: HashMap<String, SymID>,
}

pub const INPUT_SYM_ID: SymID = SymID(0);
pub const SELF_SYM_ID: SymID = SymID(1);

impl SymbolMap {
    pub fn new() -> Self {
        let mut map = HashMap::new();

        map.insert("@".to_string(), INPUT_SYM_ID);
        map.insert("self".to_string(), SELF_SYM_ID);

        Self { map }
    }

    pub fn get_id(&mut self, str: &str) -> SymID {
        match self.map.get(str) {
            Some(id) => *id,
            None => {
                let id = SymID(self.map.len().try_into().unwrap());

                self.map.insert(str.to_string(), id);

                id
            }
        }
    }
}
