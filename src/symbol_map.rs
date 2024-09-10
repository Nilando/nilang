use std::collections::HashMap;
use sandpit::TraceLeaf;

const MAX_SYM_ID: usize = 0xFFFFFF;

#[derive(Copy, Clone, PartialEq, Eq, Debug, TraceLeaf, Hash)]
pub struct SymID {
    bytes: [u8; 3]
}

impl From<usize> for SymID {
    fn from(value: usize) -> Self {
        if MAX_SYM_ID < value {
            panic!("SYM ID OVERFLOW")
        } 

        let [b1, b2, b3, ..] = value.to_le_bytes();

        Self {
            bytes: [b1, b2, b3]
        }
    }
}

impl From<SymID> for u32 {
    fn from(sym_id: SymID) -> Self {
        u32::from_le_bytes([sym_id.bytes[0], sym_id.bytes[1], sym_id.bytes[2], 0])
    }
}

pub struct SymbolMap {
    map: HashMap<String, SymID>,
}

pub const INPUT_SYM_ID: SymID = SymID { bytes: [0, 0, 0] };

impl SymbolMap {
    pub fn new() -> Self {
        let mut map = HashMap::new();

        map.insert("@".to_string(), SymID::from(0));

        Self {
            map,
        }
    }

    pub fn get_id(&mut self, str: &String) -> SymID {
        match self.map.get(str) {
            Some(id) => *id,
            None => {
                let id = SymID::from(self.map.len());

                self.map.insert(str.clone(), id);
                id
            }
        }
    }

}
