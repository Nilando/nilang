use std::collections::HashMap;
use std::rc::Rc;

pub type SymID = u32;

pub struct SymbolMap {
    str_to_id: HashMap<Rc<String>, SymID>,
    id_to_str: Vec<Rc<String>>,
}

// special symbols
pub const SELF_SYM:     SymID = 0;
pub const LEN_SYM:      SymID = 1;
pub const PUSH_SYM:     SymID = 2;
pub const NUM_SYM:      SymID = 3;
pub const STR_SYM:      SymID = 4;
// pub const FLOAT_SYM:    SymID = 5;
pub const BOOL_SYM:     SymID = 6;
pub const SYM_SYM:      SymID = 7;
pub const ABS_SYM:      SymID = 8;
pub const POW_SYM:      SymID = 9;
pub const LOG_SYM:      SymID = 10;
pub const FLOOR_SYM:    SymID = 11;
pub const CEIL_SYM:     SymID = 12;
pub const SPLIT_SYM:    SymID = 13;
pub const TRIM_SYM:     SymID = 14;
pub const FIND_SYM:     SymID = 15;
pub const CONTAINS_SYM: SymID = 16;
pub const FILTER_SYM:   SymID = 17;
pub const CONCAT_SYM:   SymID = 18;
pub const JOIN_SYM:     SymID = 19;
pub const KEYS_SYM:     SymID = 20;
pub const VALUES_SYM:   SymID = 21;
pub const ENTRIES_SYM:  SymID = 22;
pub const ARGS_SYM:     SymID = 23;
pub const BIND_SYM:     SymID = 24;

impl SymbolMap {
    pub fn new() -> Self {
        let mut this = Self {
            str_to_id: HashMap::new(),
            id_to_str: Vec::new(),
        };

        this.init();

        this
    }

    pub fn get_id(&mut self, str: &str) -> SymID {
        let str = str.to_string();
        match self.str_to_id.get(&str) {
            Some(id) => *id,
            None => self.insert(str),
        }
    }

    pub fn is_intrinsic(sym: SymID) -> bool {
        match sym {
            LEN_SYM
            | NUM_SYM
            | PUSH_SYM => true,
            _ => false
        }
    }

    pub fn get_str(&mut self, id: SymID) -> &str {
        &self.id_to_str[id as usize]
    }

    fn init(&mut self) {
        self.insert("self".to_string());
        self.insert("len".to_string());
        self.insert("push".to_string());
        self.insert("num".to_string());
    }

    fn insert(&mut self, str: String) -> SymID {
        let id = self.id_to_str.len() as u32;
        let s = Rc::new(str);

        self.id_to_str.push(s.clone());
        self.str_to_id.insert(s.clone(), id);

        id
    }
}
