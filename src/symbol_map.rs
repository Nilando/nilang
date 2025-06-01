use std::collections::HashMap;
use std::rc::Rc;

pub type SymID = u64;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SmallSymID(u16);

pub struct SymbolMap {
    str_to_id: HashMap<Rc<String>, SymID>,
    id_to_str: Vec<Rc<String>>,
}

//pub const LENGTH_SYM: SymID = 0;
//pub const PUSH_SYM: SymID = 1;

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

    pub fn get_str(&mut self, id: SymID) -> &str {
        &self.id_to_str[id as usize]
    }

    fn init(&mut self) {
        self.insert("length".to_string());
        self.insert("push".to_string());
    }

    fn insert(&mut self, str: String) -> SymID {
        let id = self.id_to_str.len() as u64;
        let s = Rc::new(str);

        self.id_to_str.push(s.clone());
        self.str_to_id.insert(s.clone(), id);

        id
    }
}
