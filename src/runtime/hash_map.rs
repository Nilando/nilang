use crate::symbol_map::SymbolMap;
use std::cell::Cell;

use super::op::equal;
use std::fmt::Debug;

use murmurhash3::murmurhash3_x64_128;
use sandpit::{field, Gc, Mutator, Trace, TraceLeaf};

use super::tagged_value::TaggedValue;
use super::value::Value;

// features of thie HashMap
// - uses power of 2 capacities
// - uses quadratic probing
// - uses murmurhash3 hashing

const MAX_LOAD: f64 = 0.7;
const INIT_CAPACITY: usize = 16;

#[derive(TraceLeaf, Copy, Clone)]
enum EntryStatus {
    Used,
    Free,
    Dead,
}

#[derive(Trace)]
struct Entry<'gc> {
    status: Cell<EntryStatus>,
    key: TaggedValue<'gc>,
    val: TaggedValue<'gc>,
}

impl<'gc> Entry<'gc> {
    pub fn new() -> Self {
        Self {
            status: Cell::new(EntryStatus::Free),
            key: Value::tagged_null(),
            val: Value::tagged_null(),
        }
    }

    pub fn is_free(&self) -> bool {
        matches!(self.status.get(), EntryStatus::Free)
    }

    pub fn is_used(&self) -> bool {
        matches!(self.status.get(), EntryStatus::Used)
    }
}

#[derive(Trace)]
pub struct GcHashMap<'gc> {
    buckets: Gc<'gc, [Entry<'gc>]>,
    entries: Cell<usize>,
}

// have null always map to the 0 index
// that way we can use the null key to represent a tombstone entry

impl<'gc> GcHashMap<'gc> {
    pub fn alloc(mu: &'gc Mutator) -> Gc<'gc, Self> {
        let hash_map = GcHashMap {
            buckets: mu.alloc_array_from_fn(INIT_CAPACITY, |_| Entry::new()),
            entries: Cell::new(0),
        };

        Gc::new(mu, hash_map)
    }

    pub fn insert(
        this: Gc<'gc, Self>,
        key: TaggedValue<'gc>,
        val: TaggedValue<'gc>,
        mu: &'gc Mutator,
    ) {
        if this.get_entry(&key).is_none() {
            this.entries.set(this.entries.get() + 1);

            if this.get_load() > MAX_LOAD {
                Self::grow(this.clone(), mu);
            }
        }

        loop {
            if let Some(idx) = this.get_key_index(&key) {
                this.buckets.write_barrier(mu, |barrier| {
                    let entry = barrier.at(idx);
                    let val_barrier = field!(&entry, Entry, val);
                    let key_barrier = field!(&entry, Entry, key);

                    entry.inner().status.set(EntryStatus::Used);
                    val_barrier.set(val);
                    key_barrier.set(key);
                });

                return;
            } else {
                Self::grow(this.clone(), mu);
            }
        }
    }

    pub fn get(&self, key: &TaggedValue<'gc>) -> Option<TaggedValue<'gc>> {
        self.get_entry(key).map(|entry| entry.val.clone())
    }

    fn get_entry(&self, key: &TaggedValue<'gc>) -> Option<&Entry<'gc>> {
        let idx = self.get_key_index(&key)?;
        let entry = &self.buckets[idx];

        if entry.is_free() {
            return None;
        }

        Some(entry)
    }

    pub fn delete(&self, key: TaggedValue<'gc>) -> Option<TaggedValue<'gc>> {
        let idx = self.get_key_index(&key)?;
        let entry = &self.buckets[idx];

        if entry.is_free() {
            return None;
        }

        let result = Some(entry.val.clone());

        // entry.key.set_null();
        // entry.val.set_null();
        entry.status.set(EntryStatus::Dead);

        result
    }

    fn grow(this: Gc<'gc, Self>, mu: &'gc Mutator) {
        let new_cap = this.buckets.len() * 2;
        let new_buckets = mu.alloc_array_from_fn(new_cap, |_| Entry::new());
        let old_buckets = this.buckets.clone();

        // update to use the newly allocated array
        this.write_barrier(mu, |barrier| {
            let buckets_ptr = field!(barrier, GcHashMap, buckets);
            buckets_ptr.set(new_buckets);
        });

        // insert old used entries into the new array
        for entry in old_buckets.iter() {
            if entry.is_used() {
                Self::insert(this.clone(), entry.key.clone(), entry.val.clone(), mu);
            }
        }
    }

    fn get_capacity(&self) -> usize {
        self.buckets.len()
    }

    fn entries_count(&self) -> usize {
        self.entries.get()
    }

    fn get_load(&self) -> f64 {
        self.entries_count() as f64 / self.get_capacity() as f64
    }

    fn modulo_mask(&self) -> usize {
        self.get_capacity() - 1
    }

    fn get_key_index(&self, key: &TaggedValue<'gc>) -> Option<usize> {
        let hash_value = hash_value(&Value::from(key));
        let mask = self.modulo_mask();
        let mut probe_pos = hash_value & mask;
        let max_probes = self.get_capacity() / 2;
        let mut probes = 0;

        loop {
            let entry = &self.buckets[probe_pos];

            match entry.status.get() {
                EntryStatus::Free => return Some(probe_pos),
                EntryStatus::Used => {
                    let v1 = Value::from(key);
                    let v2 = Value::from(&entry.key);

                    if let Ok(Value::Bool(true)) = equal(v1, v2) {
                        return Some(probe_pos);
                    }
                }
                EntryStatus::Dead => {}
            }

            probe_pos = (hash_value + (probe_pos * probe_pos)) & mask;
            probes += 1;
            if probes > max_probes {
                return None;
            }
        }
    }

    pub fn to_string(&self, syms: &mut SymbolMap) -> String {
        let mut s = String::new();

        s.push('{');

        let mut x = 0;
        for i in 0..self.buckets.len() {
            let entry = &self.buckets[i];
            let k = Value::from(&entry.key);
            let v = Value::from(&entry.val);

            if entry.is_used() {
                x += 1;
                let key_str = format!("{}: ", k.to_string(syms, false));

                let val_str = 
                if x != self.entries_count() {
                    format!("{}, ", v.to_string(syms, false))
                } else {
                    format!("{}", v.to_string(syms, false))
                };

                s.push_str(&key_str);
                s.push_str(&val_str);
            }
        }

        s.push('}');
        s
    }
}

fn hash_value(v: &Value<'_>) -> usize {
    let mut buffer = Vec::new();

    match v {
        Value::Null => buffer.push(0),
        Value::Bool(b) => {
            buffer.push(1);
            buffer.push(*b as u8);
        }
        Value::SymId(id) => {
            buffer.push(2);
            buffer.extend_from_slice(&id.to_ne_bytes());
        }
        Value::Int(i) => {
            buffer.push(3);
            buffer.extend_from_slice(&i.to_ne_bytes());
        }
        Value::Float(f) => {
            buffer.push(4);
            buffer.extend_from_slice(&f.to_ne_bytes());
        }
        Value::List(_) => {
            buffer.push(5);
            todo!()
        }
        Value::Func(_) => {
            buffer.push(6);
            todo!()
        }
        Value::String(vm_str) => {
            buffer.push(7);

            for i in 0..vm_str.len() {
                let b = vm_str.at(i).unwrap() as u8;

                buffer.push(b);
            }
        }
        Value::Closure(_) => {
            buffer.push(8);
            todo!()
        }
        Value::Map(_) => {
            buffer.push(9);
            todo!()
        }
        Value::Partial(_) => {
            buffer.push(10);
            todo!()
        }
    }

    let result = murmurhash3_x64_128(&buffer, 0);

    result.0 as usize ^ result.1 as usize
}

impl Debug for GcHashMap<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for i in 0..self.buckets.len() {
            let entry = &self.buckets[i];
            let k = Value::from(&entry.key);
            let v = Value::from(&entry.val);

            if entry.is_used() {
                if k.is_string() {
                    write!(f, "\"{:?}\": ", k)?;
                } else {
                    write!(f, "{:?}: ", k)?;
                }
                if v.is_string() {
                    write!(f, "\"{:?}\"", v)?;
                } else {
                    write!(f, "{:?}", v)?;
                }
            }
        }
        write!(f, "}}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sandpit::{Arena, Root};

    #[test]
    fn creates_empty_map() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            GcHashMap::alloc(mu);
        });
    }

    #[test]
    fn insert_and_get_key() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let map = GcHashMap::alloc(mu);
            let key = Value::into_tagged(Value::Int(1), mu);
            let val = Value::into_tagged(Value::Bool(true), mu);

            GcHashMap::insert(map.clone(), key.clone(), val, mu);

            let found = map.get(&key).unwrap();

            matches!(Value::from(&found), Value::Bool(true));
        });
    }

    #[test]
    fn insert_and_get_key_sym() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let map = GcHashMap::alloc(mu);
            let key = Value::into_tagged(Value::SymId(1), mu);
            let val = Value::into_tagged(Value::Bool(true), mu);

            GcHashMap::insert(map.clone(), key.clone(), val, mu);

            let found = map.get(&key).unwrap();

            matches!(Value::from(&found), Value::Bool(true));
        });
    }

    #[test]
    fn insert_and_get_key_repeated() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let map = GcHashMap::alloc(mu);
            for i in 0..100 {
                let key = Value::into_tagged(Value::Int(i), mu);
                let val = Value::into_tagged(Value::Int(i), mu);

                GcHashMap::insert(map.clone(), key.clone(), val, mu);
            }

            for i in 0..100 {
                let key = Value::into_tagged(Value::Int(i), mu);
                let found = Value::from(&map.get(&key).unwrap());

                if let Ok(Value::Bool(true)) = equal(found, Value::Int(i)) {
                } else {
                    assert!(false)
                }
            }
        });
    }

    #[test]
    fn overwrite_key() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let map = GcHashMap::alloc(mu);
            let key = Value::into_tagged(Value::Int(1), mu);
            let v1 = Value::into_tagged(Value::Bool(true), mu);

            GcHashMap::insert(map.clone(), key.clone(), v1, mu);

            let found = map.get(&key).unwrap();

            matches!(Value::from(&found), Value::Bool(true));

            let v2 = Value::into_tagged(Value::Bool(false), mu);

            GcHashMap::insert(map.clone(), key.clone(), v2, mu);

            let found = map.get(&key).unwrap();

            matches!(Value::from(&found), Value::Bool(false));
        });
    }
}
