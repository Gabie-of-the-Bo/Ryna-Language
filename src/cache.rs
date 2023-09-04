use std::{hash::Hash, cell::RefCell};
use rustc_hash::FxHashMap;

#[derive(Clone)]
pub struct Cache<K: Hash + PartialEq + Eq + Clone, V: Clone> {
    pub inner: RefCell<FxHashMap<K, V>>
}

impl<K: Hash + PartialEq + Eq + Clone, V: Clone> Default for Cache<K, V> {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<K: Hash + PartialEq + Eq + Clone, V: Clone> Cache<K, V> {
    pub fn get<F: Fn(K) -> V>(&self, key: K, f: F) -> V {
        let contents = self.inner.borrow_mut().get(&key).cloned();

        return match contents {
            Some(v) => v,
            None => {
                let v = f(key.clone());
                self.inner.borrow_mut().insert(key, v.clone());
                v
            },
        };
    }

    pub fn insert(&self, key: K, value: V) {
        self.inner.borrow_mut().insert(key, value);
    }

    pub fn invalidate(&self, key: &K) {
        self.inner.borrow_mut().remove(key);
    }
}

// Concrete types

type ResultCache<K, O, E> = Cache<K, Result<O, E>>;
type IdCache = ResultCache<String, usize, String>;

// Full Nessa cache

#[derive(Default, Clone)]
pub struct NessaCache {
    pub class_id: IdCache,
    pub function_id: IdCache,
    pub interface_id: IdCache,
}