use std::{hash::Hash, cell::RefCell};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{types::Type, parser::NessaExpr};

#[derive(Clone)]
pub struct Cache<K: Hash + PartialEq + Eq + Clone, V: Clone> {
    inner: RefCell<FxHashMap<K, V>>
}

impl<K: Hash + PartialEq + Eq + Clone, V: Clone> Default for Cache<K, V> {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<K: Hash + PartialEq + Eq + Clone, V: Clone> Cache<K, V> {
    pub fn get<F: FnMut(K) -> V>(&self, key: K, mut f: F) -> V {
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

    pub fn get_checked(&self, key: &K) -> Option<V> {
        return self.inner.borrow().get(key).cloned();
    }

    pub fn contains(&self, key: &K) -> bool {
        return self.inner.borrow().contains_key(key);
    }

    pub fn insert(&self, key: K, value: V) -> bool {
        return self.inner.borrow_mut().insert(key, value).is_none();
    }

    pub fn invalidate(&self, key: &K) {
        self.inner.borrow_mut().remove(key);
    }
}

// Concrete types

type ResultCache<K, O, E> = Cache<K, Result<O, E>>;
type IdCache = ResultCache<String, usize, String>;
type TemplateCache = Cache<(usize, Vec<Type>, Vec<Type>), Vec<NessaExpr>>;
type OverloadCache = Cache<(usize, Vec<Type>, Vec<Type>), usize>;
type UsageCache = Cache<usize, FxHashSet<(Vec<Type>, Vec<Type>)>>;

//  Concrete functionalities

impl UsageCache {
    pub fn add_new(&self, f_id: usize, args: Vec<Type>, templates: Vec<Type>) -> bool {
        return self.inner.borrow_mut().entry(f_id).or_default().insert((args, templates));
    }
}

// Full Nessa cache

#[derive(Default, Clone)]
pub struct NessaDividedCache<T> {
    pub functions: T,
    pub unary: T,
    pub binary: T,
    pub nary: T
}

#[derive(Default, Clone)]
pub struct NessaCache {
    pub class_id: IdCache,
    pub function_id: IdCache,
    pub interface_id: IdCache,
    pub templates: NessaDividedCache<TemplateCache>,
    pub usages: NessaDividedCache<UsageCache>,
    pub overloads: NessaDividedCache<OverloadCache>,
    pub locations: NessaDividedCache<OverloadCache>
}