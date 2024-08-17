use std::{cell::{RefCell, RefMut}, hash::Hash};
use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};

use crate::{types::Type, parser::{RynaExpr, ImportType}, patterns::Pattern, config::Imports, compilation::CompiledRynaExpr};

#[derive(Clone, Serialize, Deserialize)]
pub struct Cache<K: Hash + PartialEq + Eq + Clone, V: Clone> {
    inner: RefCell<FxHashMap<K, V>>
}

impl<K: Hash + PartialEq + Eq + Clone, V: Clone> Default for Cache<K, V> {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<K: Hash + PartialEq + Eq + Clone, V: Clone> Cache<K, V> {
    pub fn inner_clone(&self) -> FxHashMap<K, V> {
        return self.inner.borrow().clone();
    }

    pub fn inner_borrow_mut(&self) -> RefMut<FxHashMap<K, V>> {
        self.inner.borrow_mut()
    }

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
type StringCache<V> = Cache<String, V>;
type IdCache = ResultCache<String, usize, String>;
type TemplateCache = Cache<(usize, Vec<Type>, Vec<Type>), Vec<RynaExpr>>;
type OverloadCache = Cache<(usize, Vec<Type>, Vec<Type>), usize>;
type UsageCache = Cache<usize, FxHashSet<(Vec<Type>, Vec<Type>)>>;
type ImportCache<T> = FxHashSet<(String, T)>;
type OpcodeCache = Cache<(usize, usize), (CompiledRynaExpr, usize)>;

//  Concrete functionalities

impl UsageCache {
    pub fn add_new(&self, f_id: usize, args: Vec<Type>, templates: Vec<Type>) -> bool {
        return self.inner.borrow_mut().entry(f_id).or_default().insert((args, templates));
    }
}

// Full Nessa cache

#[allow(clippy::type_complexity)]
#[derive(Default, Clone, Serialize, Deserialize)]
pub struct RynaImportCache {
    pub functions: ImportCache<(usize, Vec<String>, Vec<(String, Type)>, Type)>,
    pub unary: ImportCache<(usize, Vec<String>, Type, Type)>,
    pub binary: ImportCache<(usize, Vec<String>, Type, Type, Type)>,
    pub nary: ImportCache<(usize, Vec<String>, Type, Vec<(String, Type)>, Type)>,
    
    pub classes: ImportCache<(String, Vec<String>)>,

    pub interface_def: ImportCache<(String, Vec<String>)>,
    pub interface_impl: ImportCache<(Vec<String>, Type, String, Vec<Type>)>,

    pub macros: ImportCache<(String, Pattern)>,

    pub lines: ImportCache<usize>
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct RynaDividedCache<T> {
    pub functions: T,
    pub unary: T,
    pub binary: T,
    pub nary: T
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct RynaCache {
    pub class_id: IdCache,
    pub function_id: IdCache,
    pub interface_id: IdCache,
    pub templates: RynaDividedCache<TemplateCache>,
    pub usages: RynaDividedCache<UsageCache>,
    pub overloads: RynaDividedCache<OverloadCache>,
    pub locations: RynaDividedCache<OverloadCache>,
    pub opcodes: RynaDividedCache<OpcodeCache>,
    pub imports: RynaImportCache,
    pub ranges: StringCache<(usize, usize)>
}

pub fn needs_line_import(module: &str, line: usize, cache: &mut ImportCache<usize>) -> bool {    
    cache.insert((module.to_owned(), line))
}

pub fn needs_import<T: Hash + PartialEq + Eq>(module: &str, import_type: ImportType, name: &String, imports: &Imports, cache: &mut ImportCache<T>, obj: T) -> bool {    
    (imports.contains_key(&ImportType::All) || (
        imports.contains_key(&import_type) && 
        (imports[&import_type].contains(name) || imports[&import_type].contains("*"))
    )) && cache.insert((module.to_owned(), obj))
}