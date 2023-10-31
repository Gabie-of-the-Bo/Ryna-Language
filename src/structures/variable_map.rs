use rustc_hash::{FxHashMap, FxHashSet};

use crate::types::Type;

pub struct VariableMap {
    vars: Vec<FxHashMap<String, (usize, Type)>>
}

impl VariableMap {
    pub fn new() -> Self {
        VariableMap { vars: vec!(FxHashMap::default()) }
    }

    pub fn add_context(&mut self) {
        self.vars.push(FxHashMap::default());
    } 

    pub fn remove_context(&mut self) {
        self.vars.pop().unwrap();
    }

    pub fn define_var(&mut self, name: String, idx: usize, t: Type) {
        self.vars.last_mut().unwrap().entry(name).or_insert((idx, t));
    }

    pub fn is_var_defined(&mut self, name: &String) -> bool {
        for ctx in self.vars.iter().rev() {
            if ctx.contains_key(name) {
                return true;
            }
        }

        false
    }

    pub fn is_var_defined_in_last_ctx(&mut self, name: &String) -> bool {
        self.vars.last().unwrap().contains_key(name)
    }

    pub fn get_var(&mut self, name: &String) -> Option<&(usize, Type)> {
        for ctx in self.vars.iter().rev() {
            if let Some(v) = ctx.get(name) {
                return Some(v);
            }
        }

        None
    }

    pub fn for_each_last_ctx<T: FnMut(usize) -> ()>(&self, f: T) {
        self.vars.last().unwrap().values().map(|(i, _)| *i).for_each(f);
    }

    pub fn var_names(&self) -> FxHashSet<&String> {
        self.vars.iter().rev().flat_map(|i| i.keys()).collect()
    }
}