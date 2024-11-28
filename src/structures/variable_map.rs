use rustc_hash::{FxHashMap, FxHashSet};

use crate::types::Type;

#[derive(Clone)]
struct VariableContext {
    is_base: bool,
    names: FxHashMap<String, usize>,
    order: Vec<String>,                 // In declaration order
    vars: Vec<(usize, Type)>            // In declaration order
}

impl VariableContext {
    fn new(is_base: bool) -> Self {
        Self { is_base, vars: vec!(), order: vec!(), names: FxHashMap::default() }
    }

    fn contains(&self, name: &String) -> bool {
        self.names.contains_key(name)
    }

    fn get(&self, name: &String) -> Option<&(usize, Type)> {
        match self.names.get(name) {
            Some(idx) => Some(&self.vars[*idx]),
            None => None,
        }
    }

    fn define_var(&mut self, name: String, idx: usize, t: Type) {
        if !self.contains(&name) {
            self.names.entry(name.clone()).or_insert(self.vars.len());
            self.order.push(name);
            self.vars.push((idx, t));
        }
    }
}

#[derive(Clone)]
pub struct VariableMap {
    counter: usize,
    contexts: Vec<VariableContext>
}

impl Default for VariableMap {
    fn default() -> Self {
        Self::new()
    }
}

impl VariableMap {
    pub fn new() -> Self {
        VariableMap { contexts: vec!(), counter: 0 }
    }

    pub fn depth(&self) -> usize {
        self.contexts.len()
    }

    pub fn add_context(&mut self, is_base: bool) {
        self.contexts.push(VariableContext::new(is_base));
    } 

    pub fn remove_context(&mut self) {
        self.contexts.pop().unwrap();
    }

    pub fn define_var(&mut self, name: String, idx: usize, t: Type) -> usize {
        self.contexts.last_mut().unwrap().define_var(name, idx, t);

        self.contexts.len() - 1 // Will never underflow, otherwise it would panic in the previous line
    }

    pub fn count_up(&mut self) -> usize {
        self.counter += 1;
        self.counter
    }

    pub fn is_var_defined(&mut self, name: &String) -> bool {
        for ctx in self.contexts.iter().rev() {
            if ctx.contains(name) {
                return true;
            }
        }

        false
    }

    pub fn is_var_defined_in_last_ctx(&mut self, name: &String) -> bool {
        self.contexts.last().unwrap().contains(name)
    }

    pub fn get_var(&self, name: &String) -> Option<(usize, &(usize, Type))> {
        for (i, ctx) in self.contexts.iter().enumerate().rev() {
            if let Some(v) = ctx.get(name) {
                return Some((i, v));
            }
        }

        None
    }

    pub fn num_vars(&self) -> usize {
        self.contexts.iter().map(|i| i.vars.len()).sum()
    }

    pub fn for_each_last_ctx<T: FnMut(usize, &String, &Type)>(&self, mut f: T) {
        let last_ctx = self.contexts.last().unwrap();

        for n in last_ctx.order.iter().rev() {
            let (i, t) = &last_ctx.vars[*last_ctx.names.get(n).unwrap()];
            f(*i, n, t);
        }
    }

    pub fn for_each_until_base<T: FnMut(usize, &String, &Type)>(&self, mut f: T) {
        for ctx in self.contexts.iter().rev() {
            for n in ctx.order.iter().rev() {
                let (i, t) = &ctx.vars[*ctx.names.get(n).unwrap()];
                f(*i, n, t);
            }

            if ctx.is_base {
                break;
            }
        }
    }

    pub fn var_names(&self) -> FxHashSet<&String> {
        self.contexts.iter().rev().flat_map(|i| i.names.keys()).collect()
    }
}