use std::sync::{Arc, RwLock};
use std::collections::HashMap;

use std::ops::Add;

use crate::number::*;
use crate::object::Object;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

type Binary_op = fn(&Object, &Object) -> Object;

type Operator_definitions = HashMap<(usize, usize), Binary_op>;

pub struct Operations {
    binary: Operator_definitions
}

impl Operations {
    pub fn get_binary_op(&self, a: &Object, b: &Object) -> Option<&Binary_op> {
        return self.binary.get(&(a.get_type(), b.get_type()));
    }

    pub fn def_binary_op(&mut self, a: usize, b: usize, f: Binary_op) -> bool {
        return self.binary.insert((a, b), f).is_none();
    }
}

/*
                                                  ╒════════════════╕
    ============================================= │  STANDARD OPS  │ =============================================
                                                  ╘════════════════╛
*/

pub fn standard_operations() -> Operations {
    let mut res = Operations {
        binary: HashMap::new() 
    };

    res.def_binary_op(0, 0, |a, b| {
        let n_a = &*a.deref::<Number>();
        let n_b = &*b.deref::<Number>();

        return Object::new(n_a + n_b);
    });

    res.def_binary_op(1, 1, |a, b| {
        let n_a = &*a.deref::<String>();
        let n_b = &*b.deref::<String>();

        return Object::new(format!("{}{}", n_a, n_b));
    });

    return res;
}