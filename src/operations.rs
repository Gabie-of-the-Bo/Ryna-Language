use std::collections::HashMap;

use crate::number::*;
use crate::object::Object;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

type UnaryFunction = fn(&Object) -> Object;
type BinaryFunction = fn(&Object, &Object) -> Object;

type UnaryOperations = HashMap<usize, UnaryFunction>;
type BinaryOperations = HashMap<(usize, usize), BinaryFunction>;

#[derive(Clone)]
pub struct UnaryOperator {
    pub id: usize,
    pub representation: String,
    pub operations: UnaryOperations
}

impl UnaryOperator {
    pub fn get_unary_op(&self, a: &Object) -> Option<&UnaryFunction> {
        return self.operations.get(&a.get_type());
    }

    pub fn def_unary_op(&mut self, a: usize, f: UnaryFunction) -> bool {
        return self.operations.insert(a, f).is_none();
    }
}

#[derive(Clone)]
pub struct BinaryOperator {
    pub id: usize,
    pub representation: String,
    pub operations: BinaryOperations
}

impl BinaryOperator {
    pub fn get_binary_op(&self, a: &Object, b: &Object) -> Option<&BinaryFunction> {
        return self.operations.get(&(a.get_type(), b.get_type()));
    }

    pub fn def_binary_op(&mut self, a: usize, b: usize, f: BinaryFunction) -> bool {
        return self.operations.insert((a, b), f).is_none();
    }
}

/*
                                                  ╒════════════════╕
    ============================================= │  STANDARD OPS  │ =============================================
                                                  ╘════════════════╛
*/

pub fn standard_unary_operations() -> Vec<UnaryOperator> {
    return vec!();
}

pub fn standard_binary_operations() -> Vec<BinaryOperator> {
    let mut plus = BinaryOperator {
        id: 0,
        representation: "+".into(),
        operations: HashMap::new()
    };

    plus.def_binary_op(0, 0, |a, b| {
        let n_a = &*a.deref::<Number>();
        let n_b = &*b.deref::<Number>();

        return Object::new(n_a + n_b);
    });

    plus.def_binary_op(1, 1, |a, b| {
        let n_a = &*a.deref::<String>();
        let n_b = &*b.deref::<String>();

        return Object::new(format!("{}{}", n_a, n_b));
    });

    return vec!(plus);
}