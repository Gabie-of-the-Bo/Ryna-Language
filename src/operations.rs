use crate::types::Type;
use crate::object::Object;

use crate::number::*;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

type UnaryFunction = fn(&Object) -> Object;
type BinaryFunction = fn(&Object, &Object) -> Object;
type NaryFunction = fn(&[&Object]) -> Object;

type UnaryOperations = Vec<(Type, UnaryFunction)>;
type BinaryOperations = Vec<(Type, BinaryFunction)>;
type NaryOperations = Vec<(Type, NaryFunction)>;

#[derive(Clone)]
pub struct UnaryOperator {
    pub id: usize,
    pub representation: String,
    pub operations: UnaryOperations
}

impl UnaryOperator {
    pub fn get_unary_op(&self, a: &Object) -> Option<&UnaryFunction> {
        let a_type = a.get_type();
        
        for (t, op) in &self.operations{
            if a_type.bindable_to(&t) {
                return Some(op);
            }
        }
        
        return None;
    }

    pub fn def_unary_op(&mut self, a: Type, f: UnaryFunction) {
        self.operations.push((a, f));
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
        let args_type = Type::And(vec!(a.get_type(), b.get_type()));

        for (t, op) in &self.operations{
            if args_type.bindable_to(&t) {
                return Some(op);
            }
        }
        
        return None;
    }

    pub fn def_binary_op(&mut self, a: Type, b: Type, f: BinaryFunction) {
        self.operations.push((Type::And(vec!(a, b)), f));
    }
}

#[derive(Clone)]
pub struct NaryOperator {
    pub id: usize,
    pub open_rep: String, // N-ary operators are only allowed to be enclosers, such as the call operator and the multidimensional index operator
    pub close_rep: String,
    pub operations: NaryOperations
}

impl NaryOperator {
    pub fn get_nary_op(&self, from: &Object, args: &[&Object]) -> Option<&NaryFunction> {
        let mut subtypes = vec!(from.get_type());
        subtypes.extend(args.iter().map(|i| i.get_type()));

        let args_type = Type::And(subtypes);

        for (t, op) in &self.operations{
            if args_type.bindable_to(&t) {
                return Some(op);
            }
        }
        
        return None;
    }

    pub fn def_nary_op(&mut self, from: &Object, args: &[&Object], f: NaryFunction) {
        let mut subtypes = vec!(from.get_type());
        subtypes.extend(args.iter().map(|i| i.get_type()));

        self.operations.push((Type::And(subtypes), f));
    }
}

/*
                                                  ╒════════════════╕
    ============================================= │  STANDARD OPS  │ =============================================
                                                  ╘════════════════╛
*/

pub fn standard_unary_operations() -> Vec<UnaryOperator> {
    let mut negate = UnaryOperator {
        id: 0,
        representation: "-".into(),
        operations: vec!()
    };

    negate.def_unary_op(Type::Ref(Box::new(Type::Basic(0))), |a| {
        let n_a = &*a.deref::<Number>();
        let mut res = n_a.clone();

        res.negate();

        return Object::new(res);
    });

    return vec!(negate);
}

pub fn standard_binary_operations() -> Vec<BinaryOperator> {
    let mut plus = BinaryOperator {
        id: 0,
        representation: "+".into(),
        operations: vec!()
    };

    plus.def_binary_op(Type::Ref(Box::new(Type::Basic(0))), Type::Ref(Box::new(Type::Basic(0))), |a, b| {
        let n_a = &*a.deref::<Number>();
        let n_b = &*b.deref::<Number>();

        return Object::new(n_a + n_b);
    });

    plus.def_binary_op(Type::Ref(Box::new(Type::Basic(1))), Type::Ref(Box::new(Type::Basic(1))), |a, b| {
        let n_a = &*a.deref::<String>();
        let n_b = &*b.deref::<String>();

        return Object::new(format!("{}{}", n_a, n_b));
    });

    return vec!(plus);
}

pub fn standard_call_operations() -> Vec<NaryOperator> {
    let call = NaryOperator {
        id: 0,
        open_rep: "(".into(),
        close_rep: ")".into(),
        operations: vec!()
    };

    return vec!(call);
}