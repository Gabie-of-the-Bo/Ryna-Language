use crate::types::Type;
use crate::object::Object;
use crate::context::NessaContext;

use crate::number::*;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

pub type UnaryFunction = fn(&Object) -> Object;
pub type BinaryFunction = fn(&Object, &Object) -> Object;
pub type NaryFunction = fn(&Object, &[&Object]) -> Object;

pub type UnaryOperations = Vec<(Type, Type, UnaryFunction)>;
pub type BinaryOperations = Vec<(Type, Type, BinaryFunction)>;
pub type NaryOperations = Vec<(Type, Type, NaryFunction)>;

#[derive(Clone)]
pub enum Operator {
    Unary {
        id: usize,
        representation: String,
        prefix: bool,
        precedence: usize,
        operations: UnaryOperations
    },

    Binary {
        id: usize,
        representation: String,
        precedence: usize,
        operations: BinaryOperations
    },

    Nary {
        id: usize,
        open_rep: String, // N-ary operators are only allowed to be enclosers, such as the call operator and the multidimensional index operator
        close_rep: String,
        precedence: usize,
        operations: NaryOperations
    }
}

impl Operator {
    pub fn get_id(&self) -> usize {
        return match self {
            Operator::Unary { id, .. } => *id,
            Operator::Binary { id, .. } => *id,
            Operator::Nary { id, .. } => *id
        }
    }

    pub fn get_precedence(&self) -> usize {
        return match self {
            Operator::Unary { precedence: p, .. } => *p,
            Operator::Binary { precedence: p, .. } => *p,
            Operator::Nary { precedence: p, .. } => *p
        }
    }
}

/*
                                                  ╒════════════════╕
    ============================================= │  STANDARD OPS  │ =============================================
                                                  ╘════════════════╛
*/

pub fn standard_unary_operations(ctx: &mut NessaContext) {
    ctx.define_unary_operator("-".into(), true, 100).unwrap();
    ctx.define_unary_operator("!".into(), true, 200).unwrap();
    ctx.define_unary_operator("?".into(), false, 200).unwrap();
    
    ctx.define_unary_operation(0, Type::Basic(0), Type::Basic(0), |a| {
        let n_a = &*a.deref::<Number>();
        let mut res = n_a.clone();

        res.negate();

        return Object::new(res);
    }).unwrap();
}

pub fn standard_binary_operations(ctx: &mut NessaContext) {
    ctx.define_binary_operator("+".into(), 200).unwrap();
    ctx.define_binary_operator("*".into(), 150).unwrap();

    ctx.define_binary_operation(0, Type::Basic(0), Type::Basic(0), Type::Basic(0), |a, b| {
        let n_a = &*a.deref::<Number>();
        let n_b = &*b.deref::<Number>();

        return Object::new(n_a + n_b);
    }).unwrap();

    ctx.define_binary_operation(0, Type::Basic(1), Type::Basic(1), Type::Basic(1), |a, b| {
        let n_a = &*a.deref::<String>();
        let n_b = &*b.deref::<String>();

        return Object::new(format!("{}{}", n_a, n_b));
    }).unwrap();
}

pub fn standard_nary_operations(ctx: &mut NessaContext) {
    ctx.define_nary_operator("(".into(), ")".into(), 50).unwrap();
    ctx.define_nary_operator("[".into(), "]".into(), 50).unwrap();
}