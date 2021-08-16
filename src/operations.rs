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
pub struct UnaryOperator {
    pub id: usize,
    pub representation: String,
    pub prefix: bool,
    pub operations: UnaryOperations
}

#[derive(Clone)]
pub struct BinaryOperator {
    pub id: usize,
    pub representation: String,
    pub operations: BinaryOperations
}

#[derive(Clone)]
pub struct NaryOperator {
    pub id: usize,
    pub open_rep: String, // N-ary operators are only allowed to be enclosers, such as the call operator and the multidimensional index operator
    pub close_rep: String,
    pub operations: NaryOperations
}

/*
                                                  ╒════════════════╕
    ============================================= │  STANDARD OPS  │ =============================================
                                                  ╘════════════════╛
*/

pub fn standard_unary_operations(ctx: &mut NessaContext) {
    ctx.define_unary_operator("-".into()).unwrap();
    
    ctx.define_unary_operation(0, Type::Basic(0), Type::Basic(0), |a| {
        let n_a = &*a.deref::<Number>();
        let mut res = n_a.clone();

        res.negate();

        return Object::new(res);
    }).unwrap();
}

pub fn standard_binary_operations(ctx: &mut NessaContext) {
    ctx.define_binary_operator("+".into()).unwrap();

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
    ctx.define_nary_operator("(".into(), ")".into()).unwrap();
}