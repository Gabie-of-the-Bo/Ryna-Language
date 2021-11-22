use crate::types::Type;
use crate::object::Object;
use crate::context::NessaContext;

use crate::number::*;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

pub type UnaryFunction = Option<fn(&Object) -> Result<Object, String>>;
pub type BinaryFunction = Option<fn(&Object, &Object) -> Result<Object, String>>;
pub type NaryFunction = Option<fn(&Object, &[&Object]) -> Result<Object, String>>;

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

macro_rules! define_unary_native_op {
    ($ctx: ident, $id: expr, $inner_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $result: expr) => {
        $ctx.define_native_unary_operation($id, $inner_type, $return_type, |a| {
            let $a = &*a.get::<$unwrap_type>();
            
            return Ok(Object::new($result));
        }).unwrap();
    };
}

macro_rules! define_unary_native_op_deref {
    ($ctx: ident, $id: expr, $inner_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $result: expr) => {
        $ctx.define_native_unary_operation($id, $inner_type, $return_type, |a| {
            let $a = &*a.deref::<$unwrap_type>();
            
            return Ok(Object::new($result));
        }).unwrap();
    };
}

macro_rules! define_unary_native_op_combinations {
    ($ctx: ident, $id: expr, $inner_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $result: expr) => {
        let base_ref = Type::Ref(Box::new($inner_type.clone()));
        let base_mut = Type::MutRef(Box::new($inner_type.clone()));
        
        define_unary_native_op!($ctx, $id, $inner_type, $return_type, $unwrap_type, $a, $result);
        define_unary_native_op_deref!($ctx, $id, base_ref.clone(), $return_type, $unwrap_type, $a, $result);
        define_unary_native_op_deref!($ctx, $id, base_mut.clone(), $return_type, $unwrap_type, $a, $result);
    };
}

pub fn standard_unary_operations(ctx: &mut NessaContext) {
    ctx.define_unary_operator("-".into(), true, 300).unwrap();

    ctx.define_native_unary_operation(0, Type::Basic(0), Type::Basic(0), |a| {
        let n_a = &*a.deref::<Number>();
        let mut res = n_a.clone();

        res.negate();

        return Ok(Object::new(res));
    }).unwrap();

    ctx.define_unary_operator("!".into(), true, 250).unwrap();

    define_unary_native_op_combinations!(ctx, 1, Type::Basic(2), Type::Basic(2), bool, arg, !arg);

    ctx.define_unary_operator("?".into(), false, 150).unwrap();
}

macro_rules! define_binary_native_op {
    ($ctx: ident, $id: expr, $l_type: expr, $r_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $b: ident, $result: expr) => {
        $ctx.define_native_binary_operation($id, $l_type, $r_type, $return_type, |a, b| {
            let $a = &*a.get::<$unwrap_type>();
            let $b = &*b.get::<$unwrap_type>();
    
            return Ok(Object::new($result));
        }).unwrap();
    };
}

macro_rules! define_binary_native_op_deref_l {
    ($ctx: ident, $id: expr, $l_type: expr, $r_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $b: ident, $result: expr) => {
        $ctx.define_native_binary_operation($id, $l_type, $r_type, $return_type, |a, b| {
            let $a = &*a.deref::<$unwrap_type>();
            let $b = &*b.get::<$unwrap_type>();
    
            return Ok(Object::new($result));
        }).unwrap();
    };
}

macro_rules! define_binary_native_op_deref_r {
    ($ctx: ident, $id: expr, $l_type: expr, $r_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $b: ident, $result: expr) => {
        $ctx.define_native_binary_operation($id, $l_type, $r_type, $return_type, |a, b| {
            let $a = &*a.get::<$unwrap_type>();
            let $b = &*b.deref::<$unwrap_type>();
    
            return Ok(Object::new($result));
        }).unwrap();
    };
}

macro_rules! define_binary_native_op_deref {
    ($ctx: ident, $id: expr, $l_type: expr, $r_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $b: ident, $result: expr) => {
        $ctx.define_native_binary_operation($id, $l_type, $r_type, $return_type, |a, b| {
            let $a = &*a.deref::<$unwrap_type>();
            let $b = &*b.deref::<$unwrap_type>();
    
            return Ok(Object::new($result));
        }).unwrap();
    };
}

macro_rules! define_binary_native_op_combinations {
    ($ctx: ident, $id: expr, $base_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $b: ident, $result: expr) => {
        let base_ref = Type::Ref(Box::new($base_type.clone()));
        let base_mut = Type::MutRef(Box::new($base_type.clone()));
        
        define_binary_native_op!($ctx, $id, $base_type, $base_type, $return_type, $unwrap_type, $a, $b, $result);
        define_binary_native_op_deref_l!($ctx, $id, base_ref.clone(), $base_type, $return_type, $unwrap_type, $a, $b, $result);
        define_binary_native_op_deref_r!($ctx, $id, $base_type, base_ref.clone(), $return_type, $unwrap_type, $a, $b, $result);
        define_binary_native_op_deref_l!($ctx, $id, base_mut.clone(), $base_type, $return_type, $unwrap_type, $a, $b, $result);
        define_binary_native_op_deref_r!($ctx, $id, $base_type, base_mut.clone(), $return_type, $unwrap_type, $a, $b, $result);
        define_binary_native_op_deref!($ctx, $id, base_ref.clone(), base_ref.clone(), $return_type, $unwrap_type, $a, $b, $result);
        define_binary_native_op_deref!($ctx, $id, base_ref.clone(), base_mut.clone(), $return_type, $unwrap_type, $a, $b, $result);
        define_binary_native_op_deref!($ctx, $id, base_mut.clone(), base_mut.clone(), $return_type, $unwrap_type, $a, $b, $result);
        define_binary_native_op_deref!($ctx, $id, base_mut.clone(), base_ref.clone(), $return_type, $unwrap_type, $a, $b, $result);
    };
}

// Constant identifiers
pub const DOT_BINOP_ID: usize = 5;
pub const LT_BINOP_ID: usize = 6;

pub fn standard_binary_operations(ctx: &mut NessaContext) {
    
    /*
        ╒═════════════════════════════╕
        │ Basic arithmetic operations │
        ╘═════════════════════════════╛
    */

    ctx.define_binary_operator("+".into(), 650).unwrap();

    define_binary_native_op_combinations!(ctx, 0, Type::Basic(0), Type::Basic(0), Number, arg_1, arg_2, arg_1 + arg_2);
    define_binary_native_op_combinations!(ctx, 0, Type::Basic(1), Type::Basic(1), String, arg_1, arg_2, format!("{}{}", arg_1, arg_2));

    ctx.define_binary_operator("-".into(), 700).unwrap();

    define_binary_native_op_combinations!(ctx, 1, Type::Basic(0), Type::Basic(0), Number, arg_1, arg_2, arg_1 - arg_2);

    ctx.define_binary_operator("*".into(), 500).unwrap();

    define_binary_native_op_combinations!(ctx, 2, Type::Basic(0), Type::Basic(0), Number, arg_1, arg_2, arg_1 * arg_2);

    ctx.define_binary_operator("/".into(), 550).unwrap();

    define_binary_native_op_combinations!(ctx, 3, Type::Basic(0), Type::Basic(0), Number, arg_1, arg_2, arg_1 / arg_2);

    ctx.define_binary_operator("%".into(), 600).unwrap();

    define_binary_native_op_combinations!(ctx, 4, Type::Basic(0), Type::Basic(0), Number, arg_1, arg_2, arg_1 % arg_2);

    /*
        ╒══════════════════════╕
        │ Ancillary operations │
        ╘══════════════════════╛
    */

    ctx.define_binary_operator(".".into(), 100).unwrap();

    /*
        ╒═══════════════════════╕
        │ Comparison operations │
        ╘═══════════════════════╛
    */

    ctx.define_binary_operator("<".into(), 900).unwrap();

    define_binary_native_op_combinations!(ctx, 6, Type::Basic(0), Type::Basic(2), Number, arg_1, arg_2, arg_1 < arg_2);

    ctx.define_binary_operator(">".into(), 950).unwrap();

    define_binary_native_op_combinations!(ctx, 7, Type::Basic(0), Type::Basic(2), Number, arg_1, arg_2, arg_1 > arg_2);

    ctx.define_binary_operator("<=".into(), 1000).unwrap();

    define_binary_native_op_combinations!(ctx, 8, Type::Basic(0), Type::Basic(2), Number, arg_1, arg_2, arg_1 <= arg_2);

    ctx.define_binary_operator(">=".into(), 1050).unwrap();

    define_binary_native_op_combinations!(ctx, 9, Type::Basic(0), Type::Basic(2), Number, arg_1, arg_2, arg_1 >= arg_2);

    ctx.define_binary_operator("==".into(), 1100).unwrap();

    define_binary_native_op_combinations!(ctx, 10, Type::Basic(0), Type::Basic(2), Number, arg_1, arg_2, arg_1 == arg_2);

    ctx.define_binary_operator("!=".into(), 1150).unwrap();

    define_binary_native_op_combinations!(ctx, 11, Type::Basic(0), Type::Basic(2), Number, arg_1, arg_2, arg_1 != arg_2);

    /*
        ╒════════════════════╕
        │ Logical operations │
        ╘════════════════════╛
    */

    ctx.define_binary_operator("||".into(), 1500).unwrap();

    define_binary_native_op_combinations!(ctx, 12, Type::Basic(2), Type::Basic(2), bool, arg_1, arg_2, *arg_1 || *arg_2);

    ctx.define_binary_operator("&&".into(), 1550).unwrap();

    define_binary_native_op_combinations!(ctx, 13, Type::Basic(2), Type::Basic(2), bool, arg_1, arg_2, *arg_1 && *arg_2);
}

pub fn standard_nary_operations(ctx: &mut NessaContext) {
    ctx.define_nary_operator("(".into(), ")".into(), 50).unwrap();
    ctx.define_nary_operator("[".into(), "]".into(), 75).unwrap();
}