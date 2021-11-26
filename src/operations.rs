use crate::types::Type;
use crate::object::*;
use crate::context::NessaContext;

use crate::number::*;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

pub type UnaryFunction = Option<fn(&Vec<Type>, &Type, Object) -> Result<Object, String>>;
pub type BinaryFunction = Option<fn(&Vec<Type>, &Type, Object, Object) -> Result<Object, String>>;
pub type NaryFunction = Option<fn(&Vec<Type>, &Type, Object, Vec<Object>) -> Result<Object, String>>;

pub type UnaryOperations = Vec<(usize, Type, Type, UnaryFunction)>;
pub type BinaryOperations = Vec<(usize, Type, Type, BinaryFunction)>;
pub type NaryOperations = Vec<(usize, Type, Type, NaryFunction)>;

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
        $ctx.define_native_unary_operation($id, 0, $inner_type, $return_type, |_, _, a| {
            let $a = &*a.get::<$unwrap_type>();
            
            return Ok(Object::new($result));
        }).unwrap();
    };
}

macro_rules! define_unary_native_op_deref {
    ($ctx: ident, $id: expr, $inner_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $result: expr) => {
        $ctx.define_native_unary_operation($id, 0, $inner_type, $return_type, |_, _, a| {
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

    ctx.define_native_unary_operation(0, 0, Type::Basic(0), Type::Basic(0), |_, _, a| {
        let n_a = &*a.deref::<Number>();
        let mut res = n_a.clone();

        res.negate();

        return Ok(Object::new(res));
    }).unwrap();

    ctx.define_unary_operator("!".into(), true, 250).unwrap();

    define_unary_native_op_combinations!(ctx, 1, Type::Basic(2), Type::Basic(2), bool, arg, !arg);

    ctx.define_unary_operator("*".into(), true, 155).unwrap();

    ctx.define_native_unary_operation(2, 1, Type::MutRef(Box::new(Type::TemplateParam(0))), Type::TemplateParam(0), |_, _, a| {
        return Ok(Object {
            inner: a.get::<Reference>().inner.clone()
        });
    }).unwrap();

    ctx.define_unary_operator("?".into(), false, 150).unwrap();
}

macro_rules! define_binary_native_op {
    ($ctx: ident, $id: expr, $l_type: expr, $r_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $b: ident, $result: expr) => {
        $ctx.define_native_binary_operation($id, 0, $l_type, $r_type, $return_type, |_, _, a, b| {
            let $a = &*a.get::<$unwrap_type>();
            let $b = &*b.get::<$unwrap_type>();
    
            return Ok(Object::new($result));
        }).unwrap();
    };
}

macro_rules! define_binary_native_op_deref_l {
    ($ctx: ident, $id: expr, $l_type: expr, $r_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $b: ident, $result: expr) => {
        $ctx.define_native_binary_operation($id, 0, $l_type, $r_type, $return_type, |_, _, a, b| {
            let $a = &*a.deref::<$unwrap_type>();
            let $b = &*b.get::<$unwrap_type>();
    
            return Ok(Object::new($result));
        }).unwrap();
    };
}

macro_rules! define_binary_native_op_deref_r {
    ($ctx: ident, $id: expr, $l_type: expr, $r_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $b: ident, $result: expr) => {
        $ctx.define_native_binary_operation($id, 0, $l_type, $r_type, $return_type, |_, _, a, b| {
            let $a = &*a.get::<$unwrap_type>();
            let $b = &*b.deref::<$unwrap_type>();
    
            return Ok(Object::new($result));
        }).unwrap();
    };
}

macro_rules! define_binary_native_op_deref {
    ($ctx: ident, $id: expr, $l_type: expr, $r_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $b: ident, $result: expr) => {
        $ctx.define_native_binary_operation($id, 0, $l_type, $r_type, $return_type, |_, _, a, b| {
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
    define_binary_native_op_combinations!(ctx, 10, Type::Basic(1), Type::Basic(2), String, arg_1, arg_2, arg_1 == arg_2);
    define_binary_native_op_combinations!(ctx, 10, Type::Basic(2), Type::Basic(2), bool, arg_1, arg_2, arg_1 == arg_2);

    ctx.define_binary_operator("!=".into(), 1150).unwrap();

    define_binary_native_op_combinations!(ctx, 11, Type::Basic(0), Type::Basic(2), Number, arg_1, arg_2, arg_1 != arg_2);
    define_binary_native_op_combinations!(ctx, 11, Type::Basic(1), Type::Basic(2), String, arg_1, arg_2, arg_1 != arg_2);
    define_binary_native_op_combinations!(ctx, 11, Type::Basic(2), Type::Basic(2), bool, arg_1, arg_2, arg_1 != arg_2);

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

macro_rules! idx_op_definition {
    ($array_type: expr, $idx_type: expr, $result_type: expr, $ctx: expr, $deref_arr: ident, $deref_idx: ident, $ref_method: ident) => {
        $ctx.define_native_nary_operation(
            1, 1, $array_type, &[$idx_type], $result_type, 
            |_, _, a, v| {
                let arr = &a.$deref_arr::<(Type, Vec<Object>)>().1;
                let idx = &*v[0].$deref_idx::<Number>();
    
                return match idx {
                    Number::Float(f) if f.fract() == 0.0 => Ok(arr[*f as usize].$ref_method()),
                    Number::Float(_) => Err("Unable to index array with a non-integer float".into()),
                    Number::Int(i) if i.limbs.len() == 1 => Ok(arr[i.limbs[0] as usize].$ref_method()),
                    Number::Int(_) => Err("Unable to index array with a number wider than 64 bits".into())
                };
            }
        ).unwrap();
    };
}

pub fn standard_nary_operations(ctx: &mut NessaContext) {
    ctx.define_nary_operator("(".into(), ")".into(), 50).unwrap();
    ctx.define_nary_operator("[".into(), "]".into(), 75).unwrap();

    // Indexing operations on arrays
    idx_op_definition!(
        Type::Template(3, vec!(Type::TemplateParam(0))),
        Type::Basic(0),
        Type::TemplateParam(0),
        ctx, deref, get, clone
    );

    idx_op_definition!(
        Type::MutRef(Box::new(Type::Template(3, vec!(Type::TemplateParam(0))))),
        Type::Basic(0),
        Type::MutRef(Box::new(Type::TemplateParam(0))),
        ctx, deref, get, get_ref_mut_obj
    );

    idx_op_definition!(
        Type::Ref(Box::new(Type::Template(3, vec!(Type::TemplateParam(0))))),
        Type::Basic(0),
        Type::Ref(Box::new(Type::TemplateParam(0))),
        ctx, deref, get, get_ref_obj
    );

    idx_op_definition!(
        Type::Template(3, vec!(Type::TemplateParam(0))),
        Type::MutRef(Box::new(Type::Basic(0))),
        Type::TemplateParam(0),
        ctx, deref, deref, clone
    );

    idx_op_definition!(
        Type::MutRef(Box::new(Type::Template(3, vec!(Type::TemplateParam(0))))),
        Type::MutRef(Box::new(Type::Basic(0))),
        Type::MutRef(Box::new(Type::TemplateParam(0))),
        ctx, deref, deref, get_ref_mut_obj
    );

    idx_op_definition!(
        Type::Ref(Box::new(Type::Template(3, vec!(Type::TemplateParam(0))))),
        Type::MutRef(Box::new(Type::Basic(0))),
        Type::Ref(Box::new(Type::TemplateParam(0))),
        ctx, deref, deref, get_ref_obj
    );

    idx_op_definition!(
        Type::Template(3, vec!(Type::TemplateParam(0))),
        Type::Ref(Box::new(Type::Basic(0))),
        Type::TemplateParam(0),
        ctx, deref, deref, clone
    );

    idx_op_definition!(
        Type::MutRef(Box::new(Type::Template(3, vec!(Type::TemplateParam(0))))),
        Type::Ref(Box::new(Type::Basic(0))),
        Type::MutRef(Box::new(Type::TemplateParam(0))),
        ctx, deref, deref, get_ref_mut_obj
    );

    idx_op_definition!(
        Type::Ref(Box::new(Type::Template(3, vec!(Type::TemplateParam(0))))),
        Type::Ref(Box::new(Type::Basic(0))),
        Type::Ref(Box::new(Type::TemplateParam(0))),
        ctx, deref, deref, get_ref_obj
    );
}