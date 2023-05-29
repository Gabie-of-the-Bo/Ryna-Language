use seq_macro::seq;

use crate::number::Number;
use crate::types::*;
use crate::object::*;
use crate::context::NessaContext;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

// Takes type parameters, return type and arguments
pub type FunctionOverload = Option<fn(&Vec<Type>, &Type, Vec<Object>) -> Result<Object, String>>;

pub type FunctionOverloads = Vec<(usize, Type, Type, FunctionOverload)>;

pub struct Function {
    pub id: usize,
    pub name: String,
    pub overloads: FunctionOverloads
}

/*
                                                  ╒══════════════════════╕
    ============================================= │  STANDARD FUNCTIONS  │ =============================================
                                                  ╘══════════════════════╛
*/

macro_rules! define_unary_function {
    ($ctx: ident, $id: expr, $inner_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $deref: ident, $result: expr) => {
        $ctx.define_native_function_overload(
            $id, 0,
            &[$inner_type], 
            $return_type,
            |_, _, v| {
                let $a = v[0].$deref::<$unwrap_type>();
                return Ok(Object::new($result));
            }
        ).unwrap();
    };
}

macro_rules! define_binary_function {
    ($ctx: ident, $id: expr, $inner_type_a: expr, $inner_type_b: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $deref_a: ident, $b: ident, $deref_b: ident, $result: expr) => {
        $ctx.define_native_function_overload(
            $id, 0,
            &[$inner_type_a, $inner_type_b], 
            $return_type,
            |_, _, v| {
                let $a = v[0].$deref_a::<$unwrap_type>();
                let $b = v[1].$deref_b::<$unwrap_type>();

                return Ok(Object::new($result));
            }
        ).unwrap();
    };
}

macro_rules! define_unary_function_overloads {
    ($ctx: ident, $id: expr, $inner_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $result: expr) => {
        define_unary_function!($ctx, $id, $inner_type, $return_type, $unwrap_type, $a, get, $result);
        define_unary_function!($ctx, $id, Type::Ref(Box::new($inner_type)), $return_type, $unwrap_type, $a, deref, $result);
        define_unary_function!($ctx, $id, Type::MutRef(Box::new($inner_type)), $return_type, $unwrap_type, $a, deref, $result);
    };
}

macro_rules! define_binary_function_overloads {
    ($ctx: ident, $id: expr, $inner_type: expr, $return_type: expr, $unwrap_type: ident, $a: ident, $b: ident, $result: expr) => {
        define_binary_function!($ctx, $id, $inner_type, $inner_type, $return_type, $unwrap_type, $a, get, $b, get, $result);
        define_binary_function!($ctx, $id, Type::Ref(Box::new($inner_type)), $inner_type, $return_type, $unwrap_type, $a, deref, $b, get, $result);
        define_binary_function!($ctx, $id, Type::MutRef(Box::new($inner_type)), $inner_type, $return_type, $unwrap_type, $a, deref, $b, get, $result);
        define_binary_function!($ctx, $id, $inner_type, Type::Ref(Box::new($inner_type)), $return_type, $unwrap_type, $a, get, $b, deref, $result);
        define_binary_function!($ctx, $id, $inner_type, Type::MutRef(Box::new($inner_type)), $return_type, $unwrap_type, $a, get, $b, deref, $result);
        define_binary_function!($ctx, $id, Type::Ref(Box::new($inner_type)), Type::Ref(Box::new($inner_type)), $return_type, $unwrap_type, $a, deref, $b, deref, $result);
        define_binary_function!($ctx, $id, Type::MutRef(Box::new($inner_type)), Type::Ref(Box::new($inner_type)), $return_type, $unwrap_type, $a, deref, $b, deref, $result);
        define_binary_function!($ctx, $id, Type::Ref(Box::new($inner_type)), Type::MutRef(Box::new($inner_type)), $return_type, $unwrap_type, $a, deref, $b, deref, $result);
        define_binary_function!($ctx, $id, Type::MutRef(Box::new($inner_type)), Type::MutRef(Box::new($inner_type)), $return_type, $unwrap_type, $a, deref, $b, deref, $result);
    };
}

// Constant identifiers
pub const ITERATOR_FUNC_ID: usize = 5;
pub const NEXT_FUNC_ID: usize = 6;
pub const IS_CONSUMED_FUNC_ID: usize = 7;

pub fn standard_functions(ctx: &mut NessaContext) {
    ctx.define_function("inc".into()).unwrap();

    ctx.define_native_function_overload(0, 0, &[Type::MutRef(Box::new(Type::Basic(0)))], Type::Empty, |_, _, v| { 
        *v[0].get::<Reference>().get_mut::<Number>() += Number::from(1);

        return Ok(Object::empty());
    }).unwrap();

    ctx.define_function("print".into()).unwrap();

    ctx.define_native_function_overload(1, 0, &[Type::Wildcard], Type::Empty, |_, _, v| { 
        print!("{}", v[0].to_string());

        return Ok(Object::empty());
    }).unwrap();

    ctx.define_function("deref".into()).unwrap();

    ctx.define_native_function_overload(2, 1, &[Type::MutRef(Box::new(Type::TemplateParam(0)))], Type::TemplateParam(0), |_, _, v| {
        return Ok(v[0].deref_obj());
    }).unwrap();

    ctx.define_function("arr".into()).unwrap();

    ctx.define_native_function_overload(
        3, 
        1,
        &[], 
        Type::Template(3, vec!(Type::TemplateParam(0))), 
        |t, _, _| Ok(Object::new((t[0].clone(), vec!())))
    ).unwrap();

    ctx.define_function("push".into()).unwrap();

    ctx.define_native_function_overload(
        4, 
        1,
        &[Type::MutRef(Box::new(Type::Template(3, vec!(Type::TemplateParam(0))))), Type::TemplateParam(0)], 
        Type::Empty, 
        |_, _, v| {
            let array = v[0].get::<Reference>();

            array.get_mut::<(Type, Vec<Object>)>().1.push(v[1].clone());

            return Ok(Object::empty());
        }
    ).unwrap();

    ctx.define_function("iterator".into()).unwrap();

    ctx.define_native_function_overload(
        5, 
        1,
        &[Type::MutRef(Box::new(Type::Template(3, vec!(Type::TemplateParam(0)))))], 
        Type::Template(5, vec!(Type::MutRef(Box::new(Type::TemplateParam(0))))), 
        |t, _, v| {
            return Ok(Object::new((Type::MutRef(Box::new(t[0].clone())), v[0].get::<Reference>().clone(), 0)));
        }
    ).unwrap();

    ctx.define_native_function_overload(
        5, 
        1,
        &[Type::Ref(Box::new(Type::Template(3, vec!(Type::TemplateParam(0)))))], 
        Type::Template(5, vec!(Type::Ref(Box::new(Type::TemplateParam(0))))), 
        |t, _, v| {
            return Ok(Object::new((Type::Ref(Box::new(t[0].clone())), v[0].get::<Reference>().clone(), 0)));
        }
    ).unwrap();

    ctx.define_native_function_overload(
        5, 
        1,
        &[Type::Template(3, vec!(Type::TemplateParam(0)))], 
        Type::Template(5, vec!(Type::MutRef(Box::new(Type::TemplateParam(0))))), 
        |t, _, v| {
            return Ok(Object::new((Type::MutRef(Box::new(t[0].clone())), v[0].get_ref(), 0)));
        }
    ).unwrap();

    ctx.define_function("next".into()).unwrap();

    ctx.define_native_function_overload(
        6, 
        1,
        &[Type::MutRef(Box::new(Type::Template(5, vec!(Type::MutRef(Box::new(Type::TemplateParam(0)))))))], 
        Type::TemplateParam(0), 
        |t, _, v| {
            let reference = v[0].get::<Reference>();
            let mut iterator = reference.get_mut::<(Type, Reference, usize)>();

            let item;

            {
                let array = &iterator.1.get::<(Type, Vec<Object>)>().1;

                item = match t[0] {
                    Type::MutRef(_) => array[iterator.2].get_ref_mut_obj(),
                    Type::Ref(_) => array[iterator.2].get_ref_obj(),
                    _ => array[iterator.2].clone(),
                };
            }

            iterator.2 += 1;

            return Ok(item);
        }
    ).unwrap();

    ctx.define_function("is_consumed".into()).unwrap();

    ctx.define_native_function_overload(
        7, 
        0,
        &[Type::MutRef(Box::new(Type::Template(5, vec!(Type::Wildcard))))], 
        Type::Basic(2), 
        |_, _, v| {
            let reference = v[0].get::<Reference>();
            let iterator = reference.get::<(Type, Reference, usize)>();

            return Ok(Object::new(iterator.2 >= iterator.1.get::<(Type, Vec<Object>)>().1.len()));
        }
    ).unwrap();

    ctx.define_function("panic".into()).unwrap();

    ctx.define_native_function_overload(
        8, 
        0,
        &[Type::Basic(1)], 
        Type::Empty, 
        |_, _, v| {
            return Err(v[0].get::<String>().clone());
        }
    ).unwrap();

    ctx.define_function("len".into()).unwrap();

    ctx.define_native_function_overload(
        9, 
        0,
        &[Type::MutRef(Box::new(Type::Template(3, vec!(Type::Wildcard))))], 
        Type::Basic(0), 
        |_, _, v| Ok(Object::new(Number::from(v[0].deref::<(Type, Vec<Object>)>().1.len() as u64)))
    ).unwrap();

    ctx.define_function("sin".into()).unwrap();

    define_unary_function_overloads!(ctx, 10, Type::Basic(0), Type::Basic(0), Number, a, a.sin()?);

    ctx.define_function("cos".into()).unwrap();

    define_unary_function_overloads!(ctx, 11, Type::Basic(0), Type::Basic(0), Number, a, a.cos()?);

    ctx.define_function("tan".into()).unwrap();

    define_unary_function_overloads!(ctx, 12, Type::Basic(0), Type::Basic(0), Number, a, a.tan()?);

    ctx.define_function("fact".into()).unwrap();

    define_unary_function_overloads!(ctx, 13, Type::Basic(0), Type::Basic(0), Number, a, a.fact()?);

    ctx.define_function("ln".into()).unwrap();

    define_unary_function_overloads!(ctx, 14, Type::Basic(0), Type::Basic(0), Number, a, a.ln()?);

    ctx.define_function("exp".into()).unwrap();

    define_unary_function_overloads!(ctx, 15, Type::Basic(0), Type::Basic(0), Number, a, a.exp()?);

    ctx.define_function("floor".into()).unwrap();

    define_unary_function_overloads!(ctx, 16, Type::Basic(0), Type::Basic(0), Number, a, a.floor()?);

    ctx.define_function("ceil".into()).unwrap();

    define_unary_function_overloads!(ctx, 17, Type::Basic(0), Type::Basic(0), Number, a, a.ceil()?);

    ctx.define_function("sqrt".into()).unwrap();

    define_unary_function_overloads!(ctx, 18, Type::Basic(0), Type::Basic(0), Number, a, a.sqrt()?);

    ctx.define_function("rand".into()).unwrap();

    ctx.define_native_function_overload(19, 0, &[], Type::Basic(0), |_, _, _| Ok(Object::new(Number::rand()))).unwrap();

    ctx.define_function("rand_int".into()).unwrap();

    define_binary_function_overloads!(ctx, 20, Type::Basic(0), Type::Basic(0), Number, a, b, Number::rand_int_range(&a, &b)?);

    ctx.define_function("is".into()).unwrap();

    ctx.define_native_function_overload(
        21, 
        1,
        &[Type::Wildcard], 
        Type::Basic(2), 
        |t, _, v| Ok(Object::new(v[0].get_type() == t[0]))
    ).unwrap();

    ctx.define_function("as".into()).unwrap();

    ctx.define_native_function_overload(
        22, 
        1,
        &[Type::Wildcard], 
        Type::TemplateParam(0), 
        |t, _, mut v| {
            let obj = v.pop().unwrap();
            let obj_type = obj.get_type();

            if obj_type != t[0] {
                Err(format!("Invalid type coercion"))

            } else {
                Ok(obj)
            }
        }
    ).unwrap();

    ctx.define_function("println".into()).unwrap();

    ctx.define_native_function_overload(23, 0, &[], Type::Empty, |_, _, _| { 
        println!("");

        return Ok(Object::empty());
    }).unwrap();

    ctx.define_native_function_overload(23, 0, &[Type::Wildcard], Type::Empty, |_, _, v| { 
        println!("{}", v[0].to_string());

        return Ok(Object::empty());
    }).unwrap();

    // Max tuple size is 10 for now
    seq!(I in 0..10 {
        let id = ctx.functions.len();
        ctx.define_function(format!("get_{}", I)).unwrap();

        seq!(J in 2..10 {
            ctx.define_native_function_overload(
                id, 
                J,
                &[Type::MutRef(Box::new(Type::And((0..J).into_iter().map(Type::TemplateParam).collect())))], 
                Type::Ref(Box::new(Type::TemplateParam(I))), 
                |_, _, v| Ok(v[0].deref::<Tuple>().exprs[I].get_ref_obj())
            ).unwrap();
        });
    });
}