use seq_macro::seq;

use crate::ARR_IT_OF;
use crate::ARR_OF;
use crate::math::rand_f64;
use crate::number::Integer;
use crate::types::*;
use crate::object::*;
use crate::context::NessaContext;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

// Takes type parameters, return type and arguments
pub type FunctionOverload = Option<fn(&Vec<Type>, &Type, Vec<Object>, &NessaContext) -> Result<Object, String>>;

pub type FunctionOverloads = Vec<(usize, Type, Type, FunctionOverload)>;

#[derive(Clone)]
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
            |_, _, v, _| {
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
            |_, _, v, _| {
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
pub const ITERATOR_FUNC_ID: usize = 7;
pub const NEXT_FUNC_ID: usize = 8;
pub const IS_CONSUMED_FUNC_ID: usize = 9;

pub fn standard_functions(ctx: &mut NessaContext) {
    let idx = ctx.define_function("inc".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT.to_mut()], Type::Empty, |_, _, v, _| { 
        *v[0].deref::<Integer>() += Integer::from(1);

        return Ok(Object::empty());
    }).unwrap();

    let idx = ctx.define_function("print".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].get::<Integer>().to_string());

        return Ok(Object::empty());
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FLOAT], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].get::<f64>().to_string());

        return Ok(Object::empty());
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[BOOL], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].get::<bool>().to_string());

        return Ok(Object::empty());
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].get::<String>());

        return Ok(Object::empty());
    }).unwrap();
    
    let idx = ctx.define_function("deref".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut()], T_0, |_, _, v, _| {
        return Ok(v[0].deref_obj());
    }).unwrap();

    let idx = ctx.define_function("ref".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0], T_0.to_ref(), |_, _, v, _| {
        return Ok(v[0].get_ref());
    }).unwrap();

    let idx = ctx.define_function("mut".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0], T_0.to_mut(), |_, _, v, _| {
        return Ok(v[0].get_mut());
    }).unwrap();

    let idx = ctx.define_function("arr".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[], 
        ARR_OF!(T_0), 
        |t, _, _, _| Ok(Object::arr(vec!(), t[0].clone()))
    ).unwrap();

    let idx = ctx.define_function("push".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_mut(), T_0], 
        Type::Empty, 
        |_, _, v, _| {
            let mut array = v[0].deref::<NessaArray>();
            array.elements.push(v[1].clone());

            return Ok(Object::empty());
        }
    ).unwrap();

    let idx = ctx.define_function("iterator".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_mut()], 
        ARR_IT_OF!(T_0.to_mut()), 
        |t, _, v, _| {
            return Ok(Object::arr_it(
                Type::MutRef(Box::new(t[0].clone())), 
                v[0].inner.borrow().deref().clone(), 
                0
            ));
        }
    ).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_ref()], 
        ARR_IT_OF!(T_0.to_ref()), 
        |t, _, v, _| {
            return Ok(Object::arr_it(
                Type::Ref(Box::new(t[0].clone())), 
                v[0].inner.borrow().deref().clone(), 
                0
            ));
        }
    ).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0)], 
        ARR_IT_OF!(T_0.to_mut()), 
        |t, _, v, _| {
            return Ok(Object::arr_it(
                Type::MutRef(Box::new(t[0].clone())), 
                v[0].inner.clone(), 
                0
            ));
        }
    ).unwrap();

    let idx = ctx.define_function("next".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[Type::MutRef(Box::new(ARR_IT_OF!(T_0.to_mut())))], 
        T_0, 
        |t, _, v, _| {
            let mut iterator = v[0].deref::<NessaArrayIt>();
            let item;

            {
                let mut reference = iterator.block.borrow_mut();
                let array = &reference.mut_inner::<NessaArray>();

                item = match t[0] {
                    Type::MutRef(_) => array.elements[iterator.pos].get_mut(),
                    Type::Ref(_) => array.elements[iterator.pos].get_ref(),
                    _ => array.elements[iterator.pos].clone(),
                };
            }

            iterator.pos += 1;

            return Ok(item);
        }
    ).unwrap();

    let idx = ctx.define_function("is_consumed".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_IT_OF!(T_0.to_mut()).to_mut()], 
        BOOL, 
        |_, _, v, _| {
            let iterator = v[0].deref::<NessaArrayIt>();

            return Ok(Object::new(iterator.pos >= iterator.block.borrow().get_inner::<NessaArray>().elements.len()));
        }
    ).unwrap();

    let idx = ctx.define_function("panic".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        0,
        &[STR], 
        Type::Empty, 
        |_, _, v, _| {
            return Err(v[0].get::<String>().clone());
        }
    ).unwrap();

    let idx = ctx.define_function("len".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        0,
        &[ARR_OF!(Type::Wildcard).to_ref()], 
        INT, 
        |_, _, v, _| Ok(Object::new(Integer::from(v[0].deref::<NessaArray>().elements.len() as u64)))
    ).unwrap();

    ctx.define_native_function_overload(
        idx, 
        0,
        &[ARR_OF!(Type::Wildcard).to_mut()], 
        INT, 
        |_, _, v, _| Ok(Object::new(Integer::from(v[0].deref::<NessaArray>().elements.len() as u64)))
    ).unwrap();

    let idx = ctx.define_function("sin".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().sin());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.sin());

    let idx = ctx.define_function("cos".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().cos());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.cos());

    let idx = ctx.define_function("tan".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().tan());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.tan());

    let idx = ctx.define_function("sinh".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().sinh());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.sinh());

    let idx = ctx.define_function("cosh".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().cosh());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.cosh());

    let idx = ctx.define_function("tanh".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().tanh());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.tanh());

    let idx = ctx.define_function("fact".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, INT, Integer, a, a.fact()?);

    let idx = ctx.define_function("ln".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().ln());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.ln());

    let idx = ctx.define_function("log2".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().log2());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.log2());

    let idx = ctx.define_function("log10".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().log10());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.log10());

    let idx = ctx.define_function("exp".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().exp());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.exp());

    let idx = ctx.define_function("floor".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, FLOAT, INT, f64, a, Integer::from(a.floor() as u64));

    let idx = ctx.define_function("round".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, FLOAT, INT, f64, a, Integer::from(a.round() as u64));

    let idx = ctx.define_function("ceil".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, FLOAT, INT, f64, a, Integer::from(a.ceil() as u64));

    let idx = ctx.define_function("fract".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.fract());

    let idx = ctx.define_function("sqrt".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64().sqrt());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.sqrt());

    let idx = ctx.define_function("abs".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, INT, Integer, a, a.abs());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.abs());

    let idx = ctx.define_function("rand".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[], FLOAT, |_, _, _, _| Ok(Object::new(rand_f64()))).unwrap();

    let idx = ctx.define_function("rand_int".into()).unwrap();

    define_binary_function_overloads!(ctx, idx, INT, INT, Integer, a, b, Integer::rand_int_range(&a, &b)?);

    let idx = ctx.define_function("as_float".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, a.to_f64());

    let idx = ctx.define_function("is".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[Type::Wildcard], 
        BOOL, 
        |t, _, v, ctx| Ok(Object::new(v[0].get_type().bindable_to(&t[0], ctx)))
    ).unwrap();

    let idx = ctx.define_function("as".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[Type::Wildcard], 
        T_0, 
        |t, _, mut v, ctx| {
            let obj = v.pop().unwrap();
            let obj_type = obj.get_type();

            if !obj_type.bindable_to(&t[0], ctx) {
                Err(format!("Unable to get value of type {} as {}", obj_type.get_name(ctx), t[0].get_name(ctx)))

            } else {
                Ok(obj)
            }
        }
    ).unwrap();

    let idx = ctx.define_function("println".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[], Type::Empty, |_, _, _, _| { 
        println!("");

        return Ok(Object::empty());
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT], Type::Empty, |_, _, v, _| { 
        println!("{}", v[0].get::<Integer>().to_string());

        return Ok(Object::empty());
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FLOAT], Type::Empty, |_, _, v, _| { 
        println!("{}", v[0].get::<f64>().to_string());

        return Ok(Object::empty());
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[BOOL], Type::Empty, |_, _, v, _| { 
        println!("{}", v[0].get::<bool>().to_string());

        return Ok(Object::empty());
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR], Type::Empty, |_, _, v, _| { 
        println!("{}", v[0].get::<String>());

        return Ok(Object::empty());
    }).unwrap();

    let idx = ctx.define_function("move".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut()], T_0, |_, _, mut v, _| { 
        return Ok(v.pop().unwrap().move_contents());
    }).unwrap();

    // Max tuple size is 10 for now
    seq!(I in 0..10 {
        let idx = ctx.define_function(format!("get_{}", I)).unwrap();

        seq!(J in 2..10 {
            let ts = Type::And((0..J).into_iter().map(|i| Type::TemplateParam(i, vec!())).collect());

            ctx.define_native_function_overload(
                idx, 
                J,
                &[ts.clone()], 
                Type::TemplateParam(I, vec!()), 
                |_, _, v, _| Ok(v[0].get::<NessaTuple>().elements[I].clone())
            ).unwrap();
            
            ctx.define_native_function_overload(
                idx, 
                J,
                &[Type::Ref(Box::new(ts.clone()))], 
                Type::Ref(Box::new(Type::TemplateParam(I, vec!()))), 
                |_, _, v, _| Ok(v[0].deref::<NessaTuple>().elements[I].get_ref())
            ).unwrap();
            
            ctx.define_native_function_overload(
                idx, 
                J,
                &[Type::MutRef(Box::new(ts))], 
                Type::MutRef(Box::new(Type::TemplateParam(I, vec!()))), 
                |_, _, v, _| Ok(v[0].deref::<NessaTuple>().elements[I].get_mut())
            ).unwrap();
        });
    });
}