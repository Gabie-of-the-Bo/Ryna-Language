use std::io::Read;
use std::io::Write;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use rand::Rng;
use seq_macro::seq;
use malachite::Integer;
use malachite::num::arithmetic::traits::Abs;

use crate::annotations::Annotation;
use crate::compilation::CompiledNessaExpr;
use crate::integer_ext::*;
use crate::ARR_IT_OF;
use crate::ARR_OF;
use crate::types::*;
use crate::object::*;
use crate::context::NessaContext;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

// Takes type parameters, return type and arguments
pub type FunctionOverloadFn = fn(&Vec<Type>, &Type, Vec<Object>, &NessaContext) -> Result<Object, String>;
pub type OptFunctionOverloadFn = Option<FunctionOverloadFn>;

#[derive(Clone)]
pub struct FunctionOverload {
    pub annotations: Vec<Annotation>,
    pub templates: usize,
    pub args: Type,
    pub ret: Type,
    pub function: OptFunctionOverloadFn
}

pub type FunctionOverloads = Vec<FunctionOverload>;

const EMPTY_FUNC: FunctionOverloadFn = |_, _, _, _| Ok(Object::empty());

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
pub const ITERATOR_FUNC_ID: usize = 10;
pub const NEXT_FUNC_ID: usize = 11;
pub const IS_CONSUMED_FUNC_ID: usize = 12;

pub fn standard_functions(ctx: &mut NessaContext) {
    let idx = ctx.define_function("inc".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT.to_mut()], Type::Empty, EMPTY_FUNC).unwrap();

    let idx = ctx.define_function("print".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].get::<Integer>());

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FLOAT], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].get::<f64>());

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[BOOL], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].get::<bool>());

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].get::<String>());

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT.to_ref().or(INT.to_mut())], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].deref::<Integer>());

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FLOAT.to_ref().or(FLOAT.to_mut())], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].deref::<f64>());

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[BOOL.to_ref().or(BOOL.to_mut())], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].deref::<bool>());

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR.to_ref().or(STR.to_mut())], Type::Empty, |_, _, v, _| { 
        print!("{}", v[0].deref::<String>());

        Ok(Object::empty())
    }).unwrap();
    
    let idx = ctx.define_function("deref".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut()], T_0, EMPTY_FUNC).unwrap();
    ctx.define_native_function_overload(idx, 1, &[T_0.to_ref()], T_0, EMPTY_FUNC).unwrap();

    let idx = ctx.define_function("ref".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0], T_0.to_ref(), EMPTY_FUNC).unwrap();

    let idx = ctx.define_function("mut".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0], T_0.to_mut(), EMPTY_FUNC).unwrap();

    let idx = ctx.define_function("demut".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut()], T_0.to_ref(), EMPTY_FUNC).unwrap();

    let idx = ctx.define_function("arr".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[], ARR_OF!(T_0), |t, _, _, _| Ok(Object::arr(vec!(), t[0].clone()))).unwrap();

    let idx = ctx.define_function("push".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_mut(), T_0], 
        Type::Empty, 
        |_, _, v, _| {
            let array = v[0].deref::<NessaArray>();
            array.elements.push(v[1].clone());

            Ok(Object::empty())
        }
    ).unwrap();

    let idx = ctx.define_function("reserve".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_mut(), INT], 
        Type::Empty, 
        |_, _, v, _| {
            let array = v[0].deref::<NessaArray>();
            let size = v[1].get::<Integer>();

            if is_valid_index(size) && array.elements.try_reserve_exact(to_usize(size)).is_ok() {
                Ok(Object::empty())
    
            } else {
                Err(format!("Unable to reserve {} elements", size))
            }
        }
    ).unwrap();

    let idx = ctx.define_function("capacity".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_ref()], 
        INT, 
        |_, _, mut v, _| Ok(Object::new(Integer::from(v.pop().unwrap().deref::<NessaArray>().elements.capacity() as u64)))
    ).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_mut()], 
        INT, 
        |_, _, mut v, _| Ok(Object::new(Integer::from(v.pop().unwrap().deref::<NessaArray>().elements.capacity() as u64)))
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
                v[0].inner.borrow().dereference().clone(), 
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
                v[0].inner.borrow().dereference().clone(), 
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
            Ok(Object::arr_it(
                Type::MutRef(Box::new(t[0].clone())), 
                v[0].inner.clone(), 
                0
            ))
        }
    ).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_IT_OF!(T_0.to_mut())], 
        ARR_IT_OF!(T_0.to_mut()), 
        |_, _, mut v, _| Ok(v.pop().unwrap())
    ).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_IT_OF!(T_0.to_ref())], 
        ARR_IT_OF!(T_0.to_ref()), 
        |_, _, mut v, _| Ok(v.pop().unwrap())
    ).unwrap();

    let idx = ctx.define_function("next".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[Type::MutRef(Box::new(ARR_IT_OF!(T_0.to_mut())))], 
        T_0.to_mut(), 
        |_, _, v, _| {
            let iterator = v[0].deref::<NessaArrayIt>();
            let item;

            {
                let reference = iterator.block.borrow_mut();
                let array = &reference.mut_inner::<NessaArray>();
                item = array.elements[iterator.pos].get_mut();
            }

            iterator.pos += 1;

            Ok(item)
        }
    ).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[Type::MutRef(Box::new(ARR_IT_OF!(T_0.to_ref())))], 
        T_0.to_ref(), 
        |_, _, v, _| {
            let iterator = v[0].deref::<NessaArrayIt>();
            let item;

            {
                let reference = iterator.block.borrow_mut();
                let array = &reference.mut_inner::<NessaArray>();
                item = array.elements[iterator.pos].get_ref();
            }

            iterator.pos += 1;

            Ok(item)
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

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_IT_OF!(T_0.to_ref()).to_mut()], 
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

    ctx.define_native_function_overload(
        idx, 
        0,
        &[STR.to_ref()], 
        INT, 
        |_, _, v, _| Ok(Object::new(Integer::from(v[0].deref::<String>().len() as u64)))
    ).unwrap();

    ctx.define_native_function_overload(
        idx, 
        0,
        &[STR.to_mut()], 
        INT, 
        |_, _, v, _| Ok(Object::new(Integer::from(v[0].deref::<String>().len() as u64)))
    ).unwrap();

    let idx = ctx.define_function("sin".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).sin());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.sin());

    let idx = ctx.define_function("cos".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).cos());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.cos());

    let idx = ctx.define_function("tan".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).tan());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.tan());

    let idx = ctx.define_function("sinh".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).sinh());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.sinh());

    let idx = ctx.define_function("cosh".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).cosh());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.cosh());

    let idx = ctx.define_function("tanh".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).tanh());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.tanh());

    let idx = ctx.define_function("ln".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).ln());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.ln());

    let idx = ctx.define_function("log2".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).log2());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.log2());

    let idx = ctx.define_function("log10".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).log10());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.log10());

    let idx = ctx.define_function("exp".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).exp());
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

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a).sqrt());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.sqrt());

    let idx = ctx.define_function("abs".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, INT, Integer, a, a.clone().abs());
    define_unary_function_overloads!(ctx, idx, FLOAT, FLOAT, f64, a, a.abs());

    let idx = ctx.define_function("rand".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[], FLOAT, |_, _, _, _| Ok(Object::new(rand::thread_rng().gen_range(0.0..1.0)))).unwrap();

    let idx = ctx.define_function("rand_int".into()).unwrap();

    define_binary_function_overloads!(ctx, idx, INT, INT, Integer, a, b, randint(a.clone(), b.clone()));

    let idx = ctx.define_function("as_float".into()).unwrap();

    define_unary_function_overloads!(ctx, idx, INT, FLOAT, Integer, a, to_f64(a));

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

    let idx = ctx.define_function("drop".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut()], Type::Empty, |_, _, mut v, _| { 
        v.pop().unwrap().drop_contents();
        Ok(Object::empty())
    }).unwrap();

    let idx = ctx.define_function("move".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut()], T_0, EMPTY_FUNC).unwrap();

    let idx = ctx.define_function("fwd".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[Type::Wildcard], T_0, |a, _, mut v, ctx| {
        let type_arg = a.last().unwrap();
        let param_arg = v.last().unwrap().get_type();

        match (type_arg, &param_arg) {
            (Type::Ref(a), Type::Ref(b)) if a == b => Ok(v.pop().unwrap()),
            (Type::MutRef(a), Type::MutRef(b)) if a == b => Ok(v.pop().unwrap()),
            (Type::Ref(a), Type::MutRef(b)) if a == b => Ok(v.pop().unwrap().get_ref()),

            (a, Type::MutRef(b)) if *a == **b => Ok(v.pop().unwrap().move_contents()),
            (a, Type::Ref(b)) if *a == **b => Ok(v.pop().unwrap().deref_obj().deep_clone()),

            (Type::MutRef(b), a) if *a == **b => Ok(v.pop().unwrap().get_mut_nostack()),
            (Type::Ref(b), a) if *a == **b => Ok(v.pop().unwrap().get_ref_nostack()),

            (a, b) if a == b => Ok(v.pop().unwrap().move_contents()),
            
            _ => Err(format!(
                "Unable to forward value of type {} as type {}",
                param_arg.get_name(ctx),
                type_arg.get_name(ctx)
            ))
        }
    }).unwrap();

    let idx = ctx.define_function("cfwd".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[Type::Wildcard], T_0, |a, _, mut v, ctx| {
        let type_arg = a.last().unwrap();
        let param_arg = v.last().unwrap().get_type();

        match (type_arg, &param_arg) {
            (Type::Ref(a), Type::Ref(b)) if a == b => Ok(v.pop().unwrap()),
            (Type::MutRef(a), Type::MutRef(b)) if a == b => Ok(v.pop().unwrap()),
            (Type::Ref(a), Type::MutRef(b)) if a == b => Ok(v.pop().unwrap().get_ref()),

            (a, Type::MutRef(b)) if *a == **b => Ok(v.pop().unwrap().deref_obj().deep_clone()),
            (a, Type::Ref(b)) if *a == **b => Ok(v.pop().unwrap().deref_obj().deep_clone()),
            
            (Type::MutRef(b), a) if *a == **b => Ok(v.pop().unwrap().get_mut_nostack()),
            (Type::Ref(b), a) if *a == **b => Ok(v.pop().unwrap().get_ref_nostack()),

            (a, b) if a == b => Ok(v.pop().unwrap().deep_clone()),
            
            _ => Err(format!(
                "Unable to forward value of type {} as type {}",
                param_arg.get_name(ctx),
                type_arg.get_name(ctx)
            ))
        }
    }).unwrap();

    let idx = ctx.define_function("swap".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut(), T_0.to_mut()], Type::Empty, |_, _, mut v, _| {
        let a = v.pop().unwrap();
        let b = v.pop().unwrap();

        a.swap_contents(&b);

        Ok(Object::empty())
    }).unwrap();

    let idx = ctx.define_function("get_file".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR], FILE, |_, _, v, _| {
        let path = v[0].get::<String>();
        let pathbuf = std::path::Path::new(path);

        Ok(Object::file(pathbuf.into()))
    }).unwrap();

    let idx = ctx.define_function("open".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut(), BOOL, BOOL, BOOL], Type::Empty, |_, _, v, _| {
        let file = &mut *v[0].deref::<NessaFile>();
        let read = *v[1].get::<bool>();
        let write = *v[2].get::<bool>();
        let append = *v[3].get::<bool>();

        file.open(read, write, append)?;
        
        Ok(Object::empty())
    }).unwrap();

    let idx = ctx.define_function("close".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut()], Type::Empty, |_, _, v, _| {
        let file = &mut *v[0].deref::<NessaFile>();
        file.close()?;

        Ok(Object::empty())
    }).unwrap();

    let idx = ctx.define_function("exists".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut()], BOOL, |_, _, v, _| {
        let file = &*v[0].deref::<NessaFile>();

        Ok(ObjectBlock::Bool(file.exists()?).to_obj())
    }).unwrap();

    let idx = ctx.define_function("delete".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut()], BOOL, |_, _, v, _| {
        let file = &mut *v[0].deref::<NessaFile>();

        Ok(ObjectBlock::Bool(file.delete()?).to_obj())
    }).unwrap();

    let idx = ctx.define_function("read_str".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut()], STR, |_, _, v, _| {
        let file = v[0].deref::<NessaFile>();
        let mut buf = String::new();

        if !file.is_open() {
            return Err(format!("File at {} is closed", file.path.to_str().unwrap()));
        }

        let res = file.file.as_ref().unwrap().borrow_mut().read_to_string(&mut buf);

        match res {
            Ok(_) => Ok(ObjectBlock::Str(buf).to_obj()),
            Err(_) => Err("Unable to read file".into())
        }    
    }).unwrap();

    let idx = ctx.define_function("read_bytes".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut()], ARR_OF!(INT), |_, _, v, _| {
        let file = v[0].deref::<NessaFile>();
        let mut buf = vec!();

        if !file.is_open() {
            return Err(format!("File at {} is closed", file.path.to_str().unwrap()));
        }

        let res = file.file.as_ref().unwrap().borrow_mut().read_to_end(&mut buf);

        match res {
            Ok(_) => {
                let arr = buf.into_iter().map(|i| ObjectBlock::Int(Integer::from(i as u32)).to_obj()).collect();
                Ok(Object::arr(arr, INT))
            },
            Err(_) => Err("Unable to read file".into())
        }    
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut(), INT], ARR_OF!(INT), |_, _, v, _| {
        let file = v[0].deref::<NessaFile>();
        let num_bytes = v[1].get::<Integer>();

        if !is_valid_index(num_bytes) || *num_bytes == *ZERO {
            return Err(format!("Unable to read {} bytes from file", num_bytes));
        }

        if !file.is_open() {
            return Err(format!("File at {} is closed", file.path.to_str().unwrap()));
        }

        let mut buf = vec!(0; to_usize(num_bytes));
        let res = file.file.as_ref().unwrap().borrow_mut().read_exact(&mut buf);

        match res {
            Ok(_) => {
                let arr = buf.into_iter().map(|i| ObjectBlock::Int(Integer::from(i as u32)).to_obj()).collect();
                Ok(Object::arr(arr, INT))
            },
            Err(_) => Err("Unable to read file".into())
        }    
    }).unwrap();

    let idx = ctx.define_function("write_str".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut(), STR.to_ref()], BOOL, |_, _, v, _| {
        let file = v[0].deref::<NessaFile>();
        let content = &*v[1].deref::<String>();

        if !file.is_open() {
            return Err(format!("File at {} is closed", file.path.to_str().unwrap()));
        }

        let ok = file.file.as_ref().unwrap().borrow_mut().write_all(content.as_bytes()).is_ok();

        Ok(ObjectBlock::Bool(ok).to_obj())
    }).unwrap();

    let idx = ctx.define_function("write_bytes".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut(), ARR_OF!(INT).to_ref()], BOOL, |_, _, v, _| {
        let file = v[0].deref::<NessaFile>();
        let content = &*v[1].deref::<NessaArray>();

        if !file.is_open() {
            return Err(format!("File at {} is closed", file.path.to_str().unwrap()));
        }

        let mut bytes = vec!();
        bytes.reserve_exact(content.elements.len());

        for n in &content.elements {
            let byte = n.get::<Integer>();

            if is_valid_byte(byte) {
                bytes.push(to_u8(byte));
            
            } else {
                return Err(format!("{} is not a valid byte value", byte));
            }
        }

        let ok = file.file.as_ref().unwrap().borrow_mut().write_all(&bytes).is_ok();

        Ok(ObjectBlock::Bool(ok).to_obj())
    }).unwrap();

    let idx = ctx.define_function("to_string".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT], STR, |_, _, mut v, _| {
        let obj = v.pop().unwrap().get::<Integer>().to_string();

        Ok(ObjectBlock::Str(obj).to_obj())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FLOAT], STR, |_, _, mut v, _| {
        let obj = v.pop().unwrap().get::<f64>().to_string();

        Ok(ObjectBlock::Str(obj).to_obj())
    }).unwrap();

    // String functions
    let idx = ctx.define_function("code_point_at".into()).unwrap();
    
    ctx.define_native_function_overload(idx, 0, &[STR.to_ref().or(STR.to_mut()), INT], INT, |_, _, v, _| {
        let string = &*v[0].deref::<String>();
        let idx = v[1].get::<Integer>();

        if !is_valid_index(idx) {
            return Err(format!("{} is not a valid index", idx));
        
        } else if string.len() <= to_usize(idx) {
            return Err(format!("{} is higher than the length of the string ({})", idx, string.len()));
        }

        if let Some(character) = string[to_usize(idx)..].chars().next() {
            Ok(ObjectBlock::Int(Integer::from(character as u64)).to_obj())

        } else {
            Err(format!("Invalid character start at position {}", idx))
        }
    }).unwrap();

    let idx = ctx.define_function("code_point_to_str".into()).unwrap();
    
    ctx.define_native_function_overload(idx, 0, &[INT], STR, |_, _, v, _| {
        let cp = v[0].get::<Integer>();

        if !is_valid_index(cp) {
            return Err(format!("{} is not a valid code point", cp));
        }

        if let Some(character) = char::from_u32(to_u32(cp)) {
            Ok(ObjectBlock::Str(character.to_string()).to_obj())

        } else {
            Err(format!("{} is not a valid code point", cp))
        }
    }).unwrap();

    let idx = ctx.define_function("code_point_length".into()).unwrap();
    
    ctx.define_native_function_overload(idx, 0, &[INT], INT, |_, _, v, _| {
        let cp = v[0].get::<Integer>();

        if !is_valid_index(cp) {
            return Err(format!("{} is not a valid code point", cp));
        }

        if let Some(character) = char::from_u32(to_u32(cp)) {
            Ok(ObjectBlock::Int(Integer::from(character.len_utf8() as u64)).to_obj())

        } else {
            Err(format!("{} is not a valid code point", cp))
        }
    }).unwrap();

    let idx = ctx.define_function("utf8_array".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR.to_ref().or(STR.to_mut())], ARR_OF!(INT), |_, _, v, _| {
        let string = &*v[0].deref::<String>();
        let arr = string.bytes()
                        .map(|i| ObjectBlock::Int(Integer::from(i as u64)).to_obj())
                        .collect();

        Ok(Object::arr(arr, INT))
    }).unwrap();

    let idx = ctx.define_function("utf8_to_str".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[ARR_OF!(INT).to_ref().or(ARR_OF!(INT).to_mut())], STR, |_, _, v, _| {
        let arr = &*v[0].deref::<NessaArray>();
        let mut bytes = vec!();
        bytes.reserve_exact(arr.elements.len());

        for i in &arr.elements {
            let n = i.get::<Integer>();

            if !is_valid_byte(n) {
                return Err(format!("{} is not a valid byte", n));
            }
            
            bytes.push(to_u8(n));
        }

        if let Ok(string) = String::from_utf8(bytes) {
            Ok(ObjectBlock::Str(string).to_obj())

        } else {
            Err("Invalid UTF-8 array".into())
        }
    }).unwrap();

    let idx = ctx.define_function("truncate".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT], INT, |_, _, v, _| {
        let cp = v[0].get::<Integer>();
        Ok(Object::new(truncate(cp)))
    }).unwrap();

    let idx = ctx.define_function("input".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[], STR, |_, _, _, _| {
        let mut buffer = String::new();
        
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut buffer).unwrap();
    
        Ok(Object::new(buffer))
    }).unwrap();

    let idx = ctx.define_function("num_args".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[], INT, |_, _, _, ctx| {
        Ok(Object::new(Integer::from(ctx.program_input.len() as u64)))
    }).unwrap();

    let idx = ctx.define_function("get_arg".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT], STR, |_, _, v, ctx| {
        let idx = v[0].get::<Integer>();

        if !is_valid_index(idx) {
            return Err(format!("{} is not a valid index", idx));
        
        } else if ctx.program_input.len() <= to_usize(idx) {
            return Err(format!("{} is higher than the number of input arguments ({})", idx, ctx.program_input.len()));
        }

        Ok(Object::new(ctx.program_input[to_usize(idx)].clone()))
    }).unwrap();

    let idx = ctx.define_function("set".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_mut(), T_0, INT], 
        Type::Empty, 
        |_, _, v, _| {
            let array = v[0].deref::<NessaArray>();
            let idx = v[2].get::<Integer>();

            if !is_valid_index(idx) {
                return Err(format!("{} is not a valid index", idx));
            
            } else if array.elements.len() <= to_usize(idx) {
                return Err(format!("{} is higher than the length of the array ({})", idx, array.elements.len()));
            }
            
            array.elements[to_usize(idx)] = v[1].clone();

            Ok(Object::empty())
        }
    ).unwrap();

    let idx = ctx.define_function("insert".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_mut(), T_0, INT], 
        Type::Empty, 
        |_, _, v, _| {
            let array = v[0].deref::<NessaArray>();
            let idx = v[2].get::<Integer>();

            if !is_valid_index(idx) {
                return Err(format!("{} is not a valid index", idx));
            
            } else if array.elements.len() <= to_usize(idx) {
                return Err(format!("{} is higher than the length of the array ({})", idx, array.elements.len()));
            }
            
            array.elements.insert(to_usize(idx), v[1].clone());

            Ok(Object::empty())
        }
    ).unwrap();

    let idx = ctx.define_function("remove".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_mut(), INT], 
        Type::Empty, 
        |_, _, v, _| {
            let array = v[0].deref::<NessaArray>();
            let idx = v[1].get::<Integer>();

            if !is_valid_index(idx) {
                return Err(format!("{} is not a valid index", idx));
            
            } else if array.elements.len() <= to_usize(idx) {
                return Err(format!("{} is higher than the length of the array ({})", idx, array.elements.len()));
            }
            
            array.elements.remove(to_usize(idx));

            Ok(Object::empty())
        }
    ).unwrap();

    let idx = ctx.define_function("pop".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        1,
        &[ARR_OF!(T_0).to_mut()], 
        Type::Empty, 
        |_, _, v, _| {
            let array = v[0].deref::<NessaArray>();
            array.elements.pop();

            Ok(Object::empty())
        }
    ).unwrap();

    let idx = ctx.define_function("time".into()).unwrap();

    ctx.define_native_function_overload(
        idx, 
        0,
        &[], 
        INT, 
        |_, _, _, _| {
            let time = SystemTime::now().duration_since(UNIX_EPOCH);

            match time {
                Ok(d) => {
                    let duration = d.as_nanos();

                    Ok(Object::new(Integer::from(duration)))
                },
                Err(_) => Err("Unable to get current time".into()),
            }
        }
    ).unwrap();

    let idx = ctx.define_function("dec".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT.to_mut()], Type::Empty, EMPTY_FUNC).unwrap();

    // Max tuple size is 10 for now
    seq!(I in 0..10 {
        let idx = ctx.define_function(format!("get_{}", I)).unwrap();

        seq!(J in 2..10 {
            let ts = Type::And((0..J).map(|i| Type::TemplateParam(i, vec!())).collect());

            let res = ctx.define_native_function_overload(
                idx, 
                J,
                &[ts.clone()], 
                Type::TemplateParam(I, vec!()), 
                EMPTY_FUNC
            ).unwrap();

            ctx.cache.opcodes.functions.insert((idx, res), (CompiledNessaExpr::TupleElemMove(I), 0));
            
            let res = ctx.define_native_function_overload(
                idx, 
                J,
                &[Type::Ref(Box::new(ts.clone()))], 
                Type::Ref(Box::new(Type::TemplateParam(I, vec!()))), 
                EMPTY_FUNC
            ).unwrap();
            
            ctx.cache.opcodes.functions.insert((idx, res), (CompiledNessaExpr::TupleElemRef(I), 0));

            let res = ctx.define_native_function_overload(
                idx, 
                J,
                &[Type::MutRef(Box::new(ts))], 
                Type::MutRef(Box::new(Type::TemplateParam(I, vec!()))), 
                EMPTY_FUNC
            ).unwrap();

            ctx.cache.opcodes.functions.insert((idx, res), (CompiledNessaExpr::TupleElemMut(I), 0));
        });
    });
}

pub fn define_macro_emit_fn(ctx: &mut NessaContext, name: String) {
    let idx = ctx.define_function(name).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR], crate::types::Type::Empty, |_, _, mut args, ctx| {
        let obj = args.pop().unwrap();
        let string = obj.get::<String>();

        *ctx.captured_output.borrow_mut() += string;

        Ok(Object::empty())
    }).unwrap();
}