use std::io::Read;
use std::io::Write;

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
pub type FunctionOverloadInner = fn(&Vec<Type>, &Type, Vec<Object>, &NessaContext) -> Result<Object, String>;
pub type FunctionOverload = Option<FunctionOverloadInner>;

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
pub const ITERATOR_FUNC_ID: usize = 10;
pub const NEXT_FUNC_ID: usize = 11;
pub const IS_CONSUMED_FUNC_ID: usize = 12;

pub fn standard_functions(ctx: &mut NessaContext) {
    let idx = ctx.define_function("inc".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT.to_mut()], Type::Empty, |_, _, v, _| { 
        *v[0].deref::<Integer>() += Integer::from(1);

        Ok(Object::empty())
    }).unwrap();

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
    
    let idx = ctx.define_function("deref".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut()], T_0, |_, _, v, _| {
        Ok(v[0].deref_obj().deep_clone())
    }).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_ref()], T_0, |_, _, v, _| {
        Ok(v[0].deref_obj().deep_clone())
    }).unwrap();

    let idx = ctx.define_function("ref".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0], T_0.to_ref(), |_, _, v, _| {
        Ok(v[0].get_ref())
    }).unwrap();

    let idx = ctx.define_function("mut".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0], T_0.to_mut(), |_, _, v, _| {
        Ok(v[0].get_mut())
    }).unwrap();

    let idx = ctx.define_function("demut".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut()], T_0.to_ref(), |_, _, v, _| {
        Ok(v[0].get_ref())
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
            let mut array = v[0].deref::<NessaArray>();
            let size = v[1].get::<Integer>();

            if size.limbs.len() == 1 && size.limbs[0] < usize::MAX as u64 && array.elements.try_reserve_exact(size.limbs[0] as usize).is_ok() {
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
            let mut iterator = v[0].deref::<NessaArrayIt>();
            let item;

            {
                let mut reference = iterator.block.borrow_mut();
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
            let mut iterator = v[0].deref::<NessaArrayIt>();
            let item;

            {
                let mut reference = iterator.block.borrow_mut();
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
        println!();

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[INT], Type::Empty, |_, _, v, _| { 
        println!("{}", v[0].get::<Integer>());

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FLOAT], Type::Empty, |_, _, v, _| { 
        println!("{}", v[0].get::<f64>());

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[BOOL], Type::Empty, |_, _, v, _| { 
        println!("{}", v[0].get::<bool>());

        Ok(Object::empty())
    }).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR], Type::Empty, |_, _, v, _| { 
        println!("{}", v[0].get::<String>());

        Ok(Object::empty())
    }).unwrap();

    let idx = ctx.define_function("drop".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut()], Type::Empty, |_, _, mut v, _| { 
        v.pop().unwrap().drop_contents();
        Ok(Object::empty())
    }).unwrap();

    let idx = ctx.define_function("move".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[T_0.to_mut()], T_0, |_, _, mut v, _| { 
        Ok(v.pop().unwrap().move_contents())
    }).unwrap();

    let idx = ctx.define_function("fwd".into()).unwrap();

    ctx.define_native_function_overload(idx, 1, &[Type::Wildcard], T_0, |a, _, mut v, ctx| {
        let type_arg = a.last().unwrap();
        let param_arg = v.last().unwrap().get_type();

        match (type_arg, &param_arg) {
            (Type::Ref(a), Type::Ref(b)) if a == b => Ok(v.pop().unwrap()),
            (Type::MutRef(a), Type::MutRef(b)) if a == b => Ok(v.pop().unwrap()),
            (Type::Ref(a), Type::MutRef(b)) if a == b => Ok(v.pop().unwrap().get_ref()),

            (a, Type::MutRef(b)) if *a == **b => Ok(v.pop().unwrap().move_contents()),
            (a, Type::Ref(b)) if *a == **b => Ok(v.pop().unwrap().deep_clone()),
            (a, b) if a == b => Ok(v.pop().unwrap().move_contents()),
            
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

    let idx = ctx.define_function("create_file".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR], FILE, |_, _, v, _| {
        let path = &*v[0].get::<String>();
        let file = std::fs::File::create(std::path::Path::new(path));

        match file {
            Ok(inner) => Ok(Object::file(inner)),
            Err(_) => Err(format!("Unable to create file file at {}", path)),
        }
    }).unwrap();

    let idx = ctx.define_function("open_file".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR, BOOL, BOOL, BOOL], FILE, |_, _, v, _| {
        let path = &*v[0].get::<String>();
        let read = *v[1].get::<bool>();
        let write = *v[2].get::<bool>();
        let append = *v[3].get::<bool>();

        let file = std::fs::OpenOptions::new()
            .read(read)
            .write(write)
            .append(append)
            .open(path);

        match file {
            Ok(inner) => Ok(Object::file(inner)),
            Err(_) => Err(format!("Unable to open file file at {}", path))
        }    
    }).unwrap();

    let idx = ctx.define_function("remove_file".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR], BOOL, |_, _, v, _| {
        let path = &*v[0].get::<String>();

        Ok(ObjectBlock::Bool(std::fs::remove_file(std::path::Path::new(path)).is_ok()).to_obj())
    }).unwrap();

    let idx = ctx.define_function("read_str".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut()], STR, |_, _, v, _| {
        let file = v[0].deref::<NessaFile>();
        let mut buf = String::new();
        let res = file.file.borrow_mut().read_to_string(&mut buf);

        match res {
            Ok(_) => Ok(ObjectBlock::Str(buf).to_obj()),
            Err(_) => Err("Unable to read file".into())
        }    
    }).unwrap();

    let idx = ctx.define_function("read_bytes".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut()], ARR_OF!(INT), |_, _, v, _| {
        let file = v[0].deref::<NessaFile>();
        let mut buf = vec!();
        let res = file.file.borrow_mut().read_to_end(&mut buf);

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

        if num_bytes.negative || num_bytes.is_zero() || num_bytes.limbs.len() > 1 || num_bytes.limbs[0] > usize::MAX as u64 {
            return Err(format!("Unable to read {} bytes from file", num_bytes));
        }

        let mut buf = vec!(0; num_bytes.limbs[0] as usize);
        let res = file.file.borrow_mut().read_exact(&mut buf);

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

        let ok = file.file.borrow_mut().write_all(content.as_bytes()).is_ok();

        Ok(ObjectBlock::Bool(ok).to_obj())
    }).unwrap();

    let idx = ctx.define_function("write_bytes".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[FILE.to_mut(), ARR_OF!(INT).to_ref()], BOOL, |_, _, v, _| {
        let file = v[0].deref::<NessaFile>();
        let content = &*v[1].deref::<NessaArray>();
        let mut bytes = vec!();
        bytes.reserve_exact(content.elements.len());

        for n in &content.elements {
            let byte = &*n.get::<Integer>();

            if byte.is_valid_byte() {
                bytes.push(byte.limbs[0] as u8);
            
            } else {
                return Err(format!("{} is not a valid byte value", byte));
            }
        }

        let ok = file.file.borrow_mut().write_all(&bytes).is_ok();

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
    
    ctx.define_native_function_overload(idx, 0, &[STR.to_ref(), INT], INT, |_, _, v, _| {
        let string = &*v[0].deref::<String>();
        let idx = &*v[1].get::<Integer>();

        if !idx.is_valid_index() {
            return Err(format!("{} is not a valid index", idx));
        }

        if let Some(character) = string[idx.as_usize()..].chars().next() {
            Ok(ObjectBlock::Int(Integer::from(character as u64)).to_obj())

        } else {
            Err(format!("Invalid character start at position {}", idx))
        }
    }).unwrap();
    
    ctx.define_native_function_overload(idx, 0, &[STR.to_mut(), INT], INT, |_, _, v, _| {
        let string = &*v[0].deref::<String>();
        let idx = &*v[1].get::<Integer>();

        if !idx.is_valid_index() {
            return Err(format!("{} is not a valid index", idx));
        }

        if let Some(character) = string[idx.as_usize()..].chars().next() {
            Ok(ObjectBlock::Int(Integer::from(character as u64)).to_obj())

        } else {
            Err(format!("Invalid character start at position {}", idx))
        }
    }).unwrap();

    let idx = ctx.define_function("code_point_to_str".into()).unwrap();
    
    ctx.define_native_function_overload(idx, 0, &[INT], STR, |_, _, v, _| {
        let cp = &*v[0].get::<Integer>();

        if !cp.is_valid_index() {
            return Err(format!("{} is not a valid code point", cp));
        }

        if let Some(character) = char::from_u32(cp.limbs[0] as u32) {
            Ok(ObjectBlock::Str(character.to_string()).to_obj())

        } else {
            Err(format!("{} is not a valid code point", cp))
        }
    }).unwrap();

    let idx = ctx.define_function("code_point_length".into()).unwrap();
    
    ctx.define_native_function_overload(idx, 0, &[INT], INT, |_, _, v, _| {
        let cp = &*v[0].get::<Integer>();

        if !cp.is_valid_index() {
            return Err(format!("{} is not a valid code point", cp));
        }

        if let Some(character) = char::from_u32(cp.limbs[0] as u32) {
            Ok(ObjectBlock::Int(Integer::from(character.len_utf8() as u64)).to_obj())

        } else {
            Err(format!("{} is not a valid code point", cp))
        }
    }).unwrap();

    let idx = ctx.define_function("utf8_array".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[STR.to_ref()], ARR_OF!(INT), |_, _, v, _| {
        let string = &*v[0].deref::<String>();
        let arr = string.bytes()
                        .map(|i| ObjectBlock::Int(Integer::from(i as u64)).to_obj())
                        .collect();

        Ok(Object::arr(arr, INT))
    }).unwrap();

    let idx = ctx.define_function("utf8_to_str".into()).unwrap();

    ctx.define_native_function_overload(idx, 0, &[ARR_OF!(INT).to_ref()], STR, |_, _, v, _| {
        let arr = &*v[0].deref::<NessaArray>();
        let mut bytes = vec!();
        bytes.reserve_exact(arr.elements.len());

        for i in &arr.elements {
            let n = &*i.get::<Integer>();

            if !n.is_valid_byte() {
                return Err(format!("{} is not a valid byte", n));
            }
            
            bytes.push(n.as_u8());
        }

        if let Ok(string) = String::from_utf8(bytes) {
            Ok(ObjectBlock::Str(string).to_obj())

        } else {
            Err("Invalid UTF-8 array".into())
        }
    }).unwrap();

    // Max tuple size is 10 for now
    seq!(I in 0..10 {
        let idx = ctx.define_function(format!("get_{}", I)).unwrap();

        seq!(J in 2..10 {
            let ts = Type::And((0..J).map(|i| Type::TemplateParam(i, vec!())).collect());

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