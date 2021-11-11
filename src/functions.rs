use crate::number::Number;
use crate::types::Type;
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
        println!("{}", v[0].to_string());

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
        Type::Template(5, vec!(Type::TemplateParam(0))), 
        |t, _, v| {
            return Ok(Object::new((t[0].clone(), v[0].get::<Reference>().clone(), 0)));
        }
    ).unwrap();

    ctx.define_function("next".into()).unwrap();

    ctx.define_native_function_overload(
        6, 
        1,
        &[Type::MutRef(Box::new(Type::Template(5, vec!(Type::TemplateParam(0)))))], 
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
        1,
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
        1,
        &[Type::MutRef(Box::new(Type::Template(3, vec!(Type::Wildcard))))], 
        Type::Basic(0), 
        |_, _, v| Ok(Object::new(Number::from(v[0].deref::<(Type, Vec<Object>)>().1.len() as u64)))
    ).unwrap();

}