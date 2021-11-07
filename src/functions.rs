use crate::number::Number;
use crate::types::Type;
use crate::object::*;
use crate::context::NessaContext;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

pub type FunctionOverload = Option<fn(&Vec<Type>, Vec<Object>) -> Object>;

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

pub fn standard_functions(ctx: &mut NessaContext) {
    ctx.define_function("inc".into()).unwrap();

    ctx.define_native_function_overload(0, 0, &[Type::MutRef(Box::new(Type::Basic(0)))], Type::Empty, |_, v| { 
        *v[0].get::<Reference>().get_mut::<Number>() += Number::from(1);

        return Object::empty();
    }).unwrap();

    ctx.define_function("print".into()).unwrap();

    ctx.define_native_function_overload(1, 0, &[Type::Wildcard], Type::Empty, |_, v| { 
        println!("{}", v[0].to_string());

        return Object::empty();
    }).unwrap();

    ctx.define_function("deref".into()).unwrap();

    ctx.define_native_function_overload(2, 1, &[Type::MutRef(Box::new(Type::TemplateParam(0)))], Type::TemplateParam(0), |_, v| {
        return v[0].deref_obj();
    }).unwrap();

    ctx.define_function("arr".into()).unwrap();

    ctx.define_native_function_overload(
        3, 
        1,
        &[], 
        Type::Template(3, vec!(Type::TemplateParam(0))), 
        |t, _| Object::new((t[0].clone(), vec!()))
    ).unwrap();

    ctx.define_function("push".into()).unwrap();

    ctx.define_native_function_overload(
        4, 
        1,
        &[Type::MutRef(Box::new(Type::Template(3, vec!(Type::TemplateParam(0))))), Type::TemplateParam(0)], 
        Type::Empty, 
        |_, v| {
            let array = v[0].get::<Reference>();

            array.get_mut::<(Type, Vec<Object>)>().1.push(v[1].clone());

            return Object::empty();
        }
    ).unwrap();

    ctx.define_function("iterator".into()).unwrap();

    ctx.define_native_function_overload(
        5, 
        1,
        &[Type::MutRef(Box::new(Type::Template(3, vec!(Type::TemplateParam(0)))))], 
        Type::Template(5, vec!(Type::MutRef(Box::new(Type::TemplateParam(0))))), 
        |t, v| {
            return Object::new((Type::MutRef(Box::new(t[0].clone())), v[0].get::<Reference>().clone(), 0));
        }
    ).unwrap();

    ctx.define_native_function_overload(
        5, 
        1,
        &[Type::Ref(Box::new(Type::Template(3, vec!(Type::TemplateParam(0)))))], 
        Type::Template(5, vec!(Type::Ref(Box::new(Type::TemplateParam(0))))), 
        |t, v| {
            return Object::new((Type::Ref(Box::new(t[0].clone())), v[0].get::<Reference>().clone(), 0));
        }
    ).unwrap();

    ctx.define_native_function_overload(
        5, 
        1,
        &[Type::Template(3, vec!(Type::TemplateParam(0)))], 
        Type::Template(5, vec!(Type::TemplateParam(0))), 
        |t, v| {
            return Object::new((t[0].clone(), v[0].get::<Reference>().clone(), 0));
        }
    ).unwrap();

    ctx.define_function("next".into()).unwrap();

    ctx.define_native_function_overload(
        6, 
        1,
        &[Type::MutRef(Box::new(Type::Template(5, vec!(Type::TemplateParam(0)))))], 
        Type::TemplateParam(0), 
        |t, v| {
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

            return item;
        }
    ).unwrap();

    ctx.define_function("is_consumed".into()).unwrap();

    ctx.define_native_function_overload(
        7, 
        1,
        &[Type::MutRef(Box::new(Type::Template(5, vec!(Type::Wildcard))))], 
        Type::Basic(2), 
        |_, v| {
            let reference = v[0].get::<Reference>();
            let iterator = reference.get::<(Type, Reference, usize)>();

            return Object::new(iterator.2 >= iterator.1.get::<(Type, Vec<Object>)>().1.len());
        }
    ).unwrap();
}