use crate::number::Number;
use crate::types::Type;
use crate::object::*;
use crate::context::NessaContext;
use crate::parser::NessaExpr;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

pub enum FunctionOverload {
    Native(fn(Vec<Type>, Vec<Object>) -> Object),
    Nessa(Vec<NessaExpr>, Vec<(String, Type)>, usize)
}

pub type FunctionOverloads = Vec<(Type, Type, FunctionOverload)>;

pub struct Function {
    pub id: usize,
    pub name: String,
    pub params: Vec<String>,
    pub overloads: FunctionOverloads
}

/*
                                                  ╒══════════════════════╕
    ============================================= │  STANDARD FUNCTIONS  │ =============================================
                                                  ╘══════════════════════╛
*/

pub fn standard_functions(ctx: &mut NessaContext) {
    ctx.define_function("inc".into(), vec!()).unwrap();

    ctx.define_native_function_overload(0, &[Type::Basic(0)], Type::Basic(0), |_, v| { 
        Object::new(&*v[0].deref::<Number>() + Number::from(1)) 
    }).unwrap();

    ctx.define_function("print".into(), vec!()).unwrap();

    ctx.define_native_function_overload(1, &[Type::Wildcard], Type::Empty, |_, v| { 
        println!("{}", v[0].to_string());

        return Object::empty();
    }).unwrap();

    ctx.define_function("deref".into(), vec!("T".into())).unwrap();

    ctx.define_native_function_overload(2, &[Type::Ref(Box::new(Type::TemplateParam(0)))], Type::TemplateParam(0), |_, v| {
        return v[0].deref_obj();
    }).unwrap();

    ctx.define_function("arr".into(), vec!("T".into())).unwrap();

    ctx.define_native_function_overload(
        3, 
        &[], 
        Type::Template(3, vec!(Type::TemplateParam(0))), 
        |t, _| Object::new((t[0].clone(), vec!()))
    ).unwrap();

    ctx.define_function("push".into(), vec!("T".into())).unwrap();

    ctx.define_native_function_overload(
        4, 
        &[Type::MutRef(Box::new(Type::Template(3, vec!(Type::TemplateParam(0))))), Type::TemplateParam(0)], 
        Type::Empty, 
        |_, v| {
            let array = v[0].get::<Reference>();

            array.get_mut::<(Type, Vec<Object>)>().1.push(v[1].clone());

            return Object::empty();
        }
    ).unwrap();
}