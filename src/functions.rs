use crate::number::Number;
use crate::types::Type;
use crate::object::Object;
use crate::context::NessaContext;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

pub type FunctionOverload = fn(&[&Object]) -> Object;

pub type FunctionOverloads = Vec<(Type, Type, FunctionOverload)>;

#[derive(Clone)]
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

    ctx.define_function_overload(0, &[Type::Basic(0)], Type::Basic(0), |v| { 
        Object::new(&*v[0].deref::<Number>() + Number::from(1)) 
    }).unwrap();

    ctx.define_function("print".into(), vec!()).unwrap();

    ctx.define_function_overload(1, &[Type::Wildcard], Type::Empty, |v| { 
        print!("{}", v[0].to_string());

        return Object::empty();
    }).unwrap();

    ctx.define_function("deref".into(), vec!("T".into())).unwrap();

    ctx.define_function_overload(2, &[Type::Ref(Box::new(Type::TemplateParam(0)))], Type::TemplateParam(0), |v| {
        return v[0].deref_obj();
    }).unwrap();
}