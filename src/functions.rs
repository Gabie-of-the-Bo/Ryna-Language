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
    pub overloads: FunctionOverloads
}

/*
                                                  ╒══════════════════════╕
    ============================================= │  STANDARD FUNCTIONS  │ =============================================
                                                  ╘══════════════════════╛
*/

pub fn standard_functions(ctx: &mut NessaContext) {
    ctx.define_function("inc".into()).unwrap();

    ctx.define_function_overload(0, &[Type::Basic(0)], Type::Basic(0), |v| { 
        Object::new(&*v[0].deref::<Number>() + Number::from(1)) 
    }).unwrap();
}