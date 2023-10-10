use colored::Colorize;
use serde::{Serialize, Deserialize};

use crate::{types::{Type, INT, FLOAT, STR, BOOL, T_1, T_0, T_2}, context::NessaContext, ARR_OF, ARR_IT_OF};

#[derive(Clone)]
pub struct Interface {
    pub id: usize,
    pub name: String,
    pub params: Vec<String>,
    pub fns: Vec<(String, Option<Vec<String>>, Vec<(String, Type)>, Type)>
}

#[derive(Clone, Serialize, Deserialize)]
pub struct InterfaceImpl {
    pub interface_id: usize,
    pub args: Vec<Type>,
    pub interface_type: Type
}

#[derive(Clone, Hash, Debug, PartialEq, Serialize, Deserialize)]
pub struct InterfaceConstraint {
    pub id: usize,
    pub args: Vec<Type>
}

impl InterfaceConstraint {
    pub fn new(id: usize, args: Vec<Type>) -> InterfaceConstraint {
        return InterfaceConstraint {
            id: id,
            args: args
        };
    }
}

impl InterfaceConstraint {
    pub fn get_name(&self, ctx: &NessaContext) -> String {
        if self.args.len() > 0 {
            return format!(
                "{}<{}>", 
                ctx.interfaces[self.id].name.green(),
                self.args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            );

        } else {
            return format!("{}", ctx.interfaces[self.id].name.green());
        }
    }
}

/*
                                                  ╒═══════════════════════╕
    ============================================= │  STANDARD INTERFACES  │ =============================================
                                                  ╘══════════════════════=╛
*/

// Constants for common interfaces
pub const ITERABLE_ID: usize = 0;
pub const PRINTABLE_ID: usize = 1;

pub const PRINTABLE: InterfaceConstraint = InterfaceConstraint { id: PRINTABLE_ID, args: vec!() };

#[macro_export]
macro_rules! ITERABLE_OF { ($t: expr) => { InterfaceConstraint::new($t) }; }

// Standard context
pub fn standard_interfaces(ctx: &mut NessaContext) {
    
    // Definitions
    ctx.define_interface("Iterable".into(), vec!("Iter".into(), "Elem".into()), vec!(
        ("iterator".into(), None, vec!(("".into(), Type::SelfType)), T_0),
        ("next".into(), None, vec!(("".into(), T_0.to_mut())), T_1),
        ("is_consumed".into(), None, vec!(("".into(), T_0.to_mut())), BOOL)        
    )).unwrap();

    ctx.define_interface("Printable".into(), vec!(), vec!(
        ("print".into(), None, vec!(("".into(), Type::SelfType)), Type::Empty)
    )).unwrap();

    // Implementations
    ctx.define_interface_impl("Iterable".into(), vec!("T".into()), ARR_OF!(T_2), vec!(ARR_IT_OF!(T_2.to_mut()), T_2)).unwrap();
    ctx.define_interface_impl("Iterable".into(), vec!("T".into()), ARR_OF!(T_2).to_mut(), vec!(ARR_IT_OF!(T_2.to_mut()), T_2)).unwrap();

    ctx.define_interface_impl("Printable".into(), vec!(), BOOL, vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!(), INT, vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!(), FLOAT, vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!(), STR, vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!("T".into()), Type::TemplateParam(0, vec!(PRINTABLE)).to_ref(), vec!()).unwrap();
    ctx.define_interface_impl("Printable".into(), vec!("T".into()), Type::TemplateParam(0, vec!(PRINTABLE)).to_mut(), vec!()).unwrap();
} 